import irc.bot
import openai
import json
import logging
import os
import re
import sys
import socket
import tempfile
import requests
import pyshorteners
import imgbbpy
import time
import threading
from urllib.parse import urlparse
from bs4 import BeautifulSoup
from pylatexenc.latex2text import LatexNodes2Text

logging.basicConfig(
    level=logging.INFO,
    format="%(asctime)s [%(levelname)s] %(message)s",
    handlers=[logging.StreamHandler()]
)
logger = logging.getLogger(__name__)

IRC_SYSTEM_PROMPT = (
    "Tu es un assistant IA sur un serveur IRC. "
    "Réponds de manière concise et sans formatage Markdown "
    "(pas de **, pas de #, pas de listes à tirets) "
    "car le texte s'affiche en clair sur IRC."
)

# Modèles OpenAI valides
VALID_MODELS = [
    "gpt-3.5-turbo", "gpt-4",
    "o1-mini", "o1-preview", "o1", "o3-mini",
    "gpt-4o", "gpt-4.5-preview", "gpt-4o-2024-08-06",
    "gpt-4o-mini",
]

# Plages IP privées/réservées à bloquer pour la protection SSRF
_BLOCKED_IP_PREFIXES = (
    "127.", "10.", "0.",
    "169.254.",          # link-local
    "192.168.",
    "172.16.", "172.17.", "172.18.", "172.19.",
    "172.20.", "172.21.", "172.22.", "172.23.",
    "172.24.", "172.25.", "172.26.", "172.27.",
    "172.28.", "172.29.", "172.30.", "172.31.",
    "::1", "fc", "fd",   # IPv6 loopback / ULA
)


def sanitize_title(title: str) -> str:
    """Nettoie le titre pour n'autoriser que les caractères sûrs (path traversal)."""
    return re.sub(r'[^\w\-]', '_', title)[:64]


def sanitize_nick(nick: str) -> str:
    """Nettoie un nick IRC pour usage dans les noms de fichiers."""
    return re.sub(r'[^\w\-]', '_', nick)[:32]


def validate_public_url(url: str) -> str:
    """
    Valide et normalise une URL.
    - Ajoute le schéma si absent.
    - Refuse les adresses privées/locales (protection SSRF).
    - Résout le DNS et vérifie l'IP résolue (protection contre le DNS rebinding).
    Retourne l'URL normalisée ou lève ValueError.
    """
    if not url.startswith(("http://", "https://")):
        url = "https://" + url

    parsed = urlparse(url)
    if parsed.scheme not in ("http", "https"):
        raise ValueError(f"Schéma non autorisé : {parsed.scheme}")

    hostname = parsed.hostname or ""
    if not hostname:
        raise ValueError("URL sans hôte.")

    # Refus des noms d'hôtes locaux évidents
    if hostname in ("localhost", "::1") or hostname.endswith(".local"):
        raise ValueError(f"Hôte local non autorisé : {hostname}")

    # Résolution DNS et vérification de l'IP résolue (anti DNS rebinding)
    try:
        results = socket.getaddrinfo(hostname, None)
        if not results:
            raise ValueError(f"Impossible de résoudre l'hôte : {hostname}")
        for result in results:
            resolved_ip = result[4][0]
            for prefix in _BLOCKED_IP_PREFIXES:
                if resolved_ip.startswith(prefix):
                    raise ValueError(f"IP résolue privée/réservée non autorisée : {resolved_ip}")
    except socket.gaierror as e:
        raise ValueError(f"Impossible de résoudre l'hôte '{hostname}' : {e}")

    return url


class ChatGPTBot(irc.bot.SingleServerIRCBot):
    def __init__(self, config_file):
        with open(config_file, "r") as f:
            config = json.load(f)

        self.channel_list = [channel.strip() for channel in config["channels"].split(",")]
        self.nickname = config["nickname"]
        self.server = config["server"]
        self.port = config["port"]
        self.api_key = config["api_key"]
        self.max_num_line = config["max_num_line"]
        self.imgbb_api_key = config["imgbb_api_key"]
        self.web_url = config["display_url"]
        self.image_filename = config["image_filename"]
        self.admin_user = config.get("admin_user", "")
        self.grok_api_key = config["grok_api_key"]

        # FIX: verrou pour protéger blocked_users des accès concurrents
        self._blocked_users_lock = threading.Lock()
        self.blocked_users = set()

        # Rate limiting : délai minimum entre deux appels API par utilisateur (en secondes)
        self.RATE_LIMIT_SECONDS = 5
        self._user_last_call = {}          # nick -> timestamp dernier appel
        self._rate_limit_lock = threading.Lock()
        self.user_contexts = {}  # Dict (channel, user) -> liste de {"role": ..., "content": ...}
        # Verrou pour protéger user_contexts des accès concurrents (thread Sora)
        self._context_lock = threading.Lock()
        self.model = "gpt-4o-mini"
        self.tag = ""

        # Client OpenAI v1+
        self.openai_client = openai.OpenAI(api_key=self.api_key)
        self.imgbb_client = imgbbpy.SyncClient(self.imgbb_api_key)

        os.makedirs("conversations", exist_ok=True)

        irc.bot.SingleServerIRCBot.__init__(self, [(self.server, self.port)], self.nickname, self.nickname)

    def on_welcome(self, connection, event):
        for channel in self.channel_list:
            connection.join(channel)

    def on_pubmsg(self, connection, event):
        message = event.arguments[0]
        user = event.source.nick
        channel = event.target
        bot_nickname = self.connection.get_nickname()

        # Vérification blocage en tête — s'applique à toutes les commandes
        with self._blocked_users_lock:
            if user in self.blocked_users:
                return

        # Rate limiting — l'admin est exempté
        if user != self.admin_user:
            now = time.time()
            with self._rate_limit_lock:
                last = self._user_last_call.get(user, 0)
                if now - last < self.RATE_LIMIT_SECONDS:
                    connection.privmsg(channel, f"{user}: doucement, attends {self.RATE_LIMIT_SECONDS}s entre chaque commande.")
                    return
                self._user_last_call[user] = now

        # Commande grok réservée à l'admin pour éviter l'abus
        if message.startswith("grok") and user == self.admin_user:
            prompt = message.split("grok", 1)[1].strip()
            if not prompt:
                return
            self.update_context(channel, user, prompt)
            response = self.generate_response_grok(channel, user)
            if response and response.strip():
                self.send_message_in_chunks(connection, channel, response)
            return

        # Appel direct au bot, style "Zozo: ..."
        if message.strip().startswith(bot_nickname + ":"):
            message = message[len(bot_nickname) + 1:].strip()
            command, args = self.parse_command(message)

            if command == "tag":
                # tag limité à l'admin (comportement global du bot)
                if user == self.admin_user:
                    self.tag = args
                    connection.privmsg(channel, f"Tag mis à jour: {self.tag}")
                else:
                    connection.privmsg(channel, "Commande réservée à l'admin.")

            elif command == "help":
                self.send_help_message(connection, channel)

            elif command == "raz":
                self.reset_user_context(channel, user)
                connection.privmsg(channel, "Conversation oubliée ...")

            elif command == "save":
                self.save_user_context(channel, user, args)

            elif command == "load":
                self.load_user_context(channel, user, args)

            elif command == "files":
                self.list_user_files(channel, user)

            elif command == "delete":
                self.delete_user_context(channel, user, args)

            elif command == "block" and user == self.admin_user:
                self.block_user(args)
                connection.privmsg(channel, f"Utilisateur {args} bloqué.")

            elif command == "unblock" and user == self.admin_user:
                self.unblock_user(args)
                connection.privmsg(channel, f"Utilisateur {args} débloqué.")

            elif command == "model":
                # FIX: changement de modèle réservé à l'admin (coût API)
                if user == self.admin_user:
                    self.change_model(channel, args)
                else:
                    connection.privmsg(channel, "Commande réservée à l'admin.")

            elif command == "list-models":
                self.list_models(channel)

            elif command == "current":
                connection.privmsg(channel, f"Modèle actuel utilisé : {self.model}")

            elif command == "local":
                self.generate_image_local(connection, channel, args)

            elif command == "image":
                self.generate_image_tiny(connection, channel, args)

            elif command == "imgbb":
                self.generate_image_imgbb(connection, channel, args)

            elif command == "vision":
                self.generate_image_description(connection, channel, user, args)

            elif command == "video":
                self.generate_video_sora(connection, channel, args)

            elif command == "url":
                self.summarize_url(connection, channel, args)

            else:
                self.update_context(channel, user, message)
                response = self.generate_response(channel, user)
                if response and response.strip():
                    self.send_message_in_chunks(connection, channel, response)

    # ------------------------------------------------------------------ #
    #  Utilitaires                                                         #
    # ------------------------------------------------------------------ #

    def parse_command(self, message):
        parts = message.strip().split(" ", 1)
        command = parts[0].lower()
        args = parts[1] if len(parts) > 1 else ""
        return command, args

    def build_system_prompt(self):
        prompt = IRC_SYSTEM_PROMPT
        if self.tag:
            prompt += f" {self.tag}"
        return prompt

    def send_help_message(self, connection, channel):
        help_message = (
            "'raz' oublie la conversation, 'save [titre]', 'load [titre]', "
            "'delete [titre]', 'files' liste les conversations, 'block [user]' "
            "bloque un utilisateur, 'unblock [user]' débloque un utilisateur, "
            "'model [model_name]' pour changer le modèle (admin), 'list-models' liste les modèles valides, "
            "'image [prompt]' pour générer une image, 'vision [image URL]' pour décrire une image, "
            "'url [URL]' pour résumer une page web."
        )
        connection.privmsg(channel, help_message)

    def safe_privmsg(self, connection, target, message):
        try:
            connection.privmsg(target, message)
        except Exception as e:
            logger.warning(f"safe_privmsg failed → {e}")

    def send_message_in_chunks(self, connection, target, message):
        if not message:
            return

        MAX_BYTES = 392
        lines = message.split('\n')
        total_chunks_sent = 0

        for line in lines:
            line = line.strip()
            if not line:
                continue

            while line:
                encoded = line.encode('utf-8')
                if len(encoded) <= MAX_BYTES:
                    connection.privmsg(target, line)
                    total_chunks_sent += 1
                    line = ''
                else:
                    # Trouver la coupure sûre en bytes sans couper un caractère UTF-8
                    cut = MAX_BYTES
                    while cut > 0 and (encoded[cut] & 0xC0) == 0x80:
                        cut -= 1

                    chunk_str = encoded[:cut].decode('utf-8')

                    # Essayer de couper sur un espace
                    last_space = chunk_str.rfind(' ')
                    if last_space > 0:
                        connection.privmsg(target, chunk_str[:last_space].strip())
                        line = chunk_str[last_space:].strip() + line[len(chunk_str):]
                    else:
                        connection.privmsg(target, chunk_str.strip())
                        line = line[len(chunk_str):]

                    total_chunks_sent += 1

                # FIX: sleep après chaque envoi sauf le premier message unique (anti-flood)
                if total_chunks_sent > 1 or len(lines) > 1:
                    time.sleep(0.5)

    # ------------------------------------------------------------------ #
    #  Contexte                                                            #
    # ------------------------------------------------------------------ #

    def update_context(self, channel, user, message, role="user"):
        key = (channel, user)
        with self._context_lock:
            if key not in self.user_contexts:
                self.user_contexts[key] = []

            self.user_contexts[key].append({"role": role, "content": message})

            messages = self.user_contexts[key]
            if len(messages) > self.max_num_line:
                messages = messages[-self.max_num_line:]
                while messages and messages[0]["role"] != "user":
                    messages = messages[1:]
            self.user_contexts[key] = messages

    def reset_user_context(self, channel, user):
        with self._context_lock:
            self.user_contexts[(channel, user)] = []

    def save_user_context(self, channel, user, title):
        title = sanitize_title(title)
        safe_user = sanitize_nick(user)
        if not title:
            self.connection.privmsg(channel, "Titre invalide.")
            return
        key = (channel, user)
        with self._context_lock:
            messages = self.user_contexts.get(key)
        if messages:
            filename = os.path.join("conversations", f"{safe_user}.{title}.context.json")
            with open(filename, "w") as f:
                json.dump(messages, f)
            self.connection.privmsg(channel, f"Contexte de {user} sauvegardé sous '{title}'.")
        else:
            self.connection.privmsg(channel, f"Aucun contexte à sauvegarder pour {user}.")

    def load_user_context(self, channel, user, title):
        title = sanitize_title(title)
        safe_user = sanitize_nick(user)
        if not title:
            self.connection.privmsg(channel, "Titre invalide.")
            return
        filename = os.path.join("conversations", f"{safe_user}.{title}.context.json")
        if os.path.exists(filename):
            with open(filename, "r") as f:
                context = json.load(f)
            # Migration ancien format (liste de strings) → nouveau format (liste de dicts)
            if context and isinstance(context[0], str):
                context = [{"role": "user", "content": msg.rstrip(".")} for msg in context]
            with self._context_lock:
                self.user_contexts[(channel, user)] = context
            self.connection.privmsg(channel, f"Contexte '{title}' chargé pour {user}.")
        else:
            self.connection.privmsg(channel, f"Aucun fichier de contexte trouvé pour {user} avec le titre '{title}'.")

    def list_user_files(self, channel, user):
        safe_user = sanitize_nick(user)
        prefix = f"{safe_user}."
        suffix = ".context.json"
        files = sorted(
            fname[len(prefix):-len(suffix)]
            for fname in os.listdir("conversations")
            if fname.startswith(prefix) and fname.endswith(suffix)
        )
        if files:
            self.connection.privmsg(channel, f"Fichiers de {user} : {', '.join(files)}")
        else:
            self.connection.privmsg(channel, f"Aucun fichier de contexte disponible pour {user}.")

    def delete_user_context(self, channel, user, title):
        title = sanitize_title(title)
        safe_user = sanitize_nick(user)
        if not title:
            self.connection.privmsg(channel, "Titre invalide.")
            return
        filename = os.path.join("conversations", f"{safe_user}.{title}.context.json")
        if os.path.exists(filename):
            os.remove(filename)
            self.connection.privmsg(channel, f"Fichier '{title}' supprimé pour {user}.")
        else:
            self.connection.privmsg(channel, f"Aucun fichier trouvé pour {user} avec le titre '{title}'.")

    # ------------------------------------------------------------------ #
    #  Blocage utilisateurs                                                #
    # ------------------------------------------------------------------ #

    def block_user(self, user):
        with self._blocked_users_lock:
            self.blocked_users.add(user)

    def unblock_user(self, user):
        with self._blocked_users_lock:
            self.blocked_users.discard(user)

    # ------------------------------------------------------------------ #
    #  Modèles                                                             #
    # ------------------------------------------------------------------ #

    def change_model(self, channel, model):
        if model in VALID_MODELS:
            self.model = model
            self.connection.privmsg(channel, f"Modèle changé à {model}.")
        else:
            self.connection.privmsg(channel, f"Modèle '{model}' invalide. Modèles valides : {', '.join(VALID_MODELS)}.")

    def list_models(self, channel):
        self.connection.privmsg(channel, f"Modèles valides : {', '.join(VALID_MODELS)}.")

    # ------------------------------------------------------------------ #
    #  Génération de texte (OpenAI v1+)                                   #
    # ------------------------------------------------------------------ #

    def generate_response(self, channel, user):
        key = (channel, user)
        with self._context_lock:
            messages = list(self.user_contexts.get(key, []))
        if not messages:
            return ""

        try:
            response = self.openai_client.chat.completions.create(
                model=self.model,
                # FIX: max_tokens pour éviter des réponses longues et coûteuses
                max_tokens=500,
                messages=[{"role": "system", "content": self.build_system_prompt()}] + messages
            )
            generated_text = response.choices[0].message.content.strip()
            readable_text = LatexNodes2Text().latex_to_text(generated_text)
            self.update_context(channel, user, generated_text, role="assistant")
            return readable_text
        except openai.OpenAIError as e:
            logger.error(f"OpenAI error: {e}")
            return f"[Erreur OpenAI: {e}]"
        except Exception as e:
            logger.error(f"Unexpected error in generate_response: {e}")
            return f"[Erreur inattendue: {e}]"

    def generate_response_grok(self, channel, user):
        key = (channel, user)
        with self._context_lock:
            messages = list(self.user_contexts.get(key, []))
        if not messages:
            return ""

        headers = {'Authorization': f'Bearer {self.grok_api_key}'}
        payload = {
            "model": "grok-3",
            "messages": [{"role": "system", "content": self.build_system_prompt()}] + messages,
            "max_tokens": 500,
            "temperature": 0.7
        }

        try:
            response = requests.post(
                'https://api.x.ai/v1/chat/completions',
                json=payload,
                headers=headers,
                timeout=60
            )
            response.raise_for_status()
            resp_data = response.json()
            if 'choices' in resp_data and resp_data['choices']:
                generated_text = resp_data['choices'][0]['message']['content']
            else:
                generated_text = "[Grok] Erreur ou réponse non disponible."
            readable_text = LatexNodes2Text().latex_to_text(generated_text)
            self.update_context(channel, user, generated_text, role="assistant")
            return readable_text
        except requests.exceptions.RequestException as e:
            logger.error(f"Grok HTTP error: {e}")
            return f"[Erreur Grok (HTTP): {e}]"
        except Exception as e:
            logger.error(f"Unexpected Grok error: {e}")
            return f"[Erreur Grok inattendue: {e}]"

    # ------------------------------------------------------------------ #
    #  Vision (OpenAI v1+) — description intégrée au contexte             #
    # ------------------------------------------------------------------ #

    def generate_image_description(self, connection, channel, user, image_url):
        # Validation anti-SSRF de l'URL image avant d'appeler OpenAI
        try:
            image_url = validate_public_url(image_url)
        except ValueError as e:
            connection.privmsg(channel, f"URL invalide ou non autorisée : {e}")
            return

        try:
            response = self.openai_client.chat.completions.create(
                model="gpt-4o",   # modèle vision (fixé intentionnellement)
                messages=[
                    {
                        "role": "user",
                        "content": [
                            {"type": "text", "text": "Décris cette image en détails, en français."},
                            {"type": "image_url", "image_url": {"url": image_url}},
                        ],
                    }
                ],
                max_tokens=500,
            )
            description = response.choices[0].message.content or "[Impossible de décrire l'image]"
            # Intégration dans le contexte pour la suite de la conversation
            self.update_context(channel, user, f"[vision: {image_url}]", role="user")
            self.update_context(channel, user, description, role="assistant")
            self.send_message_in_chunks(connection, channel, f"Description : {description}")
        except openai.OpenAIError as e:
            logger.error(f"Vision OpenAI error: {e}")
            connection.privmsg(channel, f"Erreur OpenAI vision: {e}")
        except Exception as e:
            logger.error(f"Unexpected vision error: {e}")
            connection.privmsg(channel, f"Erreur inattendue vision: {e}")

    # ------------------------------------------------------------------ #
    #  Résumé URL (BeautifulSoup + OpenAI v1+)                            #
    # ------------------------------------------------------------------ #

    def summarize_url(self, connection, channel, url):
        # Validation anti-SSRF avant toute requête HTTP
        try:
            url = validate_public_url(url)
        except ValueError as e:
            connection.privmsg(channel, f"URL invalide ou non autorisée : {e}")
            return

        try:
            # Limitation de la taille de la réponse HTTP (anti-bombes mémoire)
            MAX_CONTENT_BYTES = 512 * 1024  # 512 Ko
            resp = requests.get(
                url, timeout=10,
                headers={"User-Agent": "Mozilla/5.0"},
                stream=True
            )
            resp.raise_for_status()

            chunks = []
            total = 0
            for chunk in resp.iter_content(chunk_size=8192):
                chunks.append(chunk)
                total += len(chunk)
                if total >= MAX_CONTENT_BYTES:
                    break

            raw_content = b"".join(chunks)
            resp_text = raw_content.decode("utf-8", errors="replace")

            # Extraction du texte brut (pas de HTML)
            soup = BeautifulSoup(resp_text, "html.parser")
            text = soup.get_text(separator=" ", strip=True)[:4000]

            prompt = f"Voici le contenu d'une page web :\n{text}\n\nRésume en quelques phrases claires et concises."
            ai_response = self.openai_client.chat.completions.create(
                model=self.model,
                max_tokens=500,
                messages=[{"role": "user", "content": prompt}]
            )
            summary = ai_response.choices[0].message.content.strip()
            self.send_message_in_chunks(connection, channel, f"Résumé : {summary}")
        except requests.exceptions.RequestException as e:
            connection.privmsg(channel, f"Erreur récupération URL : {e}")
        except openai.OpenAIError as e:
            connection.privmsg(channel, f"Erreur OpenAI résumé : {e}")
        except Exception as e:
            logger.error(f"summarize_url unexpected error: {e}")
            connection.privmsg(channel, f"Erreur inattendue : {e}")

    # ------------------------------------------------------------------ #
    #  Génération d'images (OpenAI v1+)                                   #
    # ------------------------------------------------------------------ #

    def generate_image_tiny(self, connection, channel, prompt):
        try:
            response = self.openai_client.images.generate(
                model="dall-e-3",
                prompt=prompt,
                n=1,
                size="1024x1024"
            )
            image_url = response.data[0].url
            shortener = pyshorteners.Shortener()
            short_url = shortener.tinyurl.short(image_url)
            connection.privmsg(channel, short_url)
        except openai.OpenAIError as e:
            connection.privmsg(channel, f"Erreur génération image: {e}")
        except Exception as e:
            connection.privmsg(channel, f"Erreur inattendue: {e}")

    def generate_image_imgbb(self, connection, channel, prompt):
        # FIX: fichier temporaire unique via tempfile.mkstemp (évite les race conditions)
        fd, temp_path = tempfile.mkstemp(suffix=".png")
        os.close(fd)
        try:
            response = self.openai_client.images.generate(
                model="dall-e-3",
                prompt=prompt,
                n=1,
                size="1024x1024"
            )
            image_url = response.data[0].url
            image_data = requests.get(image_url, timeout=20).content

            with open(temp_path, 'wb') as f:
                f.write(image_data)

            imgbb_response = self.imgbb_client.upload(file=temp_path)
            connection.privmsg(channel, imgbb_response.url)
        except openai.OpenAIError as e:
            connection.privmsg(channel, f"Erreur génération image: {e}")
        except requests.exceptions.RequestException as e:
            connection.privmsg(channel, f"Erreur téléchargement image: {e}")
        except imgbbpy.exceptions.ImgBBError as e:
            connection.privmsg(channel, f"Erreur upload ImgBB: {e}")
        except Exception as e:
            connection.privmsg(channel, f"Erreur inattendue: {e}")
        finally:
            # Suppression garantie du fichier temporaire même en cas d'erreur
            if os.path.exists(temp_path):
                try:
                    os.remove(temp_path)
                except OSError as e:
                    logger.warning(f"Impossible de supprimer {temp_path}: {e}")

    def generate_image_local(self, connection, channel, prompt):
        try:
            response = self.openai_client.images.generate(
                model="dall-e-3",
                prompt=prompt,
                n=1,
                size="1024x1024"
            )
            image_url = response.data[0].url
            image_response = requests.get(image_url, timeout=20)

            with open(self.image_filename, "wb") as f:
                f.write(image_response.content)

            connection.privmsg(channel, self.web_url)
        except openai.OpenAIError as e:
            connection.privmsg(channel, f"Erreur génération image: {e}")
        except requests.exceptions.RequestException as e:
            connection.privmsg(channel, f"Erreur téléchargement image: {e}")
        except Exception as e:
            connection.privmsg(channel, f"Erreur inattendue: {e}")

    # ------------------------------------------------------------------ #
    #  Vidéo Sora (thread)                                                 #
    # ------------------------------------------------------------------ #

    def generate_video_sora(self, connection, channel, prompt):
        if not prompt.strip():
            connection.privmsg(channel, "Prompt vide ! Exemple : video un chien qui court après une balle dans un parc")
            return

        thread = threading.Thread(
            target=self._generate_video_worker,
            args=(connection, channel, prompt.strip()),
            daemon=True
        )
        thread.start()
        connection.privmsg(channel, "Sora-2 en cours de génération… je reviens avec la vidéo dès qu'elle est prête (30-120s)")

    def _generate_video_worker(self, connection, channel, prompt):
        headers = {"Authorization": f"Bearer {self.api_key}"}
        files = {
            "model": (None, "sora-2-2025-12-08"),
            "prompt": (None, prompt),
            "seconds": (None, "12"),
        }

        try:
            r = requests.post("https://api.openai.com/v1/videos", headers=headers, files=files, timeout=60)
            r.raise_for_status()
            video_id = r.json()["id"]
            self.safe_privmsg(connection, channel, f"Sora-2 → job {video_id[-10:]} en file d'attente")

            start_time = time.time()
            completed = False

            while time.time() - start_time < 900:  # 15 min max
                time.sleep(7)

                try:
                    resp = requests.get(
                        f"https://api.openai.com/v1/videos/{video_id}",
                        headers=headers,
                        timeout=60,
                    )
                    resp.raise_for_status()
                    v = resp.json()
                except Exception as e:
                    logger.warning(f"Sora polling error (retrying): {e}")
                    continue

                status = v.get("status", "").lower()

                if status == "failed":
                    error_msg = (v.get("error") or {}).get("message", "Erreur inconnue")
                    self.safe_privmsg(connection, channel, f"✖ Échec Sora-2 : {error_msg}")
                    return

                if status == "completed":
                    if not completed:
                        completed = True
                        self.safe_privmsg(connection, channel, "Vidéo générée ! Téléchargement du MP4…")

                    try:
                        resp_video = requests.get(
                            f"https://api.openai.com/v1/videos/{video_id}/content",
                            headers=headers,
                            params={"variant": "video"},
                            timeout=300,
                            stream=True,
                        )
                        resp_video.raise_for_status()
                    except Exception as e:
                        self.safe_privmsg(connection, channel, f"✖ Erreur download Sora-2 : {e}")
                        return

                    local_dir = "/var/www/html/sora"
                    os.makedirs(local_dir, exist_ok=True)
                    filename = f"{video_id}.mp4"
                    local_path = os.path.join(local_dir, filename)

                    with open(local_path, "wb") as f:
                        for chunk in resp_video.iter_content(chunk_size=8192):
                            if chunk:
                                f.write(chunk)

                    public_url = f"https://labynet.fr/sora/{filename}"
                    try:
                        shortener = pyshorteners.Shortener()
                        public_url = shortener.tinyurl.short(public_url)
                    except Exception:
                        pass

                    self.safe_privmsg(connection, channel, f"✔ Vidéo Sora-2 prête ! → {public_url}")
                    return

            self.safe_privmsg(connection, channel, "⏰ Timeout 15 min – Sora-2 trop lent ou bloqué")

        except Exception as e:
            logger.error(f"Sora fatal error: {e}")
            self.safe_privmsg(connection, channel, f"✖ Erreur fatale Sora-2 : {e}")


if __name__ == "__main__":
    config_file = sys.argv[1] if len(sys.argv) > 1 else "zozo.json"
    bot = ChatGPTBot(config_file)
    bot.start()
