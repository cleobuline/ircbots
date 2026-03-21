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
import hashlib
import uuid
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

# Modèles valides mis à jour — Mars 2026
# Sources : platform.openai.com/docs/models
VALID_MODELS = [
    # GPT-5.4 (famille actuelle, flagship)
    "gpt-5",
    "gpt-5.4",
    "gpt-5.4-2026-03-05",
    "gpt-5.4-pro",
    "gpt-5.4-pro-2026-03-05",
    "gpt-5.4-mini",
    "gpt-5.4-mini-2026-03-17",
    "gpt-5.4-nano",
    # GPT-5.2 / GPT-5.3 (toujours disponibles)
    "gpt-5.2",
    "gpt-5.2-pro",
    "gpt-5.3-chat-latest",
    # GPT-4o (toujours supportés, utiles pour audio)
    "gpt-4o",
    "gpt-4o-mini",
    "gpt-4o-2024-08-06",
    # GPT-4.5
    "gpt-4.5-preview",
    # Séries o1 / o3
    "o1-mini",
    "o1-preview",
    "o1",
    "o3-mini",
]

_BLOCKED_IP_PREFIXES = (
    "127.", "10.", "0.",
    "169.254.",
    "192.168.",
    "172.16.", "172.17.", "172.18.", "172.19.",
    "172.20.", "172.21.", "172.22.", "172.23.",
    "172.24.", "172.25.", "172.26.", "172.27.",
    "172.28.", "172.29.", "172.30.", "172.31.",
    "::1", "fc", "fd",
)

# Sémaphore global pour limiter les threads lourds simultanés
_HEAVY_SEMAPHORE = threading.Semaphore(4)


def sanitize_title(title: str) -> str:
    return re.sub(r'[^\w\-]', '_', title)[:64]


def sanitize_nick(nick: str) -> str:
    """Nettoie un nick IRC et ajoute un hash pour éviter les collisions (ex: Zozo[1] et Zozo{1})."""
    safe_base = re.sub(r'[^\w\-]', '_', nick)[:24]
    nick_hash = hashlib.md5(nick.encode('utf-8')).hexdigest()[:4]
    return f"{safe_base}_{nick_hash}"


def validate_public_url(url: str) -> str:
    if not url.startswith(("http://", "https://")):
        url = "https://" + url

    parsed = urlparse(url)
    if parsed.scheme not in ("http", "https"):
        raise ValueError(f"Schéma non autorisé : {parsed.scheme}")

    hostname = parsed.hostname or ""
    if not hostname:
        raise ValueError("URL sans hôte.")

    if hostname in ("localhost", "::1") or hostname.endswith(".local"):
        raise ValueError(f"Hôte local non autorisé : {hostname}")

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
        self.admin_user = config.get("admin_user", "")
        self.grok_api_key = config["grok_api_key"]

        # Chemins configurables (avec fallbacks)
        self.web_url_base = config.get("display_url", "https://labynet.fr/images")
        self.image_local_dir = config.get("image_local_dir", "/var/www/html/images")
        self.sora_local_dir = config.get("sora_local_dir", "/var/www/html/sora")
        self.sora_public_url = config.get("sora_public_url", "https://labynet.fr/sora")

        self._blocked_users_lock = threading.Lock()
        self.blocked_users = set()

        self.RATE_LIMIT_SECONDS = 5
        self._user_last_call = {}
        self._rate_limit_lock = threading.Lock()

        self.user_contexts = {}
        self._context_lock = threading.Lock()

        # FIX : verrou pour protéger self.model contre les race conditions entre threads
        self._model_lock = threading.Lock()
        self._model = "gpt-4o-mini"

        self.tag = ""

        self.openai_client = openai.OpenAI(api_key=self.api_key)
        self.imgbb_client = imgbbpy.SyncClient(self.imgbb_api_key)

        os.makedirs("conversations", exist_ok=True)
        os.makedirs(self.image_local_dir, exist_ok=True)
        os.makedirs(self.sora_local_dir, exist_ok=True)

        irc.bot.SingleServerIRCBot.__init__(self, [(self.server, self.port)], self.nickname, self.nickname)

    # FIX : accès thread-safe au modèle via property
    @property
    def model(self):
        with self._model_lock:
            return self._model

    @model.setter
    def model(self, value):
        with self._model_lock:
            self._model = value

    def on_welcome(self, connection, event):
        for channel in self.channel_list:
            connection.join(channel)

    def on_pubmsg(self, connection, event):
        message = event.arguments[0]
        user = event.source.nick
        channel = event.target
        bot_nickname = self.connection.get_nickname()

        with self._blocked_users_lock:
            if user in self.blocked_users:
                return

        # Ignore les messages qui ne s'adressent pas au bot (évite de
        # comptabiliser les bavardages normaux du chan dans le rate-limit).
        if not message.strip().startswith(bot_nickname + ":"):
            return

        if user != self.admin_user:
            now = time.time()
            with self._rate_limit_lock:
                last = self._user_last_call.get(user, 0)
                if now - last < self.RATE_LIMIT_SECONDS:
                    connection.privmsg(channel, f"{user}: doucement, attends {self.RATE_LIMIT_SECONDS}s entre chaque commande.")
                    return
                self._user_last_call[user] = now
                # FIX : nettoyage périodique du dictionnaire rate-limit (garde les 500 derniers)
                if len(self._user_last_call) > 500:
                    cutoff = now - 3600
                    self._user_last_call = {
                        u: t for u, t in self._user_last_call.items() if t > cutoff
                    }

        if message.strip().startswith(bot_nickname + ":"):
            message = message[len(bot_nickname) + 1:].strip()
            command, args = self.parse_command(message)

            if command == "tag":
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
                if user == self.admin_user:
                    self.change_model(channel, args)
                else:
                    connection.privmsg(channel, "Commande réservée à l'admin.")

            elif command == "list-models":
                self.list_models(channel)

            elif command == "current":
                connection.privmsg(channel, f"Modèle actuel utilisé : {self.model}")

            elif command == "grok":
                if user == self.admin_user:
                    if args:
                        self.update_context(channel, user, args)
                        threading.Thread(
                            target=self._thread_generate_response_grok,
                            args=(connection, channel, user),
                            daemon=True
                        ).start()
                    else:
                        connection.privmsg(channel, "Prompt vide pour grok.")
                else:
                    connection.privmsg(channel, "Commande réservée à l'admin.")

            elif command == "local":
                # FIX : validation args vide
                if not args.strip():
                    connection.privmsg(channel, "Prompt vide ! Exemple : local un chat sur la lune")
                    return
                threading.Thread(
                    target=self._thread_generate_image_local,
                    args=(connection, channel, args),
                    daemon=True
                ).start()

            elif command == "image":
                # FIX : validation args vide
                if not args.strip():
                    connection.privmsg(channel, "Prompt vide ! Exemple : image un coucher de soleil sur la mer")
                    return
                threading.Thread(
                    target=self._thread_generate_image_tiny,
                    args=(connection, channel, args),
                    daemon=True
                ).start()

            elif command == "imgbb":
                # FIX : validation args vide
                if not args.strip():
                    connection.privmsg(channel, "Prompt vide ! Exemple : imgbb un robot dansant")
                    return
                threading.Thread(
                    target=self._thread_generate_image_imgbb,
                    args=(connection, channel, args),
                    daemon=True
                ).start()

            elif command == "vision":
                # FIX : validation args vide
                if not args.strip():
                    connection.privmsg(channel, "URL d'image manquante ! Exemple : vision https://example.com/image.jpg")
                    return
                threading.Thread(
                    target=self._thread_generate_image_description,
                    args=(connection, channel, user, args),
                    daemon=True
                ).start()

            elif command == "video":
                self.generate_video_sora(connection, channel, args)

            elif command == "url":
                # FIX : validation args vide
                if not args.strip():
                    connection.privmsg(channel, "URL manquante ! Exemple : url https://example.com")
                    return
                threading.Thread(
                    target=self._thread_summarize_url,
                    args=(connection, channel, args),
                    daemon=True
                ).start()

            else:
                self.update_context(channel, user, message)
                threading.Thread(
                    target=self._thread_generate_response,
                    args=(connection, channel, user),
                    daemon=True
                ).start()

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
            "'url [URL]' pour résumer une page web, 'grok [prompt]' pour utiliser Grok (admin)."
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
                    # FIX : utilisation de safe_privmsg pour protéger contre les erreurs de connexion
                    self.safe_privmsg(connection, target, line)
                    total_chunks_sent += 1
                    line = ''
                else:
                    cut = MAX_BYTES
                    while cut > 0 and cut < len(encoded) and (encoded[cut] & 0xC0) == 0x80:
                        cut -= 1

                    chunk_str = encoded[:cut].decode('utf-8')
                    last_space = chunk_str.rfind(' ')

                    if last_space > 0:
                        self.safe_privmsg(connection, target, chunk_str[:last_space].strip())
                        line = chunk_str[last_space:].strip() + line[len(chunk_str):]
                    else:
                        self.safe_privmsg(connection, target, chunk_str.strip())
                        line = line[len(chunk_str):]

                    total_chunks_sent += 1

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
            # FIX : gestion d'erreur sur l'accès fichier
            try:
                with open(filename, "w") as f:
                    json.dump(messages, f)
                self.connection.privmsg(channel, f"Contexte de {user} sauvegardé sous '{title}'.")
            except OSError as e:
                logger.error(f"Erreur sauvegarde contexte : {e}")
                self.connection.privmsg(channel, f"Erreur lors de la sauvegarde : {e}")
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
            # FIX : gestion d'erreur sur l'accès fichier
            try:
                with open(filename, "r") as f:
                    context = json.load(f)
                if context and isinstance(context[0], str):
                    context = [{"role": "user", "content": msg.rstrip(".")} for msg in context]
                with self._context_lock:
                    self.user_contexts[(channel, user)] = context
                self.connection.privmsg(channel, f"Contexte '{title}' chargé pour {user}.")
            except (OSError, json.JSONDecodeError) as e:
                logger.error(f"Erreur chargement contexte : {e}")
                self.connection.privmsg(channel, f"Erreur lors du chargement : {e}")
        else:
            self.connection.privmsg(channel, f"Aucun fichier de contexte trouvé pour {user} avec le titre '{title}'.")

    def list_user_files(self, channel, user):
        safe_user = sanitize_nick(user)
        prefix = f"{safe_user}."
        suffix = ".context.json"
        # FIX : gestion d'erreur sur os.listdir
        try:
            files = sorted(
                fname[len(prefix):-len(suffix)]
                for fname in os.listdir("conversations")
                if fname.startswith(prefix) and fname.endswith(suffix)
            )
        except OSError as e:
            logger.error(f"Erreur listage fichiers : {e}")
            self.connection.privmsg(channel, f"Erreur lors du listage des fichiers : {e}")
            return
        if files:
            self.connection.privmsg(channel, f"Fichiers de {user} : {', '.join(files)}")
        else:
            self.connection.privmsg(channel, f"Aucun fichier de contexte disponible pour {user}.")

    def delete_user_context(self, channel, user, title):
        title = sanitize_title(title)
        safe_user = sanitize_nick(user)
        # FIX : validation du titre vide dans delete (manquait dans l'original)
        if not title:
            self.connection.privmsg(channel, "Titre invalide.")
            return
        filename = os.path.join("conversations", f"{safe_user}.{title}.context.json")
        if os.path.exists(filename):
            # FIX : gestion d'erreur sur la suppression
            try:
                os.remove(filename)
                self.connection.privmsg(channel, f"Fichier '{title}' supprimé pour {user}.")
            except OSError as e:
                logger.error(f"Erreur suppression fichier : {e}")
                self.connection.privmsg(channel, f"Erreur lors de la suppression : {e}")
        else:
            self.connection.privmsg(channel, f"Aucun fichier trouvé pour {user} avec le titre '{title}'.")

    # ------------------------------------------------------------------ #
    #  Blocage utilisateurs & Modèles                                    #
    # ------------------------------------------------------------------ #

    def block_user(self, user):
        with self._blocked_users_lock:
            self.blocked_users.add(user)

    def unblock_user(self, user):
        with self._blocked_users_lock:
            self.blocked_users.discard(user)

    def change_model(self, channel, model):
        if model in VALID_MODELS:
            self.model = model  # utilise le setter thread-safe
            self.connection.privmsg(channel, f"Modèle changé à {model}.")
        else:
            self.connection.privmsg(channel, f"Modèle '{model}' invalide. Modèles valides : {', '.join(VALID_MODELS)}.")

    def list_models(self, channel):
        self.connection.privmsg(channel, f"Modèles valides : {', '.join(VALID_MODELS)}.")

    # ------------------------------------------------------------------ #
    #  Génération de texte (Threaded)                                    #
    # ------------------------------------------------------------------ #

    @staticmethod
    def _completion_tokens_kwarg(model: str, n: int) -> dict:
        """Retourne le bon paramètre de limite selon la famille du modèle.
        gpt-5.x, o1, o3 → max_completion_tokens ; autres → max_tokens.
        """
        if any(model.startswith(p) for p in ("gpt-5", "o1", "o3")):
            return {"max_completion_tokens": n}
        return {"max_tokens": n}

    def _thread_generate_response(self, connection, channel, user):
        key = (channel, user)
        with self._context_lock:
            messages = list(self.user_contexts.get(key, []))
        if not messages:
            return

        current_model = self.model  # lecture thread-safe via property

        MAX_IRC_LINES = 200  # au-delà on tronque pour éviter le flood

        try:
            response = self.openai_client.chat.completions.create(
                model=current_model,
                **self._completion_tokens_kwarg(current_model, 1500),
                messages=[{"role": "system", "content": self.build_system_prompt()}] + messages
            )
            generated_text = response.choices[0].message.content.strip()
            readable_text = LatexNodes2Text().latex_to_text(generated_text)
            self.update_context(channel, user, generated_text, role="assistant")

            # Tronquer les réponses trop longues pour ne pas flooder le chan
            lines = [l for l in readable_text.splitlines() if l.strip()]
            if len(lines) > MAX_IRC_LINES:
                truncated = "\n".join(lines[:MAX_IRC_LINES])
                truncated += f"\n[… réponse tronquée à {MAX_IRC_LINES} lignes — demande une version plus courte ou précise ta question]"
                self.send_message_in_chunks(connection, channel, truncated)
            else:
                self.send_message_in_chunks(connection, channel, readable_text)
        except openai.OpenAIError as e:
            logger.exception("OpenAI error")
            self.safe_privmsg(connection, channel, f"[Erreur OpenAI: {e}]")
        except Exception as e:
            logger.exception("Unexpected error in text generation")
            self.safe_privmsg(connection, channel, f"[Erreur inattendue: {e}]")

    def _thread_generate_response_grok(self, connection, channel, user):
        key = (channel, user)
        with self._context_lock:
            messages = list(self.user_contexts.get(key, []))
        if not messages:
            return

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
            self.send_message_in_chunks(connection, channel, readable_text)
        except requests.exceptions.RequestException as e:
            logger.exception("Grok HTTP error")
            self.safe_privmsg(connection, channel, f"[Erreur Grok (HTTP): {e}]")
        except Exception as e:
            logger.exception("Unexpected Grok error")
            self.safe_privmsg(connection, channel, f"[Erreur Grok inattendue: {e}]")

    # ------------------------------------------------------------------ #
    #  Vision (Threaded)                                                 #
    # ------------------------------------------------------------------ #

    def _thread_generate_image_description(self, connection, channel, user, image_url):
        try:
            image_url = validate_public_url(image_url)
        except ValueError as e:
            self.safe_privmsg(connection, channel, f"URL invalide ou non autorisée : {e}")
            return

        try:
            response = self.openai_client.chat.completions.create(
                model="gpt-4o",
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
            self.update_context(channel, user, f"[vision: {image_url}]", role="user")
            self.update_context(channel, user, description, role="assistant")
            self.send_message_in_chunks(connection, channel, f"Description : {description}")
        except Exception as e:
            logger.exception("Vision error")
            self.safe_privmsg(connection, channel, f"Erreur vision: {e}")

    # ------------------------------------------------------------------ #
    #  Résumé URL (Threaded)                                             #
    # ------------------------------------------------------------------ #

    def _thread_summarize_url(self, connection, channel, url):
        try:
            url = validate_public_url(url)
        except ValueError as e:
            self.safe_privmsg(connection, channel, f"URL invalide ou non autorisée : {e}")
            return

        try:
            MAX_CONTENT_BYTES = 512 * 1024
            resp = requests.get(url, timeout=10, headers={"User-Agent": "Mozilla/5.0"}, stream=True)
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
        except Exception as e:
            logger.exception("URL summarize error")
            self.safe_privmsg(connection, channel, f"Erreur résumé : {e}")

    # ------------------------------------------------------------------ #
    #  Génération d'images (Threaded)                                    #
    # ------------------------------------------------------------------ #

    def _thread_generate_image_tiny(self, connection, channel, prompt):
        # FIX : sémaphore pour limiter les threads lourds simultanés
        with _HEAVY_SEMAPHORE:
            try:
                response = self.openai_client.images.generate(model="dall-e-3", prompt=prompt, n=1, size="1024x1024")
                image_url = response.data[0].url
                shortener = pyshorteners.Shortener()
                short_url = shortener.tinyurl.short(image_url)
                self.safe_privmsg(connection, channel, short_url)
            except Exception as e:
                logger.exception("Image Tiny error")
                self.safe_privmsg(connection, channel, f"Erreur génération image: {e}")

    def _thread_generate_image_imgbb(self, connection, channel, prompt):
        fd, temp_path = tempfile.mkstemp(suffix=".png")
        os.close(fd)
        # FIX : sémaphore pour limiter les threads lourds simultanés
        with _HEAVY_SEMAPHORE:
            try:
                response = self.openai_client.images.generate(model="dall-e-3", prompt=prompt, n=1, size="1024x1024")
                image_url = response.data[0].url
                image_data = requests.get(image_url, timeout=20).content

                with open(temp_path, 'wb') as f:
                    f.write(image_data)

                imgbb_response = self.imgbb_client.upload(file=temp_path)
                self.safe_privmsg(connection, channel, imgbb_response.url)
            except Exception as e:
                logger.exception("ImgBB upload error")
                self.safe_privmsg(connection, channel, f"Erreur ImgBB: {e}")
            finally:
                if os.path.exists(temp_path):
                    try:
                        os.remove(temp_path)
                    except OSError as e:
                        logger.warning(f"Impossible de supprimer {temp_path}: {e}")

    def _thread_generate_image_local(self, connection, channel, prompt):
        # FIX : sémaphore pour limiter les threads lourds simultanés
        with _HEAVY_SEMAPHORE:
            try:
                response = self.openai_client.images.generate(model="dall-e-3", prompt=prompt, n=1, size="1024x1024")
                image_url = response.data[0].url
                image_response = requests.get(image_url, timeout=20)

                unique_name = f"img_{uuid.uuid4().hex[:8]}.png"
                local_path = os.path.join(self.image_local_dir, unique_name)

                with open(local_path, "wb") as f:
                    f.write(image_response.content)

                final_url = f"{self.web_url_base.rstrip('/')}/{unique_name}"
                self.safe_privmsg(connection, channel, final_url)
            except Exception as e:
                logger.exception("Image local error")
                self.safe_privmsg(connection, channel, f"Erreur génération locale: {e}")

    # ------------------------------------------------------------------ #
    #  Vidéo Sora (Threaded)                                             #
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
        connection.privmsg(channel, "Sora en cours de génération… je reviens avec la vidéo dès qu'elle est prête (30-120s)")

    def _generate_video_worker(self, connection, channel, prompt):
        with _HEAVY_SEMAPHORE:
            try:
                # Création du job via le client OpenAI (plus propre que requests manuels)
                video = self.openai_client.videos.create(
                    model="sora-2",
                    prompt=prompt,
                    seconds=12,
                )
                self.safe_privmsg(connection, channel, f"Sora → job {video.id[-10:]} en file d'attente")

                # Polling jusqu'à completion — timeout + anti-spam + stagnation
                SORA_TIMEOUT_SECONDS = 600   # 10 min max
                SORA_STALL_SECONDS   = 120   # abandon si progress figé 2 min
                SORA_POLL_INTERVAL   = 15
                deadline             = time.time() + SORA_TIMEOUT_SECONDS
                last_progress        = None
                last_progress_change = time.time()

                while video.status in ("queued", "in_progress"):
                    now = time.time()
                    if now > deadline:
                        self.safe_privmsg(connection, channel, f"✖ Sora timeout : job {video.id[-10:]} abandonné après {SORA_TIMEOUT_SECONDS}s.")
                        return
                    time.sleep(SORA_POLL_INTERVAL)
                    video    = self.openai_client.videos.retrieve(video.id)
                    progress = getattr(video, "progress", None)
                    if progress != last_progress:
                        last_progress        = progress
                        last_progress_change = time.time()
                        if progress is not None:
                            self.safe_privmsg(connection, channel, f"Sora progression : {progress}%")
                    elif time.time() - last_progress_change > SORA_STALL_SECONDS:
                        self.safe_privmsg(connection, channel, f"✖ Sora bloqué à {last_progress}% depuis {SORA_STALL_SECONDS}s — job abandonné.")
                        return

                if video.status == "failed":
                    error_msg = getattr(getattr(video, "error", None), "message", "Erreur inconnue")
                    self.safe_privmsg(connection, channel, f"✖ Échec Sora : {error_msg}")
                    return

                if video.status != "completed":
                    self.safe_privmsg(connection, channel, f"✖ Sora statut inattendu : {video.status!r} — job abandonné.")
                    return

                # Téléchargement du MP4 via le client OpenAI
                self.safe_privmsg(connection, channel, "Vidéo générée ! Téléchargement du MP4…")
                content = self.openai_client.videos.download_content(video.id, variant="video")

                filename = f"{video.id}.mp4"
                local_path = os.path.join(self.sora_local_dir, filename)
                content.write_to_file(local_path)

                public_url = f"{self.sora_public_url.rstrip('/')}/{filename}"
                try:
                    shortener = pyshorteners.Shortener()
                    public_url = shortener.tinyurl.short(public_url)
                except Exception:
                    pass

                self.safe_privmsg(connection, channel, f"✔ Vidéo Sora prête ! → {public_url}")

            except openai.OpenAIError as e:
                logger.exception("Sora OpenAI error")
                self.safe_privmsg(connection, channel, f"✖ Erreur Sora : {e}")
            except Exception as e:
                logger.exception("Sora fatal error")
                self.safe_privmsg(connection, channel, f"✖ Erreur fatale Sora : {e}")


if __name__ == "__main__":
    config_file = sys.argv[1] if len(sys.argv) > 1 else "zozo.json"
    bot = ChatGPTBot(config_file)
    bot.start()
