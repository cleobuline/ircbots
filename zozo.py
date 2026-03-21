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

# Modèles valides — Mars 2026
VALID_MODELS = [
    "gpt-5", "gpt-5.4", "gpt-5.4-2026-03-05", "gpt-5.4-pro", "gpt-5.4-pro-2026-03-05",
    "gpt-5.4-mini", "gpt-5.4-mini-2026-03-17", "gpt-5.4-nano",
    "gpt-5.2", "gpt-5.2-pro", "gpt-5.3-chat-latest",
    "gpt-4o", "gpt-4o-mini", "gpt-4o-2024-08-06",
    "gpt-4.5-preview",
    "o1-mini", "o1-preview", "o1", "o3-mini",
]

_BLOCKED_IP_PREFIXES = (
    "127.", "10.", "0.", "169.254.", "192.168.",
    "172.16.", "172.17.", "172.18.", "172.19.", "172.20.", "172.21.",
    "172.22.", "172.23.", "172.24.", "172.25.", "172.26.", "172.27.",
    "172.28.", "172.29.", "172.30.", "172.31.",
    "::1", "fc", "fd",
)

_HEAVY_SEMAPHORE = threading.Semaphore(4)


def sanitize_title(title: str) -> str:
    return re.sub(r'[^\w\-]', '_', title)[:64]


def sanitize_nick(nick: str) -> str:
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
    if not hostname or hostname in ("localhost", "::1") or hostname.endswith(".local"):
        raise ValueError(f"Hôte local non autorisé : {hostname}")

    try:
        for result in socket.getaddrinfo(hostname, None):
            ip = result[4][0]
            for prefix in _BLOCKED_IP_PREFIXES:
                if ip.startswith(prefix):
                    raise ValueError(f"IP privée non autorisée : {ip}")
    except socket.gaierror as e:
        raise ValueError(f"Impossible de résoudre '{hostname}' : {e}")

    return url


class ChatGPTBot(irc.bot.SingleServerIRCBot):
    def __init__(self, config_file):
        with open(config_file, "r", encoding="utf-8") as f:
            config = json.load(f)

        self.channel_list = [ch.strip() for ch in config["channels"].split(",")]
        self.nickname = config["nickname"]
        self.server = config["server"]
        self.port = config["port"]
        self.max_num_line = config.get("max_num_line", 20)
        self.admin_user = config.get("admin_user", "")

        # Clés API depuis le fichier config
        self.api_key = config["api_key"]
        self.imgbb_api_key = config["imgbb_api_key"]

        # Chemins configurables
        self.web_url_base = config.get("display_url", "https://labynet.fr/images")
        self.image_local_dir = config.get("image_local_dir", "/var/www/html/images")
        self.sora_local_dir = config.get("sora_local_dir", "/var/www/html/sora")
        self.sora_public_url = config.get("sora_public_url", "https://labynet.fr/sora")

        # Locks
        self._blocked_users_lock = threading.Lock()
        self.blocked_users = set()

        self.RATE_LIMIT_SECONDS = 5
        self._user_last_call = {}
        self._rate_limit_lock = threading.Lock()

        self.user_contexts = {}
        self._context_lock = threading.Lock()

        self._model_lock = threading.Lock()
        self._model = "gpt-4o-mini"

        self.tag = ""

        self.openai_client = openai.OpenAI(api_key=self.api_key)
        self.imgbb_client = imgbbpy.SyncClient(self.imgbb_api_key)

        # Création et vérification des dossiers
        for directory in ["conversations", self.image_local_dir, self.sora_local_dir]:
            os.makedirs(directory, exist_ok=True)
            if not os.access(directory, os.W_OK):
                logger.error(f"ERREUR : Le dossier {directory} n'est pas accessible en écriture !")

        irc.bot.SingleServerIRCBot.__init__(self, [(self.server, self.port)], self.nickname, self.nickname)
        logger.info(f"Bot Zozo démarré avec config : {config_file} | Modèle par défaut : {self._model}")

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
            logger.info(f"Rejoint le canal {channel}")

    def on_kick(self, connection, event):
        kicked_nick = event.arguments[0]
        channel = event.target
        kicker = event.source.nick
        if kicked_nick == self.nickname:
            logger.warning(f"Kické de {channel} par {kicker}. Reconnexion dans 5s...")
            time.sleep(5)
            try:
                connection.join(channel)
                logger.info(f"Réintégré {channel} après kick.")
            except Exception as e:
                logger.error(f"Erreur en tentant de rejoindre {channel} : {e}")

    def on_disconnect(self, connection, event):
        logger.warning("Déconnecté du serveur IRC. Reconnexion dans 15s...")
        time.sleep(15)
        try:
            self.jump_server()
        except Exception as e:
            logger.error(f"Erreur reconnexion : {e}")

    def on_pubmsg(self, connection, event):
        message = event.arguments[0].strip()
        user = event.source.nick
        channel = event.target
        bot_nick = self.connection.get_nickname()

        with self._blocked_users_lock:
            if user in self.blocked_users:
                return

        if not message.startswith(bot_nick + ":"):
            return

        # Rate-limit ignoré pour l'admin
        if user != self.admin_user:
            now = time.time()
            with self._rate_limit_lock:
                if now - self._user_last_call.get(user, 0) < self.RATE_LIMIT_SECONDS:
                    connection.privmsg(channel, f"{user}: attends {self.RATE_LIMIT_SECONDS}s entre les commandes.")
                    return
                self._user_last_call[user] = now
                # Nettoyage périodique
                if len(self._user_last_call) > 500:
                    cutoff = now - 3600
                    self._user_last_call = {u: t for u, t in self._user_last_call.items() if t > cutoff}

        message = message[len(bot_nick) + 1:].strip()
        command, args = self.parse_command(message)

        if command == "tag" and user == self.admin_user:
            self.tag = args.strip()
            connection.privmsg(channel, f"Tag mis à jour : {self.tag}")

        elif command == "help":
            self.send_help_message(connection, channel)

        elif command == "raz":
            self.reset_user_context(channel, user)
            connection.privmsg(channel, "Conversation oubliée.")

        elif command == "save":
            self.save_user_context(channel, user, args)

        elif command == "load":
            self.load_user_context(channel, user, args)

        elif command == "files":
            self.list_user_files(channel, user)

        elif command == "delete":
            self.delete_user_context(channel, user, args)

        elif command == "block" and user == self.admin_user:
            self.block_user(args.strip())
            connection.privmsg(channel, f"{args.strip()} bloqué.")

        elif command == "unblock" and user == self.admin_user:
            self.unblock_user(args.strip())
            connection.privmsg(channel, f"{args.strip()} débloqué.")

        elif command == "model" and user == self.admin_user:
            self.change_model(channel, args.strip())

        elif command == "list-models":
            self.list_models(channel)

        elif command == "current":
            connection.privmsg(channel, f"Modèle actuel : {self.model}")

        elif command == "local":
            if not args.strip():
                connection.privmsg(channel, "Prompt vide ! Ex: local un chat sur la lune")
                return
            threading.Thread(target=self._thread_generate_image_local, args=(connection, channel, args), daemon=True).start()

        elif command == "image":
            if not args.strip():
                connection.privmsg(channel, "Prompt vide ! Ex: image un coucher de soleil")
                return
            threading.Thread(target=self._thread_generate_image_tiny, args=(connection, channel, args), daemon=True).start()

        elif command == "imgbb":
            if not args.strip():
                connection.privmsg(channel, "Prompt vide ! Ex: imgbb un robot dansant")
                return
            threading.Thread(target=self._thread_generate_image_imgbb, args=(connection, channel, args), daemon=True).start()

        elif command == "vision":
            if not args.strip():
                connection.privmsg(channel, "URL manquante ! Ex: vision https://example.com/image.jpg")
                return
            threading.Thread(target=self._thread_generate_image_description, args=(connection, channel, user, args), daemon=True).start()

        elif command == "video":
            self.generate_video_sora(connection, channel, args)

        elif command == "url":
            if not args.strip():
                connection.privmsg(channel, "URL manquante ! Ex: url https://example.com")
                return
            threading.Thread(target=self._thread_summarize_url, args=(connection, channel, args), daemon=True).start()

        else:
            self.update_context(channel, user, message)
            threading.Thread(target=self._thread_generate_response, args=(connection, channel, user), daemon=True).start()

    # ====================== Utilitaires ======================
    def parse_command(self, message):
        parts = message.split(" ", 1)
        return parts[0].lower(), (parts[1] if len(parts) > 1 else "")

    def build_system_prompt(self):
        prompt = IRC_SYSTEM_PROMPT
        if self.tag:
            prompt += f" Préfixe obligatoire : commence chaque ligne de ta réponse par '{self.tag} '."
        return prompt

    def send_help_message(self, connection, channel):
        help_text = (
            "Commandes : raz | save [titre] | load [titre] | delete [titre] | files | "
            "block/unblock [user] (admin) | model [nom] (admin) | list-models | current | "
            "image [prompt] | local [prompt] | imgbb [prompt] | vision [url] | url [url] | video [prompt]"
        )
        connection.privmsg(channel, help_text)

    def safe_privmsg(self, connection, target, message):
        try:
            connection.privmsg(target, str(message)[:400])
        except Exception:
            pass

    def send_message_in_chunks(self, connection, target, message):
        if not message:
            return
        MAX_BYTES = 392
        for line in message.split('\n'):
            line = line.strip()
            if not line:
                continue
            while line:
                encoded = line.encode('utf-8')
                if len(encoded) <= MAX_BYTES:
                    self.safe_privmsg(connection, target, line)
                    break
                cut = MAX_BYTES
                while cut > 0 and (encoded[cut] & 0xC0) == 0x80:
                    cut -= 1
                chunk = encoded[:cut].decode('utf-8', errors='replace')
                last_space = chunk.rfind(' ')
                if last_space > 5:
                    self.safe_privmsg(connection, target, chunk[:last_space].strip())
                    line = chunk[last_space:].strip() + line[len(chunk):]
                else:
                    self.safe_privmsg(connection, target, chunk.strip())
                    line = line[len(chunk):]
                time.sleep(0.35)

    # ====================== Contexte ======================
    def update_context(self, channel, user, message, role="user"):
        key = (channel, user)
        with self._context_lock:
            if key not in self.user_contexts:
                self.user_contexts[key] = []
            self.user_contexts[key].append({"role": role, "content": message})
            if len(self.user_contexts[key]) > self.max_num_line:
                msgs = self.user_contexts[key][-self.max_num_line:]
                while msgs and msgs[0]["role"] != "user":
                    msgs.pop(0)
                self.user_contexts[key] = msgs

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
        if not messages:
            self.connection.privmsg(channel, "Aucun contexte à sauvegarder.")
            return
        filename = os.path.join("conversations", f"{safe_user}.{title}.context.json")
        try:
            with open(filename, "w", encoding="utf-8") as f:
                json.dump(messages, f, ensure_ascii=False, indent=2)
            self.connection.privmsg(channel, f"Contexte sauvegardé sous '{title}'.")
        except Exception as e:
            logger.error(f"Erreur sauvegarde contexte : {e}")
            self.connection.privmsg(channel, "Erreur lors de la sauvegarde.")

    def load_user_context(self, channel, user, title):
        title = sanitize_title(title)
        safe_user = sanitize_nick(user)
        if not title:
            self.connection.privmsg(channel, "Titre invalide.")
            return
        filename = os.path.join("conversations", f"{safe_user}.{title}.context.json")
        if os.path.exists(filename):
            try:
                with open(filename, "r", encoding="utf-8") as f:
                    context = json.load(f)
                # Compatibilité ancien format (liste de strings)
                if context and isinstance(context[0], str):
                    context = [{"role": "user", "content": msg.rstrip(".")} for msg in context]
                with self._context_lock:
                    self.user_contexts[(channel, user)] = context
                self.connection.privmsg(channel, f"Contexte '{title}' chargé.")
            except Exception as e:
                logger.error(f"Erreur chargement contexte : {e}")
                self.connection.privmsg(channel, "Erreur lors du chargement du contexte.")
        else:
            self.connection.privmsg(channel, f"Aucun fichier '{title}' trouvé.")

    def list_user_files(self, channel, user):
        safe_user = sanitize_nick(user)
        prefix = f"{safe_user}."
        suffix = ".context.json"
        try:
            files = [f[len(prefix):-len(suffix)] for f in os.listdir("conversations")
                     if f.startswith(prefix) and f.endswith(suffix)]
            if files:
                self.connection.privmsg(channel, f"Fichiers de {user} : {', '.join(sorted(files))}")
            else:
                self.connection.privmsg(channel, f"Aucun fichier pour {user}.")
        except Exception as e:
            logger.error(f"Erreur listage fichiers : {e}")
            self.connection.privmsg(channel, "Erreur lors du listage des fichiers.")

    def delete_user_context(self, channel, user, title):
        title = sanitize_title(title)
        safe_user = sanitize_nick(user)
        if not title:
            self.connection.privmsg(channel, "Titre invalide.")
            return
        filename = os.path.join("conversations", f"{safe_user}.{title}.context.json")
        if os.path.exists(filename):
            try:
                os.remove(filename)
                self.connection.privmsg(channel, f"Fichier '{title}' supprimé.")
            except Exception as e:
                logger.error(f"Erreur suppression : {e}")
                self.connection.privmsg(channel, "Erreur lors de la suppression.")
        else:
            self.connection.privmsg(channel, f"Fichier '{title}' non trouvé.")

    # ====================== Modèles & Blocage ======================
    def change_model(self, channel, model_name):
        if model_name in VALID_MODELS:
            self.model = model_name
            self.connection.privmsg(channel, f"Modèle changé → {model_name}")
        else:
            self.connection.privmsg(channel, f"Modèle invalide. Utilise : list-models")

    def list_models(self, channel):
        self.connection.privmsg(channel, f"Modèles valides : {', '.join(VALID_MODELS)}")

    def block_user(self, user):
        with self._blocked_users_lock:
            self.blocked_users.add(user)

    def unblock_user(self, user):
        with self._blocked_users_lock:
            self.blocked_users.discard(user)

    # ====================== Génération Texte ======================
    def _thread_generate_response(self, connection, channel, user):
        key = (channel, user)
        with self._context_lock:
            messages = list(self.user_contexts.get(key, []))
        if not messages:
            return

        try:
            response = self.openai_client.chat.completions.create(
                model=self.model,
                max_completion_tokens=1500 if any(self.model.startswith(x) for x in ("gpt-5", "o1", "o3")) else 1500,
                messages=[{"role": "system", "content": self.build_system_prompt()}] + messages,
                temperature=0.85,
            )
            text = response.choices[0].message.content.strip()
            readable = LatexNodes2Text().latex_to_text(text)
            self.update_context(channel, user, text, "assistant")
            self.send_message_in_chunks(connection, channel, readable)
        except Exception as e:
            logger.exception("Erreur génération réponse")
            self.safe_privmsg(connection, channel, f"[Erreur : {str(e)[:150]}]")

    # ====================== Vision ======================
    def _thread_generate_image_description(self, connection, channel, user, image_url):
        try:
            image_url = validate_public_url(image_url)
            response = self.openai_client.chat.completions.create(
                model="gpt-4o",
                messages=[{
                    "role": "user",
                    "content": [
                        {"type": "text", "text": "Décris cette image en détails, en français."},
                        {"type": "image_url", "image_url": {"url": image_url}}
                    ]
                }],
                max_tokens=600,
            )
            desc = response.choices[0].message.content or "Impossible de décrire l'image."
            self.update_context(channel, user, f"[vision: {image_url}]", "user")
            self.update_context(channel, user, desc, "assistant")
            self.send_message_in_chunks(connection, channel, f"Description : {desc}")
        except Exception as e:
            logger.exception("Vision error")
            self.safe_privmsg(connection, channel, f"Erreur vision : {e}")

    # ====================== Résumé URL ======================
    def _thread_summarize_url(self, connection, channel, url):
        try:
            url = validate_public_url(url)
            resp = requests.get(url, timeout=12, headers={"User-Agent": "Mozilla/5.0"}, stream=True)
            resp.raise_for_status()

            content = b""
            for chunk in resp.iter_content(8192):
                content += chunk
                if len(content) > 512 * 1024:
                    break

            soup = BeautifulSoup(content.decode("utf-8", errors="replace"), "html.parser")
            text = soup.get_text(separator=" ", strip=True)[:4000]

            summary_resp = self.openai_client.chat.completions.create(
                model="gpt-4o-mini",
                max_tokens=500,
                messages=[{"role": "user", "content": f"Résume clairement en français cette page web :\n{text}"}]
            )
            summary = summary_resp.choices[0].message.content.strip()
            self.send_message_in_chunks(connection, channel, f"Résumé : {summary}")
        except Exception as e:
            logger.exception("URL summarize error")
            self.safe_privmsg(connection, channel, f"Erreur résumé URL : {e}")

    # ====================== Images ======================
    def _thread_generate_image_tiny(self, connection, channel, prompt):
        with _HEAVY_SEMAPHORE:
            try:
                resp = self.openai_client.images.generate(model="dall-e-3", prompt=prompt, n=1, size="1024x1024")
                short_url = pyshorteners.Shortener().tinyurl.short(resp.data[0].url)
                self.safe_privmsg(connection, channel, short_url)
            except Exception as e:
                logger.exception("Image tiny error")
                self.safe_privmsg(connection, channel, f"Erreur génération image : {e}")

    def _thread_generate_image_imgbb(self, connection, channel, prompt):
        fd, temp_path = tempfile.mkstemp(suffix=".png")
        os.close(fd)
        with _HEAVY_SEMAPHORE:
            try:
                resp = self.openai_client.images.generate(model="dall-e-3", prompt=prompt, n=1, size="1024x1024")
                data = requests.get(resp.data[0].url, timeout=20).content
                with open(temp_path, "wb") as f:
                    f.write(data)
                uploaded = self.imgbb_client.upload(file=temp_path)
                self.safe_privmsg(connection, channel, uploaded.url)
            except Exception as e:
                logger.exception("ImgBB error")
                self.safe_privmsg(connection, channel, f"Erreur ImgBB : {e}")
            finally:
                if os.path.exists(temp_path):
                    try:
                        os.remove(temp_path)
                    except OSError:
                        pass

    def _thread_generate_image_local(self, connection, channel, prompt):
        with _HEAVY_SEMAPHORE:
            try:
                resp = self.openai_client.images.generate(model="dall-e-3", prompt=prompt, n=1, size="1024x1024")
                data = requests.get(resp.data[0].url, timeout=20).content
                name = f"img_{uuid.uuid4().hex[:10]}.png"
                path = os.path.join(self.image_local_dir, name)
                with open(path, "wb") as f:
                    f.write(data)
                url = f"{self.web_url_base.rstrip('/')}/{name}"
                self.safe_privmsg(connection, channel, url)
            except Exception as e:
                logger.exception("Image local error")
                self.safe_privmsg(connection, channel, f"Erreur image locale : {e}")

    # ====================== Sora ======================
    def generate_video_sora(self, connection, channel, prompt):
        if not prompt.strip():
            connection.privmsg(channel, "Prompt vide ! Ex: video un chien qui court après une balle")
            return
        threading.Thread(target=self._generate_video_worker, args=(connection, channel, prompt.strip()), daemon=True).start()

    def _generate_video_worker(self, connection, channel, prompt):
        with _HEAVY_SEMAPHORE:
            try:
                video = self.openai_client.videos.create(
                    model="sora-2",
                    prompt=prompt,
                    seconds=12,
                )
                self.safe_privmsg(connection, channel, f"Sora → job {video.id[-10:]} lancé...")

                deadline = time.time() + 600
                last_progress = None
                last_change = time.time()

                STALL_TIMEOUT = 120  # abandon si progress figé depuis 2 min

                while video.status in ("queued", "in_progress"):
                    if time.time() > deadline:
                        self.safe_privmsg(connection, channel, "Sora timeout (10 min), job abandonné.")
                        return
                    time.sleep(15)
                    video = self.openai_client.videos.retrieve(video.id)

                    progress = getattr(video, "progress", None)
                    if progress != last_progress:
                        last_progress = progress
                        last_change = time.time()
                        if progress is not None:
                            self.safe_privmsg(connection, channel, f"Sora progression : {progress}%")
                    elif last_progress is not None and time.time() - last_change > STALL_TIMEOUT:
                        self.safe_privmsg(connection, channel, f"✖ Sora bloqué à {last_progress}% depuis {STALL_TIMEOUT}s — job abandonné.")
                        return

                if video.status == "completed":
                    self.safe_privmsg(connection, channel, "Vidéo terminée, téléchargement...")
                    content = self.openai_client.videos.download_content(video.id, variant="video")
                    filename = f"{video.id}.mp4"
                    local_path = os.path.join(self.sora_local_dir, filename)
                    content.write_to_file(local_path)

                    public_url = f"{self.sora_public_url.rstrip('/')}/{filename}"
                    try:
                        public_url = pyshorteners.Shortener().tinyurl.short(public_url)
                    except Exception:
                        pass
                    self.safe_privmsg(connection, channel, f"✔ Vidéo Sora prête → {public_url}")
                else:
                    self.safe_privmsg(connection, channel, f"Échec Sora : statut {video.status}")
            except Exception as e:
                logger.exception("Sora error")
                self.safe_privmsg(connection, channel, f"Erreur Sora : {e}")


if __name__ == "__main__":
    config_file = sys.argv[1] if len(sys.argv) > 1 else "zozo.json"
    bot = ChatGPTBot(config_file)
    bot.start()
