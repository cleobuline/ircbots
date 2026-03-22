import irc3
import asyncio
import httpx
import openai
import json
import logging
import os
import re
import sys
import socket
import hashlib
import uuid
import tempfile
import time
import base64
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
    "o1-mini", "o1", "o3-mini",
]

TINYURL_API     = "https://tinyurl.com/api-create.php"
IMGBB_API_BASE  = "https://api.imgbb.com/1/upload"

_BLOCKED_IP_PREFIXES = (
    "127.", "10.", "0.", "169.254.", "192.168.",
    "172.16.", "172.17.", "172.18.", "172.19.", "172.20.", "172.21.",
    "172.22.", "172.23.", "172.24.", "172.25.", "172.26.", "172.27.",
    "172.28.", "172.29.", "172.30.", "172.31.",
    "::1", "fc", "fd",
)

# Semaphore pour limiter les tâches lourdes en parallèle (images, vidéos)
_HEAVY_SEMAPHORE = asyncio.Semaphore(4)


# ====================== Utilitaires ======================

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


def is_heavy_model(model: str) -> bool:
    return any(model.startswith(x) for x in ("o1", "o3", "gpt-5", "gpt-4.5"))


def is_reasoning_model(model: str) -> bool:
    return any(model.startswith(x) for x in ("o1", "o3"))


# ====================== Plugin irc3 ======================

@irc3.plugin
class ZozoPlugin:

    def __init__(self, bot):
        self.bot = bot
        cfg = bot.config

        self.admin_user      = cfg.get("admin_user", "")
        self.max_num_line    = int(cfg.get("max_num_line", 20))
        self.api_key         = cfg["api_key"]
        self.imgbb_api_key   = cfg["imgbb_api_key"]
        self.web_url_base    = cfg.get("display_url", "https://labynet.fr/images")
        self.image_local_dir = cfg.get("image_local_dir", "/var/www/html/images")
        self.sora_local_dir  = cfg.get("sora_local_dir", "/var/www/html/sora")
        self.sora_public_url = cfg.get("sora_public_url", "https://labynet.fr/sora")
        self.rate_limit_secs = int(cfg.get("rate_limit_seconds", 5))

        self._model          = "gpt-4o-mini"
        self._tag            = ""
        self.user_contexts   = {}   # (channel, nick) -> list[dict]
        self.blocked_users   = set()
        self._user_last_call = {}   # nick -> float (timestamp)

        # Client httpx async partagé — une seule connexion TCP réutilisée
        self._http = httpx.AsyncClient(
            timeout=httpx.Timeout(300.0, connect=10.0),
            headers={"User-Agent": "ZozoBot/2.0"},
            follow_redirects=True,
        )

        # Client OpenAI async officiel (utilisé pour Sora)
        self._openai = openai.AsyncOpenAI(api_key=self.api_key)

        # Création des dossiers nécessaires
        for d in ["conversations", self.image_local_dir, self.sora_local_dir]:
            os.makedirs(d, exist_ok=True)
            if not os.access(d, os.W_OK):
                logger.error(f"Dossier non accessible en écriture : {d}")

        logger.info(f"ZozoPlugin initialisé | modèle par défaut : {self._model}")

    # ====================== Helpers IRC ======================

    def privmsg(self, target: str, text: str):
        """Envoie un message en tronquant à 400 caractères."""
        try:
            self.bot.privmsg(target, str(text)[:400])
        except Exception:
            pass

    async def send_chunks(self, target: str, message: str):
        """Découpe le message en morceaux IRC-safe (392 octets max) avec pause."""
        MAX_BYTES = 392
        for line in message.split('\n'):
            line = line.strip()
            if not line:
                continue
            remaining = line.encode('utf-8')
            while remaining:
                if len(remaining) <= MAX_BYTES:
                    self.privmsg(target, remaining.decode('utf-8'))
                    break
                cut = MAX_BYTES
                while cut > 0 and (remaining[cut] & 0xC0) == 0x80:
                    cut -= 1
                chunk_bytes = remaining[:cut]
                last_space = chunk_bytes.rfind(b' ')
                if last_space > 5:
                    to_send  = chunk_bytes[:last_space]
                    remaining = chunk_bytes[last_space:].lstrip(b' ') + remaining[cut:]
                else:
                    to_send   = chunk_bytes
                    remaining = remaining[cut:]
                self.privmsg(target, to_send.decode('utf-8'))
                await asyncio.sleep(0.35)

    def build_system_prompt(self) -> str:
        prompt = IRC_SYSTEM_PROMPT
        if self._tag:
            prompt += f" Préfixe obligatoire : commence chaque ligne par '{self._tag} '."
        return prompt

    # ====================== Contexte ======================

    def update_context(self, channel: str, nick: str, content: str, role: str = "user"):
        key = (channel, nick)
        ctx = self.user_contexts.setdefault(key, [])
        ctx.append({"role": role, "content": content})
        if len(ctx) > self.max_num_line:
            ctx = ctx[-self.max_num_line:]
            while ctx and ctx[0]["role"] != "user":
                ctx.pop(0)
            self.user_contexts[key] = ctx

    def reset_context(self, channel: str, nick: str):
        self.user_contexts[(channel, nick)] = []

    def save_context(self, channel: str, nick: str, title: str):
        title = sanitize_title(title)
        if not title:
            self.privmsg(channel, "Titre invalide.")
            return
        key = (channel, nick)
        messages = self.user_contexts.get(key)
        if not messages:
            self.privmsg(channel, "Aucun contexte à sauvegarder.")
            return
        filename = os.path.join("conversations", f"{sanitize_nick(nick)}.{title}.context.json")
        try:
            with open(filename, "w", encoding="utf-8") as f:
                json.dump(messages, f, ensure_ascii=False, indent=2)
            self.privmsg(channel, f"Contexte sauvegardé sous '{title}'.")
        except Exception as e:
            logger.error(f"Erreur sauvegarde : {e}")
            self.privmsg(channel, "Erreur lors de la sauvegarde.")

    def load_context(self, channel: str, nick: str, title: str):
        title = sanitize_title(title)
        if not title:
            self.privmsg(channel, "Titre invalide.")
            return
        filename = os.path.join("conversations", f"{sanitize_nick(nick)}.{title}.context.json")
        if not os.path.exists(filename):
            self.privmsg(channel, f"Aucun fichier '{title}' trouvé.")
            return
        try:
            with open(filename, "r", encoding="utf-8") as f:
                ctx = json.load(f)
            if ctx and isinstance(ctx[0], str):
                ctx = [{"role": "user", "content": m.rstrip(".")} for m in ctx]
            self.user_contexts[(channel, nick)] = ctx
            self.privmsg(channel, f"Contexte '{title}' chargé.")
        except Exception as e:
            logger.error(f"Erreur chargement : {e}")
            self.privmsg(channel, "Erreur lors du chargement.")

    def list_files(self, channel: str, nick: str):
        safe = sanitize_nick(nick)
        prefix, suffix = f"{safe}.", ".context.json"
        try:
            files = [
                f[len(prefix):-len(suffix)]
                for f in os.listdir("conversations")
                if f.startswith(prefix) and f.endswith(suffix)
            ]
            if files:
                self.privmsg(channel, f"Fichiers de {nick} : {', '.join(sorted(files))}")
            else:
                self.privmsg(channel, f"Aucun fichier pour {nick}.")
        except Exception as e:
            logger.error(f"Erreur listage : {e}")
            self.privmsg(channel, "Erreur lors du listage.")

    def delete_file(self, channel: str, nick: str, title: str):
        title = sanitize_title(title)
        if not title:
            self.privmsg(channel, "Titre invalide.")
            return
        filename = os.path.join("conversations", f"{sanitize_nick(nick)}.{title}.context.json")
        if not os.path.exists(filename):
            self.privmsg(channel, f"Fichier '{title}' non trouvé.")
            return
        try:
            os.remove(filename)
            self.privmsg(channel, f"Fichier '{title}' supprimé.")
        except Exception as e:
            logger.error(f"Erreur suppression : {e}")
            self.privmsg(channel, "Erreur lors de la suppression.")

    # ====================== API OpenAI via httpx ======================

    async def _openai_chat(self, messages: list, model: str) -> str:
        """Appelle l'API chat/completions et retourne le texte de la réponse."""
        heavy  = is_heavy_model(model)
        kwargs = {
            "model": model,
            "max_completion_tokens": 8000 if heavy else 1500,
            "messages": messages,
            "timeout": 300 if heavy else 60,
        }
        if not is_heavy_model(model):
            kwargs["temperature"] = 0.85
        resp = await self._openai.chat.completions.create(**kwargs)
        return (resp.choices[0].message.content or "").strip()

    async def _openai_image(self, prompt: str) -> str:
        """Génère une image DALL-E 3 et retourne l'URL temporaire OpenAI."""
        resp = await self._openai.images.generate(
            model="dall-e-3",
            prompt=prompt,
            n=1,
            size="1024x1024",
            timeout=120,
        )
        return resp.data[0].url

    async def _tinyurl(self, url: str) -> str:
        """Raccourcit une URL via TinyURL."""
        r = await self._http.get(TINYURL_API, params={"url": url}, timeout=10)
        r.raise_for_status()
        return r.text.strip()

    async def _imgbb_upload(self, image_bytes: bytes) -> str:
        """Upload une image sur ImgBB et retourne son URL publique."""
        
        b64 = base64.b64encode(image_bytes).decode()
        r = await self._http.post(
            IMGBB_API_BASE,
            data={"key": self.imgbb_api_key, "image": b64},
            timeout=60,
        )
        r.raise_for_status()
        return r.json()["data"]["url"]

    # ====================== Tâches asynchrones ======================

    async def _task_chat(self, channel: str, nick: str):
        key = (channel, nick)
        messages = list(self.user_contexts.get(key, []))
        if not messages:
            return
        model = self._model
        if is_heavy_model(model):
            self.privmsg(channel, f"{nick}: réflexion en cours avec {model}, patience...")
        try:
            text = await self._openai_chat(
                [{"role": "system", "content": self.build_system_prompt()}] + messages,
                model,
            )
            if not text:
                self.privmsg(channel, "[Pas de réponse du modèle.]")
                return
            readable = LatexNodes2Text().latex_to_text(text)
            self.update_context(channel, nick, text, "assistant")
            await self.send_chunks(channel, readable)
        except Exception as e:
            logger.exception("Erreur chat")
            self.privmsg(channel, f"[Erreur : {str(e)[:150]}]")

    async def _task_vision(self, channel: str, nick: str, image_url: str):
        try:
            image_url = validate_public_url(image_url)
            text = await self._openai_chat(
                [{
                    "role": "user",
                    "content": [
                        {"type": "text", "text": "Décris cette image en détails, en français."},
                        {"type": "image_url", "image_url": {"url": image_url}},
                    ],
                }],
                "gpt-4o",
            )
            desc = text or "Impossible de décrire l'image."
            self.update_context(channel, nick, f"[vision: {image_url}]", "user")
            self.update_context(channel, nick, desc, "assistant")
            await self.send_chunks(channel, f"Description : {desc}")
        except Exception as e:
            logger.exception("Vision error")
            self.privmsg(channel, f"Erreur vision : {e}")

    async def _task_summarize_url(self, channel: str, url: str):
        try:
            url = validate_public_url(url)
            resp = await self._http.get(url, timeout=12, headers={"User-Agent": "Mozilla/5.0"})
            resp.raise_for_status()
            raw = resp.content[:512 * 1024]
            soup = BeautifulSoup(raw.decode("utf-8", errors="replace"), "html.parser")
            page_text = soup.get_text(separator=" ", strip=True)[:4000]
            summary = await self._openai_chat(
                [{"role": "user", "content": f"Résume clairement en français cette page web :\n{page_text}"}],
                "gpt-4o-mini",
            )
            if not summary:
                self.privmsg(channel, "Impossible de résumer cette page.")
                return
            await self.send_chunks(channel, f"Résumé : {summary}")
        except Exception as e:
            logger.exception("URL summarize error")
            self.privmsg(channel, f"Erreur résumé URL : {e}")

    async def _task_image_tiny(self, channel: str, prompt: str):
        async with _HEAVY_SEMAPHORE:
            try:
                openai_url = await self._openai_image(prompt)
                short = await self._tinyurl(openai_url)
                self.privmsg(channel, short)
            except Exception as e:
                logger.exception("Image tiny error")
                self.privmsg(channel, f"Erreur génération image : {e}")

    async def _task_image_imgbb(self, channel: str, prompt: str):
        async with _HEAVY_SEMAPHORE:
            try:
                openai_url = await self._openai_image(prompt)
                img_bytes = (await self._http.get(openai_url, timeout=20)).content
                public_url = await self._imgbb_upload(img_bytes)
                self.privmsg(channel, public_url)
            except Exception as e:
                logger.exception("ImgBB error")
                self.privmsg(channel, f"Erreur ImgBB : {e}")

    async def _task_image_local(self, channel: str, prompt: str):
        async with _HEAVY_SEMAPHORE:
            try:
                openai_url = await self._openai_image(prompt)
                img_bytes = (await self._http.get(openai_url, timeout=20)).content
                name = f"img_{uuid.uuid4().hex[:10]}.png"
                path = os.path.join(self.image_local_dir, name)
                with open(path, "wb") as f:
                    f.write(img_bytes)
                url = f"{self.web_url_base.rstrip('/')}/{name}"
                self.privmsg(channel, url)
            except Exception as e:
                logger.exception("Image local error")
                self.privmsg(channel, f"Erreur image locale : {e}")

    async def _task_sora(self, channel: str, prompt: str):
        async with _HEAVY_SEMAPHORE:
            try:
                # Lancement du job via la lib officielle openai async
                video = await self._openai.videos.create(
                    model="sora-2",
                    prompt=prompt,
                    seconds=4,
                )
                self.privmsg(channel, f"Sora → job {video.id[-10:]} lancé...")

                deadline      = asyncio.get_event_loop().time() + 600
                last_progress = None
                last_change   = asyncio.get_event_loop().time()
                STALL_TIMEOUT = 120

                while video.status in ("queued", "in_progress"):
                    if asyncio.get_event_loop().time() > deadline:
                        self.privmsg(channel, "Sora timeout (10 min), job abandonné.")
                        return
                    await asyncio.sleep(15)
                    video = await self._openai.videos.retrieve(video.id)

                    progress = getattr(video, "progress", None)
                    now = asyncio.get_event_loop().time()
                    if progress != last_progress:
                        last_progress = progress
                        last_change   = now
                        if progress is not None:
                            self.privmsg(channel, f"Sora progression : {progress}%")
                    elif last_progress is not None and now - last_change > STALL_TIMEOUT:
                        self.privmsg(channel, f"✖ Sora bloqué à {last_progress}% depuis {STALL_TIMEOUT}s — abandonné.")
                        return

                if video.status == "completed":
                    self.privmsg(channel, "Vidéo terminée, téléchargement...")
                    filename   = f"{video.id}.mp4"
                    local_path = os.path.join(self.sora_local_dir, filename)
                    raw = await self._openai.videos.download_content(video.id, variant="video")
                    with open(local_path, "wb") as f:
                        f.write(raw.read())
                    public_url = f"{self.sora_public_url.rstrip('/')}/{filename}"
                    try:
                        public_url = await self._tinyurl(public_url)
                    except Exception:
                        pass
                    self.privmsg(channel, f"✔ Vidéo Sora prête → {public_url}")
                else:
                    self.privmsg(channel, f"Échec Sora : statut {video.status}")
            except Exception as e:
                logger.exception("Sora error")
                self.privmsg(channel, f"Erreur Sora : {e}")

    # ====================== Routage des commandes ======================

    @irc3.event(irc3.rfc.PRIVMSG)
    async def on_privmsg(self, mask, target, data, **kwargs):
        # Ignore les messages privés (hors canaux)
        if not target.startswith("#"):
            return

        nick    = mask.nick
        channel = target
        message = data.strip()
        bot_nick = self.bot.nick

        if nick in self.blocked_users:
            return
        if not message.startswith(bot_nick + ":"):
            return

        # Rate-limit (admin exempté)
        if nick != self.admin_user:

            now = time.monotonic()
            last = self._user_last_call.get(nick, 0)
            if now - last < self.rate_limit_secs:
                self.privmsg(channel, f"{nick}: attends {self.rate_limit_secs}s entre les commandes.")
                return
            self._user_last_call[nick] = now
            if len(self._user_last_call) > 500:
                cutoff = now - 3600
                self._user_last_call = {u: t for u, t in self._user_last_call.items() if t > cutoff}

        message = message[len(bot_nick) + 1:].strip()
        parts   = message.split(" ", 1)
        command = parts[0].lower()
        args    = parts[1].strip() if len(parts) > 1 else ""

        # --- Commandes admin ---
        if command == "tag" and nick == self.admin_user:
            self._tag = args
            self.privmsg(channel, f"Tag mis à jour : {self._tag}")

        elif command == "model" and nick == self.admin_user:
            if args in VALID_MODELS:
                self._model = args
                self.privmsg(channel, f"Modèle changé → {self._model}")
            else:
                self.privmsg(channel, "Modèle invalide. Utilise : list-models")

        elif command == "block" and nick == self.admin_user:
            self.blocked_users.add(args)
            self.privmsg(channel, f"{args} bloqué.")

        elif command == "unblock" and nick == self.admin_user:
            self.blocked_users.discard(args)
            self.privmsg(channel, f"{args} débloqué.")

        # --- Commandes générales ---
        elif command == "help":
            self.privmsg(channel,
                "Commandes : raz | save [titre] | load [titre] | delete [titre] | files | "
                "block/unblock [user] (admin) | model [nom] (admin) | list-models | current | "
                "image [prompt] | local [prompt] | imgbb [prompt] | vision [url] | url [url] | video [prompt]"
            )

        elif command == "raz":
            self.reset_context(channel, nick)
            self.privmsg(channel, "Conversation oubliée.")

        elif command == "save":
            self.save_context(channel, nick, args)

        elif command == "load":
            self.load_context(channel, nick, args)

        elif command == "files":
            self.list_files(channel, nick)

        elif command == "delete":
            self.delete_file(channel, nick, args)

        elif command == "list-models":
            self.privmsg(channel, f"Modèles valides : {', '.join(VALID_MODELS)}")

        elif command == "current":
            self.privmsg(channel, f"Modèle actuel : {self._model}")

        elif command == "image":
            if not args:
                self.privmsg(channel, "Prompt vide ! Ex: image un coucher de soleil")
            else:
                asyncio.ensure_future(self._task_image_tiny(channel, args))

        elif command == "local":
            if not args:
                self.privmsg(channel, "Prompt vide ! Ex: local un chat sur la lune")
            else:
                asyncio.ensure_future(self._task_image_local(channel, args))

        elif command == "imgbb":
            if not args:
                self.privmsg(channel, "Prompt vide ! Ex: imgbb un robot dansant")
            else:
                asyncio.ensure_future(self._task_image_imgbb(channel, args))

        elif command == "vision":
            if not args:
                self.privmsg(channel, "URL manquante ! Ex: vision https://example.com/image.jpg")
            else:
                asyncio.ensure_future(self._task_vision(channel, nick, args))

        elif command == "url":
            if not args:
                self.privmsg(channel, "URL manquante ! Ex: url https://example.com")
            else:
                asyncio.ensure_future(self._task_summarize_url(channel, args))

        elif command == "video":
            if not args:
                self.privmsg(channel, "Prompt vide ! Ex: video un chien qui court")
            else:
                asyncio.ensure_future(self._task_sora(channel, args))

        else:
            # Message libre → conversation
            self.update_context(channel, nick, message)
            asyncio.ensure_future(self._task_chat(channel, nick))

    @irc3.event(irc3.rfc.JOIN)
    async def on_join(self, mask, channel, **kwargs):
        if mask.nick == self.bot.nick:
            logger.info(f"Rejoint {channel}")

    @irc3.event(irc3.rfc.KICK)
    async def on_kick(self, mask, channel, target, **kwargs):
        if target == self.bot.nick:
            logger.warning(f"Kické de {channel} par {mask.nick}. Retentative dans 5s...")
            await asyncio.sleep(5)
            self.bot.join(channel)



# ====================== Point d'entrée ======================

def main():
    config_file = sys.argv[1] if len(sys.argv) > 1 else "zozo.json"
    with open(config_file, "r", encoding="utf-8") as f:
        cfg = json.load(f)

    channels = [ch.strip() for ch in cfg["channels"].split(",")]

    bot = irc3.IrcBot.from_config({
        "nick":     cfg["nickname"],
        "host":     cfg["server"],
        "port":     cfg["port"],
        "includes": ["irc3.plugins.core", "irc3.plugins.autojoins"],
        "autojoins": channels,
        # Champs custom transmis au plugin
        "api_key":          cfg["api_key"],
        "imgbb_api_key":    cfg["imgbb_api_key"],
        "admin_user":       cfg.get("admin_user", ""),
        "max_num_line":     cfg.get("max_num_line", 20),
        "rate_limit_seconds": cfg.get("rate_limit_seconds", 5),
        "display_url":      cfg.get("display_url", "https://labynet.fr/images"),
        "image_local_dir":  cfg.get("image_local_dir", "/var/www/html/images"),
        "sora_local_dir":   cfg.get("sora_local_dir", "/var/www/html/sora"),
        "sora_public_url":  cfg.get("sora_public_url", "https://labynet.fr/sora"),
    })
    bot.include(__name__)   # charge ZozoPlugin depuis ce fichier
    bot.run()


if __name__ == "__main__":
    main()
