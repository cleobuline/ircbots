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
import time
import base64
import tiktoken
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

TINYURL_API    = "https://tinyurl.com/api-create.php"
IMGBB_API_BASE = "https://api.imgbb.com/1/upload"

_BLOCKED_IP_PREFIXES = (
    "127.", "10.", "0.", "169.254.", "192.168.",
    "172.16.", "172.17.", "172.18.", "172.19.", "172.20.", "172.21.",
    "172.22.", "172.23.", "172.24.", "172.25.", "172.26.", "172.27.",
    "172.28.", "172.29.", "172.30.", "172.31.",
    "::1", "fc", "fd",
)

# FIX: Semaphore créé dans __init__ pour éviter le problème de boucle asyncio
_CONTEXT_TTL = 86400

# Prix OpenAI Mars 2026
PRICING = {
    "gpt-5.4-pro":  (5.00, 20.00),
    "gpt-5.4":      (2.50, 15.00),
    "gpt-5.4-mini": (0.75, 4.50),
    "gpt-5.4-nano": (0.20, 1.25),
    "gpt-5":        (2.50, 15.00),
    "gpt-4o":       (2.50, 10.00),
    "gpt-4o-mini":  (0.15, 0.60),
    "o1":           (15.00, 60.00),
    "o1-mini":      (3.00, 12.00),
    "o3-mini":      (1.10, 4.40),
}

DALL_E_COST_PER_IMAGE = 0.04
SORA_COST_PER_SECOND  = 0.10


def get_model_price(model: str):
    for key in PRICING:
        if model.startswith(key):
            return PRICING[key]
    return PRICING.get("gpt-4o-mini", (0.15, 0.60))


def sanitize_title(title: str) -> str:
    return re.sub(r'[^\w\-]', '_', title)[:64]


def sanitize_nick(nick: str) -> str:
    safe_base = re.sub(r'[^\w\-]', '_', nick)[:24]
    nick_hash = hashlib.md5(nick.encode('utf-8')).hexdigest()[:4]
    return f"{safe_base}_{nick_hash}"


async def validate_public_url(url: str) -> str:
    if not url.startswith(("http://", "https://")):
        url = "https://" + url
    parsed = urlparse(url)
    if parsed.scheme not in ("http", "https"):
        raise ValueError(f"Schéma non autorisé : {parsed.scheme}")
    hostname = parsed.hostname or ""
    if not hostname or hostname in ("localhost", "::1") or hostname.endswith(".local"):
        raise ValueError(f"Hôte local non autorisé : {hostname}")

    loop = asyncio.get_running_loop()
    try:
        addr_info = await loop.run_in_executor(None, lambda: socket.getaddrinfo(hostname, None))
        for result in addr_info:
            ip = result[4][0]
            for prefix in _BLOCKED_IP_PREFIXES:
                if ip.startswith(prefix):
                    raise ValueError(f"IP privée non autorisée : {ip}")
    except socket.gaierror as e:
        raise ValueError(f"Impossible de résoudre '{hostname}' : {e}")
    return url


# FIX: fusion de is_heavy_model et is_reasoning_model en une seule fonction claire
def is_heavy_model(model: str) -> bool:
    """Modèles coûteux/lents nécessitant un semaphore et un timeout plus long."""
    return model.startswith(("o1", "o3"))


def is_reasoning_model(model: str) -> bool:
    """Modèles qui n'acceptent pas le paramètre temperature."""
    return model.startswith(("o1", "o3"))


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
        # FIX: délai flood configurable
        self.flood_delay     = float(cfg.get("flood_delay", 0.35))

        self._model          = "gpt-4o-mini"
        self._tag            = ""
        self.user_contexts   = {}
        self.user_costs      = {}
        self.blocked_users   = set()
        self._user_last_call = {}
        self._context_last_used = {}

        # FIX: Semaphore créé ici, dans le bon contexte asyncio
        self._heavy_semaphore = asyncio.Semaphore(4)

        self._http = httpx.AsyncClient(
            timeout=httpx.Timeout(300.0, connect=10.0),
            headers={"User-Agent": "ZozoBot/2.0"},
            follow_redirects=True,
        )
        self._openai = openai.AsyncOpenAI(api_key=self.api_key)
        self._tokenizer = tiktoken.encoding_for_model("gpt-4o")

        for d in ["conversations", self.image_local_dir, self.sora_local_dir]:
            os.makedirs(d, exist_ok=True)

        # FIX: ensure_future au lieu de get_event_loop().create_task()
        asyncio.ensure_future(self._cleanup_task())
        logger.info(f"ZozoPlugin initialisé | modèle par défaut : {self._model}")

    async def _cleanup_task(self):
        while True:
            await asyncio.sleep(3600)
            cutoff = time.monotonic() - _CONTEXT_TTL
            stale = [k for k, t in self._context_last_used.items() if t < cutoff]
            for k in stale:
                self.user_contexts.pop(k, None)
                self.user_costs.pop(k, None)
                self._context_last_used.pop(k, None)
            if stale:
                logger.info(f"Purge TTL : {len(stale)} contexte(s) supprimé(s).")

    async def close(self):
        await self._http.aclose()
        logger.info("ZozoPlugin : client HTTP fermé.")

    def privmsg(self, target: str, text: str):
        try:
            self.bot.privmsg(target, str(text)[:400])
        except Exception as e:
            # FIX: log l'erreur au lieu de l'avaler silencieusement
            logger.debug(f"privmsg échoué vers {target} : {e}")

    async def send_chunks(self, target: str, message: str):
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
                    to_send = chunk_bytes[:last_space]
                    remaining = chunk_bytes[last_space:].lstrip(b' ') + remaining[cut:]
                else:
                    to_send = chunk_bytes
                    remaining = remaining[cut:]
                self.privmsg(target, to_send.decode('utf-8'))
                # FIX: délai configurable pour s'adapter aux flood protections IRC
                await asyncio.sleep(self.flood_delay)

    def build_system_prompt(self) -> str:
        prompt = IRC_SYSTEM_PROMPT
        if self._tag:
            prompt += f" Préfixe obligatoire : commence chaque ligne par '{self._tag} '."
        return prompt

    def update_context(self, channel: str, nick: str, content: str, role: str = "user"):
        key = (channel, nick)
        ctx = self.user_contexts.setdefault(key, [])
        ctx.append({"role": role, "content": content})
        self._context_last_used[key] = time.monotonic()

        if len(ctx) > self.max_num_line:
            ctx = ctx[-self.max_num_line:]
            # S'assurer que le contexte commence bien par un message user
            while ctx and ctx[0]["role"] != "user":
                ctx.pop(0)
            if not ctx:
                self.user_contexts.pop(key, None)
                self.user_costs.pop(key, None)
                self._context_last_used.pop(key, None)
            else:
                self.user_contexts[key] = ctx

    def reset_context(self, channel: str, nick: str):
        key = (channel, nick)
        self.user_contexts[key] = []
        self.user_costs[key] = 0.0
        self._context_last_used.pop(key, None)

    def add_cost(self, channel: str, nick: str | None, cost: float):
        # FIX: si nick est None, on loggue quand même le coût globalement
        if not nick:
            logger.debug(f"Coût non attribué (nick=None) : ${cost:.5f} sur {channel}")
            return
        key = (channel, nick)
        self.user_costs[key] = self.user_costs.get(key, 0.0) + cost

    def show_quota(self, channel: str, nick: str):
        key = (channel, nick)
        ctx = self.user_contexts.get(key, [])
        total_cost = self.user_costs.get(key, 0.0)
        try:
            tokens = sum(len(self._tokenizer.encode(m["content"])) for m in ctx)
        except Exception:
            tokens = sum(len(m["content"]) // 4 for m in ctx)

        msg = (
            f"{nick} → Modèle: {self._model} | "
            f"Contexte: {len(ctx)}/{self.max_num_line} | "
            f"~{tokens} tokens | "
            f"Coût session: ${total_cost:.4f}"
        )
        self.privmsg(channel, msg)

    # ====================== Sauvegarde / Chargement ======================
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
            # Compatibilité avec anciens formats (liste de strings)
            if ctx and isinstance(ctx[0], str):
                ctx = [{"role": "user", "content": m} for m in ctx]
            self.user_contexts[(channel, nick)] = ctx
            self._context_last_used[(channel, nick)] = time.monotonic()
            self.privmsg(channel, f"Contexte '{title}' chargé ({len(ctx)} messages).")
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

    # ====================== API OpenAI ======================
    async def _openai_chat(self, messages: list, model: str, channel: str | None = None, nick: str | None = None) -> str:
        heavy = is_heavy_model(model)
        kwargs = {
            "model": model,
            "max_completion_tokens": 8000 if heavy else 1500,
            "messages": messages,
        }

        # FIX: temperature supprimé — de nombreux modèles (gpt-4o, gpt-5.x, o1, o3)
        # n'acceptent que la valeur par défaut (1) et renvoient une 400 sinon.
        # On laisse l'API utiliser sa valeur par défaut pour tous les modèles.

        timeout = 300 if heavy else 60
        try:
            resp = await asyncio.wait_for(
                self._openai.chat.completions.create(**kwargs),
                timeout=timeout,
            )
            text = (resp.choices[0].message.content or "").strip()

            if resp.usage and channel and nick:
                in_t = resp.usage.prompt_tokens
                out_t = resp.usage.completion_tokens
                price_in, price_out = get_model_price(model)
                cost = (in_t * price_in + out_t * price_out) / 1_000_000
                self.add_cost(channel, nick, cost)
                logger.info(f"Tokens | {model} | in={in_t} out={out_t} cost=${cost:.5f}")

            return text
        except asyncio.TimeoutError:
            logger.error(f"Timeout _openai_chat après {timeout}s (modèle={model})")
            raise
        except Exception:
            logger.exception("Erreur _openai_chat")
            raise

    async def _openai_image(self, prompt: str) -> str:
        resp = await asyncio.wait_for(
            self._openai.images.generate(
                model="dall-e-3", prompt=prompt, n=1, size="1024x1024"
            ),
            timeout=120,
        )
        return resp.data[0].url

    async def _tinyurl(self, url: str) -> str:
        r = await self._http.get(TINYURL_API, params={"url": url}, timeout=10)
        r.raise_for_status()
        return r.text.strip()

    async def _imgbb_upload(self, image_bytes: bytes) -> str:
        b64 = base64.b64encode(image_bytes).decode()
        r = await self._http.post(
            IMGBB_API_BASE,
            data={"key": self.imgbb_api_key, "image": b64},
            timeout=60,
        )
        r.raise_for_status()
        return r.json()["data"]["url"]

    # ====================== Tâches ======================
    async def _task_chat(self, channel: str, nick: str):
        key = (channel, nick)
        # FIX: snapshot du contexte au moment de l'appel pour limiter la race condition
        messages = list(self.user_contexts.get(key, []))
        if not messages:
            return
        model = self._model
        if is_heavy_model(model):
            self.privmsg(channel, f"{nick}: réflexion en cours avec {model}, patience...")

        try:
            text = await self._openai_chat(
                [{"role": "system", "content": self.build_system_prompt()}] + messages,
                model, channel, nick
            )
            if not text:
                self.privmsg(channel, "[Pas de réponse du modèle.]")
                return
            readable = LatexNodes2Text().latex_to_text(text)
            self.update_context(channel, nick, text, "assistant")
            await self.send_chunks(channel, readable)
        except asyncio.TimeoutError:
            self.privmsg(channel, f"[Timeout : le modèle {model} n'a pas répondu à temps.]")
        except Exception as e:
            logger.exception("Erreur chat")
            self.privmsg(channel, f"[Erreur : {str(e)[:150]}]")

    async def _task_vision(self, channel: str, nick: str, image_url: str):
        try:
            image_url = await validate_public_url(image_url)
            text = await self._openai_chat([{
                "role": "user",
                "content": [
                    {"type": "text", "text": "Décris cette image en détails, en français."},
                    {"type": "image_url", "image_url": {"url": image_url}},
                ]
            }], "gpt-4o", channel, nick)
            desc = text or "Impossible de décrire l'image."
            self.update_context(channel, nick, f"[vision: {image_url}]", "user")
            self.update_context(channel, nick, desc, "assistant")
            await self.send_chunks(channel, f"Description : {desc}")
        except Exception as e:
            logger.exception("Vision error")
            self.privmsg(channel, f"Erreur vision : {e}")

    async def _task_summarize_url(self, channel: str, url: str):
        try:
            url = await validate_public_url(url)
            resp = await self._http.get(url, timeout=12, headers={"User-Agent": "Mozilla/5.0"})
            resp.raise_for_status()
            soup = BeautifulSoup(
                resp.content[:512 * 1024].decode("utf-8", errors="replace"),
                "html.parser"
            )
            page_text_raw = soup.get_text(separator=" ", strip=True)

            # FIX: tronquer en tokens plutôt qu'en caractères pour respecter la limite du modèle
            try:
                tokens = self._tokenizer.encode(page_text_raw)
                page_text = self._tokenizer.decode(tokens[:3000])
            except Exception:
                page_text = page_text_raw[:4000]

            summary = await self._openai_chat(
                [{"role": "user", "content": f"Résume clairement en français cette page web :\n{page_text}"}],
                "gpt-4o-mini"
            )
            await self.send_chunks(
                channel,
                f"Résumé : {summary}" if summary else "Impossible de résumer cette page."
            )
        except Exception as e:
            logger.exception("URL summarize error")
            self.privmsg(channel, f"Erreur résumé URL : {e}")

    async def _task_image_tiny(self, channel: str, nick: str, prompt: str):
        # FIX: nick passé pour comptabiliser le coût
        async with self._heavy_semaphore:
            try:
                openai_url = await self._openai_image(prompt)
                short = await self._tinyurl(openai_url)
                self.add_cost(channel, nick, DALL_E_COST_PER_IMAGE)
                self.privmsg(channel, short)
            except Exception as e:
                self.privmsg(channel, f"Erreur génération image : {e}")

    async def _task_image_imgbb(self, channel: str, nick: str, prompt: str):
        # FIX: nick passé pour comptabiliser le coût
        async with self._heavy_semaphore:
            try:
                openai_url = await self._openai_image(prompt)
                img_bytes = (await self._http.get(openai_url, timeout=20)).content
                public_url = await self._imgbb_upload(img_bytes)
                self.add_cost(channel, nick, DALL_E_COST_PER_IMAGE)
                self.privmsg(channel, public_url)
            except Exception as e:
                self.privmsg(channel, f"Erreur ImgBB : {e}")

    async def _task_image_local(self, channel: str, nick: str, prompt: str):
        # FIX: nick passé pour comptabiliser le coût
        async with self._heavy_semaphore:
            try:
                openai_url = await self._openai_image(prompt)
                img_bytes = (await self._http.get(openai_url, timeout=20)).content
                name = f"img_{uuid.uuid4().hex[:10]}.png"
                path = os.path.join(self.image_local_dir, name)
                with open(path, "wb") as f:
                    f.write(img_bytes)
                url = f"{self.web_url_base.rstrip('/')}/{name}"
                self.add_cost(channel, nick, DALL_E_COST_PER_IMAGE)
                self.privmsg(channel, url)
            except Exception as e:
                self.privmsg(channel, f"Erreur image locale : {e}")

    async def _task_sora(self, channel: str, nick: str, prompt: str):
        # FIX: nick passé pour comptabiliser le coût
        async with self._heavy_semaphore:
            try:
                video = await self._openai.videos.create(
                    model="sora-2", prompt=prompt, seconds=4
                )
                self.privmsg(channel, f"Sora → job {video.id[-10:]} lancé...")

                loop = asyncio.get_running_loop()
                deadline = loop.time() + 600
                last_progress = None
                last_change = loop.time()

                while video.status in ("queued", "in_progress"):
                    if loop.time() > deadline:
                        self.privmsg(channel, "Sora timeout (10 min), job abandonné.")
                        return
                    await asyncio.sleep(15)
                    video = await self._openai.videos.retrieve(video.id)
                    progress = getattr(video, "progress", None)
                    now = loop.time()
                    if progress != last_progress:
                        last_progress = progress
                        last_change = now
                        if progress is not None:
                            self.privmsg(channel, f"Sora progression : {progress}%")
                    elif last_progress is not None and now - last_change > 120:
                        self.privmsg(channel, "Sora bloqué — abandonné.")
                        return

                if video.status == "completed":
                    self.privmsg(channel, "Vidéo terminée, téléchargement...")
                    filename = f"{video.id}.mp4"
                    local_path = os.path.join(self.sora_local_dir, filename)

                    raw = await self._openai.videos.download_content(video.id, variant="video")

                    # Gestion robuste du retour (bytes ou objet avec .read())
                    if isinstance(raw, (bytes, bytearray)):
                        content = raw
                    elif hasattr(raw, "read"):
                        content = raw.read() if not asyncio.iscoroutinefunction(raw.read) else await raw.read()
                    else:
                        content = raw

                    with open(local_path, "wb") as f:
                        f.write(content)

                    self.add_cost(channel, nick, SORA_COST_PER_SECOND * 4)

                    public_url = f"{self.sora_public_url.rstrip('/')}/{filename}"
                    try:
                        public_url = await self._tinyurl(public_url)
                    except Exception:
                        pass
                    self.privmsg(channel, f"Vidéo Sora prête → {public_url}")
                else:
                    self.privmsg(channel, f"Échec Sora : statut {video.status}")
            except Exception as e:
                logger.exception("Sora error")
                self.privmsg(channel, f"Erreur Sora : {e}")

    # ====================== Routage ======================
    @irc3.event(irc3.rfc.PRIVMSG)
    async def on_privmsg(self, mask, target, data, **kwargs):
        if not target.startswith("#"):
            return

        nick = mask.nick
        channel = target
        message = data.strip()
        bot_nick = self.bot.nick

        if nick in self.blocked_users:
            return
        if not message.startswith(bot_nick + ":"):
            return

        if nick != self.admin_user:
            now = time.monotonic()
            if now - self._user_last_call.get(nick, 0) < self.rate_limit_secs:
                self.privmsg(channel, f"{nick}: attends {self.rate_limit_secs}s entre les commandes.")
                return
            self._user_last_call[nick] = now

        message = message[len(bot_nick) + 1:].strip()
        parts = message.split(" ", 1)
        command = parts[0].lower()
        args = parts[1].strip() if len(parts) > 1 else ""

        # Commandes admin
        if command == "tag" and nick == self.admin_user:
            self._tag = args
            self.privmsg(channel, f"Tag mis à jour : {self._tag!r}")

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

        # Commandes utilisateur
        elif command in ("quota", "status"):
            self.show_quota(channel, nick)

        elif command == "help":
            self.privmsg(channel,
                "Commandes : raz | quota | status | save [titre] | load [titre] | delete [titre] | files | "
                "list-models | current | image [prompt] | local [prompt] | imgbb [prompt] | "
                "vision [url] | url [url] | video [prompt]")

        elif command == "raz":
            self.reset_context(channel, nick)
            self.privmsg(channel, "Conversation oubliée (coût réinitialisé).")

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

        # FIX: nick transmis aux tâches image/video pour comptabiliser les coûts
        elif command == "image":
            if not args:
                self.privmsg(channel, "Prompt vide !")
            else:
                asyncio.create_task(self._task_image_tiny(channel, nick, args))
        elif command == "local":
            if not args:
                self.privmsg(channel, "Prompt vide !")
            else:
                asyncio.create_task(self._task_image_local(channel, nick, args))
        elif command == "imgbb":
            if not args:
                self.privmsg(channel, "Prompt vide !")
            else:
                asyncio.create_task(self._task_image_imgbb(channel, nick, args))
        elif command == "vision":
            if not args:
                self.privmsg(channel, "URL manquante !")
            else:
                asyncio.create_task(self._task_vision(channel, nick, args))
        elif command == "url":
            if not args:
                self.privmsg(channel, "URL manquante !")
            else:
                asyncio.create_task(self._task_summarize_url(channel, args))
        elif command == "video":
            if not args:
                self.privmsg(channel, "Prompt vide !")
            else:
                asyncio.create_task(self._task_sora(channel, nick, args))

        else:
            self.update_context(channel, nick, message)
            asyncio.create_task(self._task_chat(channel, nick))

    @irc3.event(irc3.rfc.JOIN)
    async def on_join(self, mask, channel, **kwargs):
        if mask.nick == self.bot.nick:
            logger.info(f"Rejoint {channel}")

    @irc3.event(irc3.rfc.KICK)
    async def on_kick(self, mask, channel, target, **kwargs):
        if target == self.bot.nick:
            await asyncio.sleep(5)
            self.bot.join(channel)


def main():
    config_file = sys.argv[1] if len(sys.argv) > 1 else "zozo.json"
    with open(config_file, "r", encoding="utf-8") as f:
        cfg = json.load(f)

    channels = [ch.strip() for ch in cfg["channels"].split(",")]

    bot = irc3.IrcBot.from_config({
        "nick":              cfg["nickname"],
        "host":              cfg["server"],
        "port":              cfg["port"],
        "includes":          ["irc3.plugins.core", "irc3.plugins.autojoins"],
        "autojoins":         channels,
        "api_key":           cfg["api_key"],
        "imgbb_api_key":     cfg["imgbb_api_key"],
        "admin_user":        cfg.get("admin_user", ""),
        "max_num_line":      cfg.get("max_num_line", 20),
        "rate_limit_seconds": cfg.get("rate_limit_seconds", 5),
        "flood_delay":       cfg.get("flood_delay", 0.35),
        "display_url":       cfg.get("display_url", "https://labynet.fr/images"),
        "image_local_dir":   cfg.get("image_local_dir", "/var/www/html/images"),
        "sora_local_dir":    cfg.get("sora_local_dir", "/var/www/html/sora"),
        "sora_public_url":   cfg.get("sora_public_url", "https://labynet.fr/sora"),
    })
    bot.include(__name__)
    bot.run()


if __name__ == "__main__":
    main()
