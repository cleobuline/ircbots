import asyncio
import json
import logging
import os
import re
import sys
import time
import uuid
import hashlib
import httpx
import irc3 
import functools
from collections import OrderedDict
from typing import Dict, Tuple, List, Optional
from pylatexenc.latex2text import LatexNodes2Text

# Google GenAI SDK
from google import genai
from google.genai import types as genai_types

# ====================== CONFIGURATION & LOGGING ======================

logging.basicConfig(
    level=logging.INFO,
    format="%(asctime)s [%(levelname)s] %(message)s",
    handlers=[logging.StreamHandler()]
)
logger = logging.getLogger(__name__)

IRC_SYSTEM_PROMPT = "Tu es un assistant IA concis sur IRC. Réponds toujours dans la langue de l'usager. Sois précis et scientifique."

MODEL_CHAT  = "gemini-2.5-flash"
MODEL_IMAGE = "imagen-4.0-generate-001"
MODEL_VEO   = "veo-3.1-generate-preview"
MODEL_MUSIC = "lyria-3-pro-preview"

MAX_DOWNLOAD_BYTES = 10 * 1024 * 1024
MAX_CONTEXT_KEYS   = 500
RATE_LIMIT_SECONDS = 3.0
URL_COOLDOWN_SECS  = 8.0

# Pre-compilation des Regex pour la performance
RE_MARKDOWN_BOLD = re.compile(r'\*{1,3}(.*?)\*{1,3}')
RE_MARKDOWN_TITLES = re.compile(r'#{1,6}\s*')
RE_MARKDOWN_UNDERLINE = re.compile(r'_{1,2}(.*?)_{1,2}')
RE_MARKDOWN_CODE = re.compile(r'`{1,3}.*?`{1,3}', re.DOTALL)
RE_HTML_CLEAN = re.compile(r'<(script|style|header|footer|nav|aside).*?>.*?</\1>', re.DOTALL | re.IGNORECASE)
RE_HTML_TAGS = re.compile(r'<[^>]+>')
RE_WHITESPACE = re.compile(r'\s+')

# ====================== UTILITAIRES NON-BLOQUANTS ======================

async def save_file_async(path: str, data: bytes):
    """Écrit des données binaires sans bloquer l'event loop."""
    def _write():
        with open(path, "wb") as f:
            f.write(data)
    await asyncio.get_running_loop().run_in_executor(None, _write)

async def save_json_async(path: str, data: list):
    """Sauvegarde du JSON sans bloquer l'event loop."""
    def _write_json():
        with open(path, "w", encoding="utf-8") as f:
            json.dump(data, f, ensure_ascii=False, indent=2)
    await asyncio.get_running_loop().run_in_executor(None, _write_json)

def sanitize_nick(nick: str) -> str:
    safe_base = re.sub(r'[^\w\-]', '_', nick)[:24]
    nick_hash = hashlib.md5(nick.encode('utf-8')).hexdigest()[:4]
    return f"{safe_base}_{nick_hash}"

def sanitize_title(title: str) -> str:
    safe = re.sub(r'[^\w\-]', '_', title)[:32]
    return safe if safe else "default"

def _ctx_to_api(ctx: List[Dict[str, str]]) -> List[genai_types.Content]:
    return [
        genai_types.Content(role=msg["role"], parts=[genai_types.Part(text=msg["text"])])
        for msg in ctx
    ]

# ====================== PLUGIN IRC ======================

@irc3.plugin
class ZozoPlugin:
    def __init__(self, bot):
        self.bot = bot
        cfg = bot.config

        self.gemini_api_key   = cfg.get("gemini_api_key")
        self.web_url_base     = cfg.get("display_url")
        self.video_public_url = cfg.get("video_public_url")
        self.audio_public_url = cfg.get("audio_public_url")
        self.audio_local_dir  = cfg.get("audio_local_dir")
        self.image_local_dir  = cfg.get("image_local_dir")
        self.video_local_dir  = cfg.get("video_local_dir")
        self.max_num_line     = int(cfg.get("max_num_line", 20))
        self.flood_delay      = float(cfg.get("flood_delay", 0.5))

        self._gemini = genai.Client(api_key=self.gemini_api_key)

        # Cache LRU pour les contextes
        self.user_contexts: OrderedDict[Tuple[str, str], List[Dict[str, str]]] = OrderedDict()
        self._context_locks: Dict[Tuple[str, str], asyncio.Lock] = {}
        self._heavy_semaphore = asyncio.Semaphore(1)
        self._latex = LatexNodes2Text()

        self._last_request: Dict[str, float] = {}

        for d in [self.image_local_dir, self.video_local_dir, self.audio_local_dir, "conversations"]:
            os.makedirs(d, exist_ok=True)

        logger.info("Zozo Gemini (Optimisé) prêt.")

    def _get_context_lock(self, key: Tuple[str, str]) -> asyncio.Lock:
        return self._context_locks.setdefault(key, asyncio.Lock())

    def _access_context(self, key: Tuple[str, str]) -> List[Dict[str, str]]:
        """Récupère le contexte en mode LRU (Least Recently Used)."""
        if key in self.user_contexts:
            self.user_contexts.move_to_end(key)
        else:
            if len(self.user_contexts) >= MAX_CONTEXT_KEYS:
                old_key, _ = self.user_contexts.popitem(last=False)
                self._context_locks.pop(old_key, None)
                logger.info(f"Purge LRU : {old_key} supprimé.")
            self.user_contexts[key] = []
        return self.user_contexts[key]

    def _check_rate_limit(self, nick: str, cooldown: float) -> bool:
        now = time.monotonic()
        last = self._last_request.get(nick, 0.0)
        if now - last < cooldown:
            return False
        self._last_request[nick] = now
        return True

    async def send_chunks(self, target: str, text: str):
        text = text.replace('\n', ' ').replace('\r', '')
        limit = 400
        while text:
            if len(text) <= limit:
                self.bot.privmsg(target, text)
                break
            chunk_limit = text.rfind(' ', 0, limit)
            if chunk_limit <= 0: chunk_limit = limit
            self.bot.privmsg(target, text[:chunk_limit].strip())
            text = text[chunk_limit:].strip()
            await asyncio.sleep(self.flood_delay)

    def _clean(self, text: str) -> str:
        """Nettoyage optimisé via Regex pré-compilées."""
        if not text: return ""
        text = self._latex.latex_to_text(text)
        text = RE_MARKDOWN_BOLD.sub(r'\1', text)
        text = RE_MARKDOWN_TITLES.sub('', text)
        text = RE_MARKDOWN_UNDERLINE.sub(r'\1', text)
        text = RE_MARKDOWN_CODE.sub('', text)
        return text.replace('\n', ' ').strip()

    def _extract_text(self, html: str) -> str:
        text = RE_HTML_CLEAN.sub('', html)
        text = RE_HTML_TAGS.sub(' ', text)
        return RE_WHITESPACE.sub(' ', text).strip()[:12000]

    # ====================== TÂCHES ASYNC ======================

    async def _task_music(self, target: str, nick: str, prompt: str):
        async with self._heavy_semaphore:
            self.bot.privmsg(target, f"{nick}: Composition musicale en cours... 🎹")
            try:
                resp = await self._gemini.aio.models.generate_content(
                    model=MODEL_MUSIC,
                    contents=prompt,
                    config=genai_types.GenerateContentConfig(response_modalities=["AUDIO", "TEXT"]),
                )
                audio_bytes = None
                for part in resp.candidates[0].content.parts:
                    if hasattr(part, 'inline_data') and part.inline_data:
                        audio_bytes = part.inline_data.data
                        break
                if audio_bytes:
                    name = f"snd_{uuid.uuid4().hex[:10]}.mp3"
                    await save_file_async(os.path.join(self.audio_local_dir, name), audio_bytes)
                    self.bot.privmsg(target, f"{nick}: ✅ {self.audio_public_url.rstrip('/')}/{name}")
                else:
                    self.bot.privmsg(target, f"{nick}: ❌ Aucun audio généré.")
            except Exception as e:
                self.bot.privmsg(target, f"{nick}: ❌ Erreur : {type(e).__name__}")

    async def _task_video(self, target: str, nick: str, prompt: str):
        async with self._heavy_semaphore:
            self.bot.privmsg(target, f"{nick}: Génération vidéo Veo... ⏳")
            try:
                operation = await self._gemini.aio.models.generate_videos(
                    model=MODEL_VEO,
                    prompt=prompt,
                    config=genai_types.GenerateVideosConfig(duration_seconds=4, aspect_ratio="9:16"),
                )
                
                # Polling
                while not operation.done:
                    await asyncio.sleep(15)
                    operation = await self._gemini.aio.operations.get(operation)

                if operation.error:
                    # Ici l'erreur de sécurité apparaîtra souvent en clair
                    err = getattr(operation.error, 'message', str(operation.error))
                    self.bot.privmsg(target, f"{nick}: ❌ Erreur Veo : {err}")
                    return

                # VERIFICATION CRUCIALE
                if not operation.response or not operation.response.generated_videos:
                    self.bot.privmsg(target, f"{nick}: ❌ Vidéo bloquée (Filtre de sécurité). 🛡️")
                    return

                video_obj = operation.response.generated_videos[0].video
                vid_bytes = await self._gemini.aio.files.download(file=video_obj)
                
                name = f"vid_{uuid.uuid4().hex[:12]}.mp4"
                await save_file_async(os.path.join(self.video_local_dir, name), vid_bytes)
                self.bot.privmsg(target, f"{nick}: ✅ {self.video_public_url.rstrip('/')}/{name}")

            except Exception as e:
                self.bot.privmsg(target, f"{nick}: ❌ Erreur technique vidéo.")

    async def _task_image(self, target: str, nick: str, prompt: str):
        async with self._heavy_semaphore:
            self.bot.privmsg(target, f"{nick}: Génération d'image... 🎨")
            try:
                resp = await self._gemini.aio.models.generate_images(
                    model=MODEL_IMAGE, 
                    prompt=prompt, 
                    config=genai_types.GenerateImagesConfig(number_of_images=1)
                )

                # VERIFICATION : Est-ce qu'on a bien reçu une image ?
                if not resp or not hasattr(resp, 'generated_images') or not resp.generated_images:
                    # Si c'est vide, c'est presque toujours le filtre de sécurité (Safety)
                    self.bot.privmsg(target, f"{nick}: ❌ Contenu bloqué ou refusé par les filtres de sécurité. 🛡️")
                    return

                img_bytes = resp.generated_images[0].image.image_bytes
                name = f"img_{uuid.uuid4().hex[:10]}.png"
                
                await save_file_async(os.path.join(self.image_local_dir, name), img_bytes)
                self.bot.privmsg(target, f"{nick}: ✅ {self.web_url_base.rstrip('/')}/{name}")

            except Exception as e:
                # Capture les erreurs d'API (Quota, Réseau, etc.)
                err_msg = str(e)
                if "429" in err_msg:
                    msg = "Quota épuisé. 📈"
                elif "safety" in err_msg.lower():
                    msg = "Refusé pour raisons de sécurité. 🛡️"
                else:
                    msg = f"Erreur : {type(e).__name__}"
                
                logger.error(f"Image error for {nick}: {e}")
                self.bot.privmsg(target, f"{nick}: ❌ {msg}")
    # ====================== ROUTAGE IRC ======================

    @irc3.event(irc3.rfc.PRIVMSG)
    async def on_privmsg(self, mask, target, data, **kwargs):
        if not target.startswith("#"): return
        nick, message = mask.nick, data.strip()
        
        # Détection flexible du prefix
        bot_prefix = self.bot.nick.lower() + ":"
        if not message.lower().startswith(bot_prefix): return

        cmd_raw = message[len(bot_prefix):].strip()
        if not cmd_raw: return

        parts = cmd_raw.split(maxsplit=1)
        cmd, args = parts[0].lower(), (parts[1].strip() if len(parts) > 1 else "")
        key = (target, nick)

        # Cooldown & Context Access
        cooldown = URL_COOLDOWN_SECS if cmd in {"image", "video", "music", "url", "vision"} else RATE_LIMIT_SECONDS
        if not self._check_rate_limit(nick, cooldown):
            self.bot.privmsg(target, f"{nick}: Doucement... ⏳")
            return

        async with self._get_context_lock(key):
            ctx = self._access_context(key)

            if cmd == "raz":
                ctx.clear()
                self.bot.privmsg(target, f"{nick}: Mémoire effacée. ✅")
            
            elif cmd == "save":
                if not args: return
                path = os.path.join("conversations", f"{sanitize_nick(nick)}.{sanitize_title(args)}.json")
                await save_json_async(path, ctx)
                self.bot.privmsg(target, f"{nick}: 💾 Sauvegardé.")

            elif cmd in {"image", "video", "music"}:
                if not args: self.bot.privmsg(target, f"{nick}: Prompt requis.")
                else: 
                    task_map = {"image": self._task_image, "video": self._task_video, "music": self._task_music}
                    asyncio.create_task(task_map[cmd](target, nick, args))

            elif cmd == "help":
                self.bot.privmsg(target, f"{nick}: raz | save [titre] | load [titre] | image | video | music | vision | url")

            else:
                # Chat textuel par défaut
                ctx.append({"role": "user", "text": cmd_raw})
                if len(ctx) > self.max_num_line: ctx[:] = ctx[-self.max_num_line:]
                try:
                    resp = await self._gemini.aio.models.generate_content(
                        model=MODEL_CHAT, contents=_ctx_to_api(ctx),
                        config=genai_types.GenerateContentConfig(system_instruction=IRC_SYSTEM_PROMPT)
                    )
                    answer = self._clean(resp.text)
                    ctx.append({"role": "model", "text": answer})
                    await self.send_chunks(target, f"{nick}: {answer}")
                except Exception as e:
                    if ctx: ctx.pop()
                    self.bot.privmsg(target, f"{nick}: ❌ Erreur API.")


# ====================== POINT D'ENTRÉE ======================

def main():
    config_file = sys.argv[1] if len(sys.argv) > 1 else "zozo-gemini.json"

    try:
        with open(config_file, "r", encoding="utf-8") as f:
            cfg = json.load(f)
    except FileNotFoundError:
        logger.error(f"Fichier de config introuvable : {config_file}")
        sys.exit(1)
    except json.JSONDecodeError as e:
        logger.error(f"Erreur JSON dans {config_file} : {e}")
        sys.exit(1)

    bot = irc3.IrcBot.from_config({
        "nick":             cfg["nickname"],
        "host":             cfg["server"],
        "port":             cfg["port"],
        "includes":         ["irc3.plugins.core", "irc3.plugins.autojoins"],
        "autojoins":        [ch.strip() for ch in cfg["channels"].split(",")],
        "gemini_api_key":   cfg["gemini_api_key"],
        "display_url":      cfg.get("display_url", "https://labynet.fr/images"),
        "video_public_url": cfg.get("video_public_url", "https://labynet.fr/videos"),
        "image_local_dir":  cfg.get("image_local_dir", "/var/www/html/images"),
        "video_local_dir":  cfg.get("video_local_dir", "/var/www/html/videos"),
        "audio_local_dir":  cfg.get("audio_local_dir", "/var/www/html/audio"),
        "audio_public_url": cfg.get("audio_public_url", "https://labynet.fr/audio"),
        "max_num_line":     cfg.get("max_num_line", 20),
        "flood_delay":      cfg.get("flood_delay", 0.5),
    })

    bot.include(__name__)

    try:
        bot.run()
    except KeyboardInterrupt:
        logger.info("Arrêt propre du bot (Ctrl+C).")


if __name__ == "__main__":
    main()
