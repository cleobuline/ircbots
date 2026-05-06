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

IRC_SYSTEM_PROMPT = (
    "Tu es un assistant IA sur un serveur IRC. "
    "Tu réponds dans la langue de l'usager de façon chaleureuse mais concise."
)

# Modèles Gemini
MODEL_CHAT = "gemini-2.5-flash"
# MODEL_CHAT  = "gemini-3.1-pro-preview"
MODEL_IMAGE = "imagen-4.0-generate-001"
MODEL_VEO   = "veo-3.1-generate-preview"
MODEL_MUSIC = "lyria-3-pro-preview"

# Limites de sécurité
MAX_DOWNLOAD_BYTES = 10 * 1024 * 1024
MAX_CONTEXT_KEYS   = 500
RATE_LIMIT_SECONDS = 3.0
URL_COOLDOWN_SECS  = 8.0

# Pre-compilation des Regex pour la performance
RE_MARKDOWN_BOLD = re.compile(r'\*{1,3}(.*?)\*{1,3}')
RE_MARKDOWN_TITLES = re.compile(r'#{1,6}\s*')
RE_MARKDOWN_UNDERLINE = re.compile(r'_{1,2}(.*?)_{1,2}')
RE_MARKDOWN_CODE = re.compile(r'`{1,3}.*?`{1,3}', re.DOTALL)
RE_HTML_CLEAN = re.compile(r'<(script|style|header|footer|nav|aside|form).*?>.*?</\1>', re.DOTALL | re.IGNORECASE)
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
    # On ajoute \. à l'intérieur des crochets pour autoriser les points
    safe = re.sub(r'[^\w\-\.]', '_', title)[:32]
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

        logger.info("Zozo Gemini (Version Complète Optimisée) prêt.")

    # ====================== HELPERS ======================

    def _get_context_lock(self, key: Tuple[str, str]) -> asyncio.Lock:
        return self._context_locks.setdefault(key, asyncio.Lock())

    def _access_context(self, key: Tuple[str, str]) -> List[Dict[str, str]]:
        if key in self.user_contexts:
            self.user_contexts.move_to_end(key)
        else:
            if len(self.user_contexts) >= MAX_CONTEXT_KEYS:
                old_key, _ = self.user_contexts.popitem(last=False)
                self._context_locks.pop(old_key, None)
            self.user_contexts[key] = []
        return self.user_contexts[key]

    def _check_rate_limit(self, nick: str, cooldown: float) -> bool:
        now = time.monotonic()
        last = self._last_request.get(nick, 0.0)
        if now - last < cooldown:
            return False
        self._last_request[nick] = now
        return True

    def privmsg(self, target: str, text: str):
        self.bot.privmsg(target, str(text)[:400])

    async def send_chunks(self, target: str, message: str):
        """Découpe le message en respectant les sauts de ligne et l'encodage UTF-8."""
        if not message:
            return
            
        MAX_BYTES = 392
        
        # On traite chaque ligne séparément pour respecter la structure originale
        for line in message.split('\n'):
            line = line.strip()
            if not line:
                continue
                
            remaining = line.encode('utf-8')
            
            while remaining:
                if len(remaining) <= MAX_BYTES:
                    self.privmsg(target, remaining.decode('utf-8', errors='replace'))
                    break
                
                # On recule jusqu'à trouver une frontière de caractère UTF-8 valide[cite: 2]
                cut = MAX_BYTES
                while cut > 0 and (remaining[cut] & 0xC0) == 0x80:
                    cut -= 1
                
                # On tente de couper sur le dernier espace disponible dans le bloc[cite: 2]
                chunk_bytes = remaining[:cut]
                last_space = chunk_bytes.rfind(b' ')
                
                if last_space > 5:
                    to_send = chunk_bytes[:last_space]
                    # On retire l'espace utilisé pour la coupe du reste à envoyer[cite: 2]
                    remaining = remaining[last_space:].lstrip(b' ')
                else:
                    to_send = chunk_bytes
                    remaining = remaining[cut:]
                
                self.privmsg(target, to_send.decode('utf-8', errors='replace'))
                await asyncio.sleep(self.flood_delay)

    def _extract_text(self, html: str) -> str:
        text = RE_HTML_CLEAN.sub('', html)
        text = RE_HTML_TAGS.sub(' ', text)
        return RE_WHITESPACE.sub(' ', text).strip()[:12000]

    def _clean(self, text: str) -> str:
        if not text: return ""
        text = self._latex.latex_to_text(text)
        text = RE_MARKDOWN_BOLD.sub(r'\1', text)
        text = RE_MARKDOWN_TITLES.sub('', text)
        text = RE_MARKDOWN_UNDERLINE.sub(r'\1', text)
        text = RE_MARKDOWN_CODE.sub('', text)
        # SUPPRIME la ligne ci-dessous ou modifie-la comme ceci :
        return text.strip()


    # ====================== TÂCHES ASYNC ======================

    async def _task_music(self, target: str, nick: str, prompt: str):
        async with self._heavy_semaphore:
            self.privmsg(target, f"{nick}: Composition musicale (Lyria 3) en cours... 🎹")
            try:
                resp = await self._gemini.aio.models.generate_content(
                    model=MODEL_MUSIC,
                    contents=prompt,
                    config=genai_types.GenerateContentConfig(response_modalities=["AUDIO"])
                )
                
                audio_bytes = None
                # Exploration profonde pour trouver l'audio
                if resp.candidates and resp.candidates[0].content.parts:
                    for part in resp.candidates[0].content.parts:
                        if hasattr(part, 'inline_data') and part.inline_data:
                            audio_bytes = part.inline_data.data
                            break
                        elif hasattr(part, 'data') and part.data:
                            audio_bytes = part.data
                            break

                if audio_bytes:
                    name = f"snd_{uuid.uuid4().hex[:10]}.mp3"
                    file_path = os.path.join(self.audio_local_dir, name)
                    await save_file_async(file_path, audio_bytes)
                    
                    url = f"{self.audio_public_url.rstrip('/')}/{name}"
                    self.privmsg(target, f"{nick}: ✅ Musique prête : {url}")
                else:
                    # Extraction de la raison précise de l'échec
                    reason = "Inconnue"
                    if resp.candidates:
                        # Vérifie si c'est un blocage de sécurité (SAFETY) ou autre
                        reason = resp.candidates[0].finish_reason or "Contenu filtré"
                    
                    self.privmsg(target, f"{nick}: ❌ Flux audio vide. Raison : {reason}")
                    logger.warning(f"Échec Lyria pour {nick}. Prompt: {prompt} | Raison: {reason}")
                    
            except Exception as e:
                logger.error(f"Music Error: {e}")
                self.privmsg(target, f"{nick}: ❌ Erreur technique : {type(e).__name__}")
                                
    async def _task_url(self, target: str, nick: str, url: str, prompt: str):
        async with self._heavy_semaphore:
            self.privmsg(target, f"{nick}: Exploration de la page... 🌐")
            if "pastebin.com" in url and "/raw/" not in url: url = url.replace("pastebin.com/", "pastebin.com/raw/")
            try:
                async with httpx.AsyncClient(timeout=20.0, follow_redirects=True) as client:
                    async with client.stream("GET", url) as response:
                        response.raise_for_status()
                        chunks, total = [], 0
                        async for chunk in response.aiter_bytes(chunk_size=8192):
                            total += len(chunk)
                            if total > MAX_DOWNLOAD_BYTES:
                                self.privmsg(target, f"{nick}: ❌ Page trop lourde."); return
                            chunks.append(chunk)
                        raw = b"".join(chunks).decode("utf-8", errors="replace")

                web_text = raw[:15000] if "text/plain" in response.headers.get("Content-Type", "") or "pastebin" in url else self._extract_text(raw)
                if len(web_text) < 10: self.privmsg(target, f"{nick}: ❌ Contenu illisible."); return

                ctx = self._access_context((target, nick))
                ctx.append({"role": "user", "text": f"Contenu de {url} :\n{web_text}\n\nQuestion : {prompt or 'Résumé court.'}"})
                resp = await self._gemini.aio.models.generate_content(model=MODEL_CHAT, contents=_ctx_to_api(ctx[-3:]), config=genai_types.GenerateContentConfig(system_instruction=IRC_SYSTEM_PROMPT))
                answer = self._clean(resp.text or "")
                ctx.append({"role": "model", "text": answer})
                await self.send_chunks(target, f"{nick}: {answer}")
            except Exception as e:
                self.privmsg(target, f"{nick}: ❌ Erreur URL : {str(e)[:50]}")

    async def _task_vision(self, target: str, nick: str, url: str, prompt: str, ext: str):
        self.privmsg(target, f"{nick}: Analyse média en cours... 🧐")
        try:
            mime = "video/mp4" if ext == "mp4" else f"image/{ext}".replace("jpg", "jpeg")
            async with httpx.AsyncClient(timeout=30.0) as client:
                async with client.stream("GET", url) as resp_file:
                    resp_file.raise_for_status()
                    chunks, total = [], 0
                    async for chunk in resp_file.aiter_bytes(chunk_size=8192):
                        total += len(chunk)
                        if total > MAX_DOWNLOAD_BYTES: self.privmsg(target, f"{nick}: ❌ Trop lourd."); return
                        chunks.append(chunk)
            
            resp = await self._gemini.aio.models.generate_content(
                model=MODEL_CHAT,
                contents=[genai_types.Content(role="user", parts=[
                    genai_types.Part(text=prompt or "Analyse détaillée."),
                    genai_types.Part(inline_data=genai_types.Blob(mime_type=mime, data=b"".join(chunks)))
                ])],
                config=genai_types.GenerateContentConfig(system_instruction=IRC_SYSTEM_PROMPT)
            )
            await self.send_chunks(target, f"{nick}: {self._clean(resp.text or '')}")
        except Exception:
            self.privmsg(target, f"{nick}: ❌ Erreur analyse média.")

    async def _task_video(self, target: str, nick: str, prompt: str):
        async with self._heavy_semaphore:
            self.privmsg(target, f"{nick}: Génération vidéo Veo... ⏳")
            try:
                operation = await self._gemini.aio.models.generate_videos(
                    model=MODEL_VEO, prompt=f"{prompt}, 4s, vertical 9:16",
                    config=genai_types.GenerateVideosConfig(duration_seconds=4, aspect_ratio="9:16"),
                )
                while not operation.done:
                    await asyncio.sleep(15)
                    operation = await self._gemini.aio.operations.get(operation)

                if operation.error or not operation.response.generated_videos:
                    err = getattr(operation.error, 'message', "Filtre de sécurité")
                    self.privmsg(target, f"{nick}: ❌ Erreur : {err}"); return

                vid_bytes = await self._gemini.aio.files.download(file=operation.response.generated_videos[0].video)
                name = f"vid_{uuid.uuid4().hex[:12]}.mp4"
                await save_file_async(os.path.join(self.video_local_dir, name), vid_bytes)
                self.privmsg(target, f"{nick}: ✅ {self.video_public_url.rstrip('/')}/{name}")
            except Exception as e:
                self.privmsg(target, f"{nick}: ❌ Erreur : {type(e).__name__}")

    async def _task_image(self, target: str, nick: str, prompt: str):
        async with self._heavy_semaphore:
            self.privmsg(target, f"{nick}: Génération d'image... 🎨")
            try:
                resp = await self._gemini.aio.models.generate_images(model=MODEL_IMAGE, prompt=prompt, config=genai_types.GenerateImagesConfig(number_of_images=1))
                if not resp.generated_images: self.privmsg(target, f"{nick}: ❌ Bloqué."); return
                img_bytes = resp.generated_images[0].image.image_bytes
                name = f"img_{uuid.uuid4().hex[:10]}.png"
                await save_file_async(os.path.join(self.image_local_dir, name), img_bytes)
                self.privmsg(target, f"{nick}: ✅ {self.web_url_base.rstrip('/')}/{name}")
            except Exception:
                self.privmsg(target, f"{nick}: ❌ Erreur image.")

    # ====================== ROUTAGE IRC ======================

    @irc3.event(irc3.rfc.PRIVMSG)
    async def on_privmsg(self, mask, target, data, **kwargs):
        if not target.startswith("#"): return
        nick, message = mask.nick, data.strip()
        prefix = self.bot.nick + ":"
        if not message.startswith(prefix): return

        cmd_raw = message[len(prefix):].strip()
        if not cmd_raw: return
        parts = cmd_raw.split(maxsplit=1)
        cmd, args = parts[0].lower(), (parts[1].strip() if len(parts) > 1 else "")
        key = (target, nick)

        cooldown = URL_COOLDOWN_SECS if cmd in {"image", "video", "music", "url", "vision"} else RATE_LIMIT_SECONDS
        if not self._check_rate_limit(nick, cooldown):
            self.privmsg(target, f"{nick}: Doucement ! ⏳"); return

        if cmd == "vision":
            m = re.search(r'(https?://\S+\.(?P<ext>png|jpg|jpeg|webp|mp4))', args, re.I)
            if m: asyncio.create_task(self._task_vision(target, nick, m.group(1), args.replace(m.group(1), "").strip(), m.group('ext').lower()))
            else: self.privmsg(target, f"{nick}: Lien média requis.")
            return

        elif cmd == "url":
            m = re.search(r'(https?://\S+)', args)
            if m: asyncio.create_task(self._task_url(target, nick, m.group(1), args.replace(m.group(1), "").strip()))
            else: self.privmsg(target, f"{nick}: URL requise.")
            return

        elif cmd in {"image", "video", "music"}:
            if not args: self.privmsg(target, f"{nick}: Prompt requis.")
            else:
                task_map = {"image": self._task_image, "video": self._task_video, "music": self._task_music}
                asyncio.create_task(task_map[cmd](target, nick, args))
            return

        elif cmd == "raz":
            self._access_context(key).clear()
            self.privmsg(target, f"{nick}: Mémoire effacée. ✅"); return

        elif cmd == "save":
            if not args: self.privmsg(target, f"{nick}: Titre requis."); return
            safe_t = sanitize_title(args)
            await save_json_async(os.path.join("conversations", f"{sanitize_nick(nick)}.{safe_t}.json"), self._access_context(key))
            self.privmsg(target, f"{nick}: 💾 Sauvé : {safe_t}."); return

        elif cmd == "load":
            if not args: self.privmsg(target, f"{nick}: Titre requis."); return
            path = os.path.join("conversations", f"{sanitize_nick(nick)}.{sanitize_title(args)}.json")
            if os.path.exists(path):
                def _l(): 
                    with open(path, "r", encoding="utf-8") as f: return json.load(f)
                self.user_contexts[key] = await asyncio.get_running_loop().run_in_executor(None, _l)
                self.privmsg(target, f"{nick}: 📂 Chargé ({len(self.user_contexts[key])} msg).")
                return  # <--- AJOUTE CETTE LIGNE ICI
            else: self.privmsg(target, f"{nick}: Inconnu."); return
        elif cmd == "list":
            p = sanitize_nick(nick)
            files = [f.replace(f"{p}.", "").replace(".json", "") for f in os.listdir("conversations") if f.startswith(p) and f.endswith(".json")]
            self.privmsg(target, f"{nick}: 📋 {', '.join(files) if files else 'Aucune.'}"); return

        elif cmd == "delete":
            if not args: self.privmsg(target, f"{nick}: Titre requis."); return
            path = os.path.join("conversations", f"{sanitize_nick(nick)}.{sanitize_title(args)}.json")
            if os.path.exists(path): 
                await asyncio.get_running_loop().run_in_executor(None, os.remove, path)
                self.privmsg(target, f"{nick}: 🗑️ Supprimé."); return

        elif cmd == "help":
            self.privmsg(target, f"{nick}: raz|save|load|list|delete|image|video|music|vision|url"); return

        # ---- CHAT ----
        async with self._get_context_lock(key):
            ctx = self._access_context(key)
            if re.search(r'https?://\S+\.(png|jpg|jpeg|webp)', cmd_raw, re.I):
                self.privmsg(target, f"{nick}: Utilise 'vision' pour les images."); return
            ctx.append({"role": "user", "text": cmd_raw})
            if len(ctx) > self.max_num_line: ctx[:] = ctx[-self.max_num_line:]
            try:
                resp = await self._gemini.aio.models.generate_content(model=MODEL_CHAT, contents=_ctx_to_api(ctx), config=genai_types.GenerateContentConfig(system_instruction=IRC_SYSTEM_PROMPT))
                answer = self._clean(resp.text or "")
                ctx.append({"role": "model", "text": answer})
                await self.send_chunks(target, f"{nick}: {answer}")
            except Exception as e:
                if ctx: ctx.pop()
                # On affiche le type d'erreur et le message court
                err_msg = f"{type(e).__name__}: {str(e)[:100]}"
                logger.error(f"Chat Error: {e}")
                self.privmsg(target, f"{nick}: ❌ {err_msg}")

    @irc3.event(irc3.rfc.JOIN)
    async def on_join(self, mask, channel, **kwargs):
        if mask.nick == self.bot.nick: logger.info(f"Rejoint {channel}")

 
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
