import irc3
import asyncio
import json
import logging
import os
import re
import sys
import uuid
import hashlib
from typing import Dict, Tuple, List, Any
from pylatexenc.latex2text import LatexNodes2Text

# Google GenAI SDK
from google import genai
from google.genai import types as genai_types

logging.basicConfig(
    level=logging.INFO,
    format="%(asctime)s [%(levelname)s] %(message)s",
    handlers=[logging.StreamHandler()]
)
logger = logging.getLogger(__name__)

IRC_SYSTEM_PROMPT = (
    "Tu es un assistant IA sur un serveur IRC. "
    "Réponds de manière concise, texte brut uniquement, sans Markdown."
)

# Modèles Gemini 2026
MODEL_CHAT  = "gemini-2.5-flash"
MODEL_IMAGE = "imagen-4.0-generate-001"
MODEL_VEO   = "veo-3.1-generate-preview"


def sanitize_nick(nick: str) -> str:
    safe_base = re.sub(r'[^\w\-]', '_', nick)[:24]
    nick_hash = hashlib.md5(nick.encode('utf-8')).hexdigest()[:4]
    return f"{safe_base}_{nick_hash}"


def _ctx_to_api(ctx: List[Dict[str, str]]) -> List[genai_types.Content]:
    """
    Convertit le contexte sérialisable {"role": str, "text": str}
    en objets genai_types.Content pour l'API Gemini.
    """
    return [
        genai_types.Content(
            role=msg["role"],
            parts=[genai_types.Part(text=msg["text"])]
        )
        for msg in ctx
    ]


@irc3.plugin
class ZozoPlugin:
    def __init__(self, bot):
        self.bot = bot
        cfg = bot.config

        self.gemini_api_key  = cfg.get("gemini_api_key")
        self.web_url_base    = cfg.get("display_url", "https://labynet.fr/images")
        self.video_public_url = cfg.get("video_public_url", "https://labynet.fr/videos")
        self.image_local_dir = cfg.get("image_local_dir", "/var/www/html/images")
        self.video_local_dir = cfg.get("video_local_dir", "/var/www/html/videos")
        self.max_num_line    = int(cfg.get("max_num_line", 20))
        self.flood_delay     = float(cfg.get("flood_delay", 0.5))

        self._gemini = genai.Client(api_key=self.gemini_api_key)

        # FIX: contexte stocké en dicts sérialisables {"role": str, "text": str}
        # Les genai_types.Part sont reconstruits à la volée dans _ctx_to_api()
        self.user_contexts: Dict[Tuple[str, str], List[Dict[str, str]]] = {}
        self._context_locks: Dict[Tuple[str, str], asyncio.Lock] = {}
        self._heavy_semaphore = asyncio.Semaphore(1)
        self._latex = LatexNodes2Text()

        os.makedirs(self.image_local_dir, exist_ok=True)
        os.makedirs(self.video_local_dir, exist_ok=True)
        os.makedirs("conversations", exist_ok=True)

        logger.info("Zozo Gemini Turbo prêt.")

    def _get_context_lock(self, key: Tuple[str, str]) -> asyncio.Lock:
        return self._context_locks.setdefault(key, asyncio.Lock())

    def privmsg(self, target: str, text: str):
        try:
            self.bot.privmsg(target, str(text)[:400])
        except Exception as e:
            logger.debug(f"privmsg échoué : {e}")

    async def send_chunks(self, target: str, message: str):
        """Envoi découpé UTF-8 safe pour IRC"""
        MAX_BYTES = 392
        for line in message.split('\n'):
            line = line.strip()
            if not line:
                continue
            remaining = line.encode('utf-8')
            while remaining:
                if len(remaining) <= MAX_BYTES:
                    self.privmsg(target, remaining.decode('utf-8'))
                    remaining = b''
                else:
                    cut = MAX_BYTES
                    while cut > 0 and (remaining[cut] & 0xC0) == 0x80:
                        cut -= 1
                    self.privmsg(target, remaining[:cut].decode('utf-8', errors='ignore'))
                    remaining = remaining[cut:]
                    await asyncio.sleep(self.flood_delay)

    def _clean(self, text: str) -> str:
        """Triple nettoyage : LaTeX → Markdown → sauts de ligne"""
        text = self._latex.latex_to_text(text)
        text = re.sub(r'[\*\#\_]', '', text)
        return text.replace('\n', ' ').strip()

    # ====================== TÂCHES ONE-SHOT ======================

    async def _task_vision(self, target: str, nick: str, url: str, prompt: str, ext: str):
        """Analyse d'image via Gemini (hors contexte chat)"""
        try:
            mime = "image/jpeg" if ext in ["jpg", "jpeg"] else f"image/{ext}"
            parts = [
                genai_types.Part(text=prompt or "Analyse cette image en français."),
                genai_types.Part(file_data=genai_types.FileData(file_uri=url, mime_type=mime)),
            ]
            resp = await self._gemini.aio.models.generate_content(
                model=MODEL_CHAT,
                contents=[genai_types.Content(role="user", parts=parts)],
                config=genai_types.GenerateContentConfig(system_instruction=IRC_SYSTEM_PROMPT),
            )
            await self.send_chunks(target, f"{nick}: {self._clean(resp.text)}")
        except Exception as e:
            logger.error(f"Vision error: {e}")
            self.privmsg(target, f"{nick}: Erreur vision.")

    async def _task_video(self, target: str, nick: str, prompt: str):
        """Génération vidéo avec Veo 3.1"""
        async with self._heavy_semaphore:
            self.privmsg(target, f"{nick}: Vidéo Veo en cours...")
            try:
                loop = asyncio.get_running_loop()

                # generate_videos() retourne directement le nom de l'opération (str)
                # et non un objet avec .done/.name
                op_name = await loop.run_in_executor(
                    None,
                    lambda: self._gemini.models.generate_videos(
                        model=MODEL_VEO,
                        prompt=f"{prompt}, 5s, vertical 9:16 smartphone video",
                        config=genai_types.GenerateVideosConfig(
                            duration_seconds=4, aspect_ratio="9:16"
                        ),
                    )
                )

                # Polling par nom d'opération jusqu'à completion ou timeout
                deadline = loop.time() + 600
                while True:
                    if loop.time() > deadline:
                        self.privmsg(target, f"{nick}: ❌ Veo timeout (10 min).")
                        return
                    await asyncio.sleep(15)
                    operation = await loop.run_in_executor(
                        None, lambda: self._gemini.operations.get(op_name)
                    )
                    if operation.done:
                        break

                video_obj = operation.response.generated_videos[0].video
                vid_bytes = await self._gemini.aio.files.download(file=video_obj)
                name = f"vid_{uuid.uuid4().hex[:12]}.mp4"
                with open(os.path.join(self.video_local_dir, name), "wb") as f:
                    f.write(vid_bytes)
                self.privmsg(target, f"{nick}: ✅ {self.video_public_url.rstrip('/')}/{name}")
            except Exception as e:
                logger.error(f"Video error: {e}")
                self.privmsg(target, f"{nick}: ❌ Erreur vidéo.")

    async def _task_image(self, target: str, nick: str, prompt: str):
        """Génération d'image avec Imagen 4"""
        async with self._heavy_semaphore:
            try:
                resp = await self._gemini.aio.models.generate_images(
                    model=MODEL_IMAGE,
                    prompt=prompt,
                    config=genai_types.GenerateImagesConfig(number_of_images=1),
                )
                img_bytes = resp.generated_images[0].image.image_bytes
                name = f"img_{uuid.uuid4().hex[:10]}.png"
                with open(os.path.join(self.image_local_dir, name), "wb") as f:
                    f.write(img_bytes)
                url = f"{self.web_url_base.rstrip('/')}/{name}"
                self.privmsg(target, f"{nick}: {url}")
            except Exception as e:
                logger.error(f"Image error: {e}")
                self.privmsg(target, f"{nick}: ❌ Erreur image.")

    # ====================== ROUTAGE IRC ======================

    @irc3.event(irc3.rfc.PRIVMSG)
    async def on_privmsg(self, mask, target, data, **kwargs):
        if not target.startswith("#"):
            return
        nick, message = mask.nick, data.strip()
        if not message.startswith(self.bot.nick + ":"):
            return

        cmd_raw = message[len(self.bot.nick) + 1:].strip()
        parts = cmd_raw.split(" ", 1)
        cmd, args = parts[0].lower(), (parts[1].strip() if len(parts) > 1 else "")
        key = (target, nick)

        # --- Commandes spécifiques ---
        if cmd == "vision":
            m = re.search(r'(https?://\S+\.(?P<ext>png|jpg|jpeg|webp))', args, re.I)
            if m:
                url, ext = m.group(1), m.group('ext').lower()
                asyncio.create_task(
                    self._task_vision(target, nick, url, args.replace(url, "").strip(), ext)
                )
            else:
                self.privmsg(target, f"{nick}: Lien image (png/jpg/webp) requis.")
            return

        elif cmd in ("image", "local", "imgbb"):
            if not args:
                self.privmsg(target, "Prompt vide !")
            else:
                asyncio.create_task(self._task_image(target, nick, args))
            return

        elif cmd == "video":
            if not args:
                self.privmsg(target, "Prompt vide !")
            else:
                asyncio.create_task(self._task_video(target, nick, args))
            return

        elif cmd == "raz":
            self.user_contexts[key] = []
            self.privmsg(target, f"{nick}: Mémoire effacée.")
            return

        # FIX: save et load restaurés avec contexte sérialisable
        elif cmd == "save":
            if not args:
                self.privmsg(target, "Titre requis.")
                return
            path = os.path.join("conversations", f"{sanitize_nick(nick)}.{args}.json")
            try:
                with open(path, "w", encoding="utf-8") as f:
                    json.dump(self.user_contexts.get(key, []), f, ensure_ascii=False, indent=2)
                self.privmsg(target, f"Sauvé : {args}")
            except Exception as e:
                logger.error(f"Save error: {e}")
                self.privmsg(target, "Erreur lors de la sauvegarde.")
            return

        elif cmd == "load":
            if not args:
                self.privmsg(target, "Titre requis.")
                return
            path = os.path.join("conversations", f"{sanitize_nick(nick)}.{args}.json")
            if os.path.exists(path):
                try:
                    with open(path, "r", encoding="utf-8") as f:
                        self.user_contexts[key] = json.load(f)
                    self.privmsg(target, f"Chargé : {args} ({len(self.user_contexts[key])} messages)")
                except Exception as e:
                    logger.error(f"Load error: {e}")
                    self.privmsg(target, "Erreur lors du chargement.")
            else:
                self.privmsg(target, "Fichier inconnu.")
            return

        elif cmd == "help":
            self.privmsg(target,
                "Commandes : raz | save [titre] | load [titre] | "
                "image [prompt] | video [prompt] | vision [url] [prompt]"
            )
            return

        # --- Chat textuel ---
        async with self._get_context_lock(key):
            ctx = self.user_contexts.setdefault(key, [])

            if re.search(r'https?://\S+\.(png|jpg|jpeg|webp)', cmd_raw, re.I):
                self.privmsg(target, f"{nick}: Utilise 'vision' pour les images.")
                return

            # FIX: stockage texte brut sérialisable
            ctx.append({"role": "user", "text": cmd_raw})
            if len(ctx) > self.max_num_line:
                ctx[:] = ctx[-self.max_num_line:]

            try:
                resp = await self._gemini.aio.models.generate_content(
                    model=MODEL_CHAT,
                    contents=_ctx_to_api(ctx),
                    config=genai_types.GenerateContentConfig(
                        system_instruction=IRC_SYSTEM_PROMPT
                    ),
                )
                answer = self._clean(resp.text)

                # FIX: stockage texte brut pour la réponse aussi
                ctx.append({"role": "model", "text": answer})
                await self.send_chunks(target, f"{nick}: {answer}")

            except Exception as e:
                logger.error(f"Chat error: {e}")
                if ctx:
                    ctx.pop()
                self.privmsg(target, f"{nick}: Erreur chat.")

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
    config_file = sys.argv[1] if len(sys.argv) > 1 else "zozo-gemini.json"
    with open(config_file, "r", encoding="utf-8") as f:
        cfg = json.load(f)
    # FIX: clés passées explicitement, pas de **cfg pour éviter d'exposer gemini_api_key
    bot = irc3.IrcBot.from_config({
        "nick":            cfg["nickname"],
        "host":            cfg["server"],
        "port":            cfg["port"],
        "includes":        ["irc3.plugins.core", "irc3.plugins.autojoins"],
        "autojoins":       [ch.strip() for ch in cfg["channels"].split(",")],
        "gemini_api_key":   cfg["gemini_api_key"],
        "display_url":      cfg.get("display_url", "https://labynet.fr/images"),
        "video_public_url": cfg.get("video_public_url", "https://labynet.fr/videos"),
        "image_local_dir":  cfg.get("image_local_dir", "/var/www/html/images"),
        "video_local_dir":  cfg.get("video_local_dir", "/var/www/html/videos"),
        "max_num_line":    cfg.get("max_num_line", 20),
        "flood_delay":     cfg.get("flood_delay", 0.5),
    })
    bot.include(__name__)
    bot.run()


if __name__ == "__main__":
    main()
