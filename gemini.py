import irc3
import asyncio
import json
import logging
import os
import re
import sys
import uuid
import hashlib
import httpx
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
    "Tu répond de façon chaleureuse aux usager."
)

# Modèles Gemini 2026
MODEL_CHAT  = "gemini-2.5-flash"
MODEL_IMAGE = "imagen-4.0-generate-001"
MODEL_VEO   = "veo-3.1-generate-preview"
MODEL_MUSIC = "gemini-2.0-flash-exp"

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
        self.audio_public_url = cfg.get("audio_public_url", "https://labynet.fr/audio")
        self.audio_local_dir = cfg.get("audio_local_dir", "/var/www/html/audio")
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
        os.makedirs(self.audio_local_dir, exist_ok=True)
        os.makedirs("conversations", exist_ok=True)

        logger.info("Zozo Gemini Turbo prêt.")

    def _get_context_lock(self, key: Tuple[str, str]) -> asyncio.Lock:
        return self._context_locks.setdefault(key, asyncio.Lock())

    def privmsg(self, target: str, text: str):
        try:
            self.bot.privmsg(target, str(text)[:400])
        except Exception as e:
            logger.debug(f"privmsg échoué : {e}")

    async def send_chunks(self, target: str, text: str):
        """Envoie le texte par morceaux en coupant proprement aux espaces"""
        # On nettoie les sauts de ligne excessifs pour IRC
        text = text.replace('\n', ' ').replace('\r', '')
        limit = 400

        while len(text) > 0:
            if len(text) <= limit:
                self.privmsg(target, text)
                break
            
            # On cherche le dernier espace avant la limite
            chunk_limit = text.rfind(' ', 0, limit)
            
            # Si on ne trouve pas d'espace (mot ultra long), on coupe à la limite
            if chunk_limit <= 0:
                chunk_limit = limit
                
            chunk = text[:chunk_limit].strip()
            if chunk:
                self.privmsg(target, chunk)
            
            # On reprend la suite du texte
            text = text[chunk_limit:].strip()
            
            # Petit délai pour éviter le flood (0.5s par défaut dans ta config)
            await asyncio.sleep(self.bot.config.get("flood_delay", 0.5))
            
    def _extract_text(self, html: str) -> str:
        """Nettoie le HTML pour ne garder que le texte utile"""
        try:
            # On essaie avec BeautifulSoup si installé
            from bs4 import BeautifulSoup
            soup = BeautifulSoup(html, 'html.parser')
            # On supprime le superflu pour ne pas polluer l'IA
            for s in soup(["script", "style", "header", "footer", "nav", "aside", "form"]):
                s.decompose()
            text = soup.get_text(separator=' ')
        except ImportError:
            # Fallback si BeautifulSoup n'est pas là
            text = re.sub(r'<(script|style|header|footer|nav|aside).*?>.*?</\1>', '', html, flags=re.DOTALL | re.IGNORECASE)
            text = re.sub(r'<[^>]+>', ' ', text)
        
        # Nettoyage des espaces blancs et limitation de taille
        text = re.sub(r'\s+', ' ', text).strip()
        return text[:12000]
    def _clean(self, text: str) -> str:
        """Triple nettoyage : LaTeX → Markdown → sauts de ligne"""
        text = self._latex.latex_to_text(text)
        text = re.sub(r'[\*\#\_]', '', text)
        return text.replace('\n', ' ').strip()
        
        
    async def _task_music(self, target: str, nick: str, prompt: str):
        """Génération de musique réelle via Lyria 3 avec dossier dédié"""
        async with self._heavy_semaphore:
            self.privmsg(target, f"{nick}: Composition musicale (Lyria 3) en cours... 🎹")
            try:
                resp = await self._gemini.aio.models.generate_content(
                    model="lyria-3-pro-preview", 
                    contents=prompt,
                    config=genai_types.GenerateContentConfig(
                        response_modalities=["AUDIO", "TEXT"],
                    ),
                )
                
                # VERIFICATION : Si l'API ne renvoie pas de candidats (erreur ou blocage)
                if not resp or not resp.candidates:
                    self.privmsg(target, f"{nick}: ❌ L'API n'a renvoyé aucun résultat (possible filtrage ou erreur temporaire).")
                    return

                audio_bytes = None
                # On parcourt les parts du premier candidat
                for part in resp.candidates[0].content.parts:
                    if hasattr(part, 'inline_data') and part.inline_data:
                        audio_bytes = part.inline_data.data
                        break
                
                if audio_bytes:
                    name = f"snd_{uuid.uuid4().hex[:10]}.mp3"
                    local_path = os.path.join(self.audio_local_dir, name)
                    
                    with open(local_path, "wb") as f:
                        f.write(audio_bytes)
                    
                    url = f"{self.audio_public_url.rstrip('/')}/{name}"
                    self.privmsg(target, f"{nick}: ✅ Musique prête : {url}")
                else:
                    self.privmsg(target, f"{nick}: ❌ Aucun flux audio généré. Essaye d'être plus descriptif dans ton prompt.")
                    
            except Exception as e:
                logger.error(f"Music generation error: {e}")
                # Affiche une erreur plus propre sur IRC
                self.privmsg(target, f"{nick}: ❌ Erreur technique : {type(e).__name__}")

                
    async def _task_url(self, target: str, nick: str, url: str, prompt: str):
        """Explore une URL et l'analyse avec Gemini"""
        self.privmsg(target, f"{nick}: Exploration de la page en cours... 🌐")
        
        if "pastebin.com" in url and "/raw/" not in url:
            url = url.replace("pastebin.com/", "pastebin.com/raw/")

        headers = {
            "User-Agent": "Mozilla/5.0 (X11; Linux x86_64) AppleWebKit/537.36 (KHTML, like Gecko) Chrome/122.0.0.0 Safari/537.36"
        }

        try:
            async with httpx.AsyncClient(timeout=20.0, follow_redirects=True, headers=headers, verify=False) as client:
                resp = await client.get(url)
                resp.raise_for_status()
                
                # Correction ici : on s'assure que le nom correspond à la fonction définie plus bas
                if "text/plain" in resp.headers.get("Content-Type", "") or "pastebin" in url:
                    web_text = resp.text[:15000]
                else:
                    web_text = self._extract_text(resp.text) # <--- LE "T" EST ICI

            if not web_text or len(web_text) < 10:
                self.privmsg(target, f"{nick}: ❌ Contenu trop court ou illisible.")
                return

            instructions = (
                f"Contenu de la page {url} :\n\n{web_text}\n\n"
                f"Question de {nick} : {prompt if prompt else 'Fais un résumé court.'}"
            )

            key = (target, nick)
            ctx = self.user_contexts.setdefault(key, [])
            ctx.append({"role": "user", "text": instructions})

            response = await self._gemini.aio.models.generate_content(
                model=MODEL_CHAT,
                contents=_ctx_to_api(ctx[-3:]),
                config=genai_types.GenerateContentConfig(system_instruction=IRC_SYSTEM_PROMPT),
            )
            
            answer = self._clean(response.text)
            ctx.append({"role": "model", "text": answer})
            await self.send_chunks(target, f"{nick}: {answer}")

        except Exception as e:
            logger.error(f"URL ERROR: {e}")
            self.privmsg(target, f"{nick}: ❌ Erreur : {str(e)[:60]}")
            
            
            
            
    async def _task_vision(self, target: str, nick: str, url: str, prompt: str, ext: str):
        """Analyse d'image ou de vidéo (Version asynchrone sécurisée)"""
        try:
            if ext == "mp4":
                mime = "video/mp4"
            else:
                mime = "image/jpeg" if ext in ["jpg", "jpeg"] else f"image/{ext}"

            # Remplacement de requests par httpx (plus sûr pour un bot IRC)
            async with httpx.AsyncClient(timeout=30.0, verify=False) as client:
                resp_file = await client.get(url)
                resp_file.raise_for_status()
                file_data = resp_file.content
            
            parts = [
                genai_types.Part(text=prompt or "Décris ce média."),
                genai_types.Part(inline_data=genai_types.Blob(mime_type=mime, data=file_data)),
            ]
            
            resp = await self._gemini.aio.models.generate_content(
                model=MODEL_CHAT,
                contents=[genai_types.Content(role="user", parts=parts)],
                config=genai_types.GenerateContentConfig(system_instruction=IRC_SYSTEM_PROMPT),
            )
            await self.send_chunks(target, f"{nick}: {self._clean(resp.text)}")
        except Exception as e:
            logger.error(f"Vision error: {e}")
            self.privmsg(target, f"{nick}: ❌ Erreur d'analyse média (vérifie l'URL).")

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
                        prompt=f"{prompt}, 4s, vertical 9:16  ",
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
            m = re.search(r'(https?://\S+\.(?P<ext>png|jpg|jpeg|webp|mp4))', args, re.I)
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
        elif cmd == "url":
            m = re.search(r'(https?://\S+)', args)
            if m:
                url = m.group(1)
                user_query = args.replace(url, "").strip()
                asyncio.create_task(self._task_url(target, nick, url, user_query))
            else:
                self.privmsg(target, f"{nick}: Il me faut une URL valide (http/https).")
            return
        elif cmd == "video":
            if not args:
                self.privmsg(target, "Prompt vide !")
            else:
                asyncio.create_task(self._task_video(target, nick, args))
            return
        elif cmd == "music":
            if not args:
                self.privmsg(target, "Décris la musique que tu veux !")
            else:
                asyncio.create_task(self._task_music(target, nick, args))
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
        "audio_local_dir":  cfg.get("audio_local_dir", "/var/www/html/audio"),
        "audio_public_url": cfg.get("audio_public_url", "https://labynet.fr/audio"),
        "max_num_line":    cfg.get("max_num_line", 20),
        "flood_delay":     cfg.get("flood_delay", 0.5),
    })
    bot.include(__name__)
    bot.run()


if __name__ == "__main__":
    main()
