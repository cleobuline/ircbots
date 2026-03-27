import irc3
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
from typing import Dict, Tuple, List, Optional
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
    "Tu réponds de façon chaleureuse mais concise aux usagers."
)

# Modèles Gemini
MODEL_CHAT  = "gemini-2.5-flash"
MODEL_IMAGE = "imagen-4.0-generate-001"
MODEL_VEO   = "veo-3.1-generate-preview"
MODEL_MUSIC = "lyria-3-pro-preview"

# Limites de sécurité
MAX_DOWNLOAD_BYTES = 10 * 1024 * 1024   # 10 Mo max pour vision/url
MAX_CONTEXT_KEYS   = 500                 # Max d'entrées dans user_contexts
RATE_LIMIT_SECONDS = 3.0                 # Cooldown par nick
URL_COOLDOWN_SECS  = 8.0                 # Cooldown spécifique aux requêtes URL (plus lourdes)


# ====================== UTILITAIRES ======================

def sanitize_nick(nick: str) -> str:
    """Retourne un nom de fichier sûr basé sur le nick IRC."""
    safe_base = re.sub(r'[^\w\-]', '_', nick)[:24]
    nick_hash = hashlib.md5(nick.encode('utf-8')).hexdigest()[:4]
    return f"{safe_base}_{nick_hash}"


def sanitize_title(title: str) -> str:
    """Sanitise le titre en autorisant les points pour la compatibilité."""
    # On ajoute \. dans la regex pour ne pas transformer les points en underscores
    safe = re.sub(r'[^\w\-\.]', '_', title)[:32]
    if not safe:
        safe = "default"
    return safe

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


# ====================== PLUGIN IRC ======================

@irc3.plugin
class ZozoPlugin:
    def __init__(self, bot):
        self.bot = bot
        cfg = bot.config

        self.gemini_api_key   = cfg.get("gemini_api_key")
        self.web_url_base     = cfg.get("display_url", "https://labynet.fr/images")
        self.video_public_url = cfg.get("video_public_url", "https://labynet.fr/videos")
        self.audio_public_url = cfg.get("audio_public_url", "https://labynet.fr/audio")
        self.audio_local_dir  = cfg.get("audio_local_dir", "/var/www/html/audio")
        self.image_local_dir  = cfg.get("image_local_dir", "/var/www/html/images")
        self.video_local_dir  = cfg.get("video_local_dir", "/var/www/html/videos")
        self.max_num_line     = int(cfg.get("max_num_line", 20))
        self.flood_delay      = float(cfg.get("flood_delay", 0.5))

        self._gemini = genai.Client(api_key=self.gemini_api_key)

        # Contexte sérialisable {"role": str, "text": str}
        self.user_contexts: Dict[Tuple[str, str], List[Dict[str, str]]] = {}
        self._context_locks: Dict[Tuple[str, str], asyncio.Lock] = {}
        self._heavy_semaphore = asyncio.Semaphore(1)
        self._latex = LatexNodes2Text()

        # Rate-limiting : nick -> timestamp de la dernière requête
        self._last_request: Dict[str, float] = {}

        os.makedirs(self.image_local_dir, exist_ok=True)
        os.makedirs(self.video_local_dir, exist_ok=True)
        os.makedirs(self.audio_local_dir, exist_ok=True)
        os.makedirs("conversations", exist_ok=True)

        logger.info("Zozo Gemini prêt.")

    # ====================== HELPERS ======================

    def _get_context_lock(self, key: Tuple[str, str]) -> asyncio.Lock:
        return self._context_locks.setdefault(key, asyncio.Lock())

    def _check_rate_limit(self, nick: str, cooldown: float = RATE_LIMIT_SECONDS) -> bool:
        """
        Retourne True si le nick est autorisé à envoyer une nouvelle requête.
        Retourne False si le cooldown n'est pas écoulé.
        Met à jour le timestamp si autorisé.
        """
        now = time.monotonic()
        last = self._last_request.get(nick, 0.0)
        if now - last < cooldown:
            return False
        self._last_request[nick] = now
        return True

    def _prune_contexts(self):
        """Purge les contextes les plus anciens si la limite est atteinte."""
        if len(self.user_contexts) > MAX_CONTEXT_KEYS:
            # Supprime les 50 premières clés (FIFO approximatif)
            keys_to_delete = list(self.user_contexts.keys())[:50]
            for k in keys_to_delete:
                self.user_contexts.pop(k, None)
                self._context_locks.pop(k, None)
            logger.info(f"Purge mémoire : {len(keys_to_delete)} contextes supprimés.")

    def privmsg(self, target: str, text: str):
        try:
            self.bot.privmsg(target, str(text)[:400])
        except Exception as e:
            logger.debug(f"privmsg échoué : {e}")

    async def send_chunks_old(self, target: str, text: str):
        """Envoie le texte par morceaux en coupant proprement aux espaces."""
        text = text.replace('\n', ' ').replace('\r', '')
        limit = 400

        while text:
            if len(text) <= limit:
                self.privmsg(target, text)
                break

            chunk_limit = text.rfind(' ', 0, limit)
            if chunk_limit <= 0:
                chunk_limit = limit

            chunk = text[:chunk_limit].strip()
            if chunk:
                self.privmsg(target, chunk)

            text = text[chunk_limit:].strip()
            await asyncio.sleep(self.flood_delay)
    async def send_chunks(self, target: str, text: str):
        """Envoie le texte ligne par ligne, avec découpage si nécessaire."""
        # On traite chaque ligne de la réponse séparément
        for line in text.split('\n'):
            line = line.strip()
            if not line:
                continue
            
            limit = 400
            while line:
                if len(line) <= limit:
                    self.privmsg(target, line)
                    break

                chunk_limit = line.rfind(' ', 0, limit)
                if chunk_limit <= 0:
                    chunk_limit = limit

                chunk = line[:chunk_limit].strip()
                if chunk:
                    self.privmsg(target, chunk)

                line = line[chunk_limit:].strip()
                await asyncio.sleep(self.flood_delay)
            
            # Petit délai entre les lignes pour la stabilité IRC
            await asyncio.sleep(self.flood_delay)
    def _extract_text(self, html: str) -> str:
        """Nettoie le HTML pour ne garder que le texte utile."""
        try:
            from bs4 import BeautifulSoup
            soup = BeautifulSoup(html, 'html.parser')
            for s in soup(["script", "style", "header", "footer", "nav", "aside", "form"]):
                s.decompose()
            text = soup.get_text(separator=' ')
        except ImportError:
            text = re.sub(
                r'<(script|style|header|footer|nav|aside).*?>.*?</\1>', '',
                html, flags=re.DOTALL | re.IGNORECASE
            )
            text = re.sub(r'<[^>]+>', ' ', text)

        text = re.sub(r'\s+', ' ', text).strip()
        return text[:12000]

    def _clean(self, text: str) -> str:
        """Nettoyage haute précision pour IRC : préserve la structure verticale."""
        # 1. Harmonisation des guillemets et apostrophes "intelligents"
        text = text.replace('“', '"').replace('”', '"').replace('‘', "'").replace('’', "'")
        
        # 2. Suppression des blocs de code Markdown (```...```) avant le LaTeX
        text = re.sub(r'```[\w]*\n?(.*?)```', r'\1', text, flags=re.DOTALL)
        
        # 3. Conversion LaTeX vers texte
        try:
            text = self._latex.latex_to_text(text)
        except Exception:
            pass
            
        # 4. Code inline `commande` → «commande»
        text = re.sub(r'`([^`]+)`', r'«\1»', text)
        
        # 5. Suppression du gras et de l'italique (*, **, ***)
        text = re.sub(r'\*{1,3}([^*]+)\*{1,3}', r'\1', text)
        text = re.sub(r'\*+', '', text)
        
        # 6. Suppression des symboles de titres (#)
        text = re.sub(r'^\s*#{1,6}\s+', '', text, flags=re.MULTILINE)
        
        # 7. Suppression des soulignés (__ ou _)
        text = re.sub(r'_{1,2}([^_]+)_{1,2}', r'\1', text)
        
        # 8. Conversion des liens Markdown [texte](url) → texte (url)
        text = re.sub(r'\[([^\]]+)\]\((https?://[^\)]+)\)', r'\1 (\2)', text)
        
        # 9. Harmonisation des listes (tiret simple)
        text = re.sub(r'^\s*[-*+]\s+', '- ', text, flags=re.MULTILINE)
        text = re.sub(r'^\s*\d+\.\s+', '- ', text, flags=re.MULTILINE)
        
        # 10. Nettoyage des retours chariot et espaces inutiles
        text = text.replace('\r', '')
        
        # 11. On réduit les espaces multiples (sans toucher aux sauts de ligne)
        text = re.sub(r'[ \t]{2,}', ' ', text)
        
        return text.strip()

    # ====================== TÂCHES ASYNC ======================

    async def _task_music(self, target: str, nick: str, prompt: str):
        """Génération de musique via Lyria 3."""
        async with self._heavy_semaphore:
            self.privmsg(target, f"{nick}: Composition musicale (Lyria 3) en cours... 🎹")
            t0 = time.monotonic()
            try:
                resp = await self._gemini.aio.models.generate_content(
                    model=MODEL_MUSIC,
                    contents=prompt,
                    config=genai_types.GenerateContentConfig(
                        response_modalities=["AUDIO", "TEXT"],
                    ),
                )

                if not resp or not resp.candidates:
                    self.privmsg(target, f"{nick}: ❌ Aucun résultat (filtrage ou erreur temporaire).")
                    return

                audio_bytes = None
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
                    elapsed = time.monotonic() - t0
                    logger.info(f"Music générée en {elapsed:.1f}s → {name}")
                    self.privmsg(target, f"{nick}: ✅ Musique prête : {url}")
                else:
                    self.privmsg(target, f"{nick}: ❌ Aucun flux audio généré. Sois plus descriptif.")

            except Exception as e:
                logger.error(f"Music error: {e}")
                self.privmsg(target, f"{nick}: ❌ Erreur technique : {type(e).__name__}")

    async def _task_url(self, target: str, nick: str, url: str, prompt: str):
        """Explore une URL et l'analyse avec Gemini. Protégée par semaphore."""
        async with self._heavy_semaphore:
            self.privmsg(target, f"{nick}: Exploration de la page en cours... 🌐")

            # Réécriture Pastebin vers raw
            if "pastebin.com" in url and "/raw/" not in url:
                url = url.replace("pastebin.com/", "pastebin.com/raw/")

            headers = {
                "User-Agent": (
                    "Mozilla/5.0 (X11; Linux x86_64) AppleWebKit/537.36 "
                    "(KHTML, like Gecko) Chrome/122.0.0.0 Safari/537.36"
                )
            }

            try:
                # FIX: verify=True (SSL activé), limite de taille en streaming
                async with httpx.AsyncClient(
                    timeout=20.0, follow_redirects=True, headers=headers
                ) as client:
                    async with client.stream("GET", url) as response:
                        response.raise_for_status()
                        chunks = []
                        total = 0
                        async for chunk in response.aiter_bytes(chunk_size=8192):
                            total += len(chunk)
                            if total > MAX_DOWNLOAD_BYTES:
                                self.privmsg(target, f"{nick}: ❌ Page trop lourde (> 10 Mo), abandon.")
                                return
                            chunks.append(chunk)
                        raw = b"".join(chunks).decode("utf-8", errors="replace")

                content_type = response.headers.get("Content-Type", "")
                if "text/plain" in content_type or "pastebin" in url:
                    web_text = raw[:15000]
                else:
                    web_text = self._extract_text(raw)

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

                response_ai = await self._gemini.aio.models.generate_content(
                    model=MODEL_CHAT,
                    contents=_ctx_to_api(ctx[-3:]),
                    config=genai_types.GenerateContentConfig(system_instruction=IRC_SYSTEM_PROMPT),
                )

                answer = self._clean(response_ai.text or "")
                ctx.append({"role": "model", "text": answer})
                await self.send_chunks(target, f"{nick}: {answer}")

            except httpx.HTTPStatusError as e:
                logger.error(f"URL HTTP error {e.response.status_code}: {url}")
                self.privmsg(target, f"{nick}: ❌ Erreur HTTP {e.response.status_code}.")
            except Exception as e:
                logger.error(f"URL error: {e}")
                self.privmsg(target, f"{nick}: ❌ Erreur : {str(e)[:60]}")

    async def _task_vision(self, target: str, nick: str, url: str, prompt: str, ext: str):
        """Analyse d'image ou de vidéo via Gemini avec limite de taille."""
        self.privmsg(target, f"{nick}: Analyse du média en cours... 🧐")
        try:
            if ext == "mp4":
                mime = "video/mp4"
            else:
                mime = "image/jpeg" if ext in ["jpg", "jpeg"] else f"image/{ext}"

            # FIX: verify=True, téléchargement en streaming avec limite
            async with httpx.AsyncClient(timeout=30.0) as client:
                async with client.stream("GET", url) as resp_file:
                    resp_file.raise_for_status()
                    chunks = []
                    total = 0
                    async for chunk in resp_file.aiter_bytes(chunk_size=8192):
                        total += len(chunk)
                        if total > MAX_DOWNLOAD_BYTES:
                            self.privmsg(target, f"{nick}: ❌ Fichier trop lourd (> 10 Mo).")
                            return
                        chunks.append(chunk)
                    file_data = b"".join(chunks)

            parts = [
                genai_types.Part(text=prompt or "Analyse ce média de façon détaillée en français."),
                genai_types.Part(inline_data=genai_types.Blob(mime_type=mime, data=file_data)),
            ]

            resp = await self._gemini.aio.models.generate_content(
                model=MODEL_CHAT,
                contents=[genai_types.Content(role="user", parts=parts)],
                config=genai_types.GenerateContentConfig(system_instruction=IRC_SYSTEM_PROMPT),
            )

            answer = self._clean(resp.text or "")
            await self.send_chunks(target, f"{nick}: {answer}")

        except Exception as e:
            logger.error(f"Vision error: {e}")
            self.privmsg(target, f"{nick}: ❌ Erreur analyse média (URL inaccessible ou format non supporté).")

    async def _task_video(self, target: str, nick: str, prompt: str):
        """Génération vidéo avec Veo 3.1."""
        async with self._heavy_semaphore:
            self.privmsg(target, f"{nick}: Génération vidéo Veo en cours... ⏳")
            t0 = time.monotonic()
            try:
                loop = asyncio.get_running_loop()

                op_name = await loop.run_in_executor(
                    None,
                    lambda: self._gemini.models.generate_videos(
                        model=MODEL_VEO,
                        prompt=f"{prompt}, 4s, vertical 9:16",
                        config=genai_types.GenerateVideosConfig(
                            duration_seconds=4, aspect_ratio="9:16"
                        ),
                    )
                )

                # Polling avec gestion d'erreur transiente
                deadline = loop.time() + 600
                while True:
                    if loop.time() > deadline:
                        self.privmsg(target, f"{nick}: ❌ Timeout Veo (10 min).")
                        return
                    await asyncio.sleep(15)
                    try:
                        operation = await loop.run_in_executor(
                            None, lambda: self._gemini.operations.get(op_name)
                        )
                    except Exception as poll_err:
                        logger.warning(f"Polling transitoire ignoré : {poll_err}")
                        continue
                    if operation.done:
                        break

                video_obj = operation.response.generated_videos[0].video
                vid_bytes = await self._gemini.aio.files.download(file=video_obj)
                name = f"vid_{uuid.uuid4().hex[:12]}.mp4"
                with open(os.path.join(self.video_local_dir, name), "wb") as f:
                    f.write(vid_bytes)
                elapsed = time.monotonic() - t0
                logger.info(f"Vidéo générée en {elapsed:.1f}s → {name}")
                self.privmsg(target, f"{nick}: ✅ {self.video_public_url.rstrip('/')}/{name}")

            except Exception as e:
                logger.error(f"Video error: {e}")
                self.privmsg(target, f"{nick}: ❌ Erreur vidéo : {type(e).__name__}")

    async def _task_image(self, target: str, nick: str, prompt: str):
        """Génération d'image avec Imagen 4."""
        async with self._heavy_semaphore:
            self.privmsg(target, f"{nick}: Génération d'image en cours... 🎨")
            t0 = time.monotonic()
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
                elapsed = time.monotonic() - t0
                logger.info(f"Image générée en {elapsed:.1f}s → {name}")
                self.privmsg(target, f"{nick}: ✅ {url}")
            except Exception as e:
                logger.error(f"Image error: {e}")
                self.privmsg(target, f"{nick}: ❌ Erreur image : {type(e).__name__}")

    # ====================== ROUTAGE IRC ======================

    @irc3.event(irc3.rfc.PRIVMSG)
    async def on_privmsg(self, mask, target, data, **kwargs):
        # Canaux uniquement
        if not target.startswith("#"):
            return

        nick, message = mask.nick, data.strip()

        # Doit commencer par "BotNick:"
        if not message.startswith(self.bot.nick + ":"):
            return

        cmd_raw = message[len(self.bot.nick) + 1:].strip()
        if not cmd_raw:
            return

        parts = cmd_raw.split(maxsplit=1)
        cmd  = parts[0].lower()
        args = parts[1].strip() if len(parts) > 1 else ""
        key  = (target, nick)

        # --- Rate-limiting global ---
        # Les commandes lourdes ont un cooldown plus long
        heavy_cmds = {"image", "video", "music", "url", "vision", "local", "imgbb"}
        cooldown = URL_COOLDOWN_SECS if cmd in heavy_cmds else RATE_LIMIT_SECONDS
        if not self._check_rate_limit(nick, cooldown):
            self.privmsg(target, f"{nick}: Doucement ! ⏳ Attends quelques secondes.")
            return

        # Purge périodique de la mémoire si trop de contextes
        self._prune_contexts()

        # ---- Commandes ----

        if cmd == "vision":
            m = re.search(r'(https?://\S+\.(?P<ext>png|jpg|jpeg|webp|mp4))', args, re.I)
            if m:
                url = m.group(1)
                ext = m.group('ext').lower()
                asyncio.create_task(
                    self._task_vision(target, nick, url, args.replace(url, "").strip(), ext)
                )
            else:
                self.privmsg(target, f"{nick}: Lien image/vidéo requis (png/jpg/webp/mp4).")
            return

        elif cmd in ("image", "local", "imgbb"):
            if not args:
                self.privmsg(target, f"{nick}: Prompt vide !")
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
                self.privmsg(target, f"{nick}: URL valide requise (http/https).")
            return

        elif cmd == "video":
            if not args:
                self.privmsg(target, f"{nick}: Prompt vide !")
            else:
                asyncio.create_task(self._task_video(target, nick, args))
            return

        elif cmd == "music":
            if not args:
                self.privmsg(target, f"{nick}: Décris la musique que tu veux !")
            else:
                asyncio.create_task(self._task_music(target, nick, args))
            return

        elif cmd == "raz":
            self.user_contexts[key] = []
            self.privmsg(target, f"{nick}: Mémoire effacée. ✅")
            return

        elif cmd == "save":
            if not args:
                self.privmsg(target, f"{nick}: Titre requis.")
                return
            # FIX: sanitisation du titre pour éviter l'injection de chemin
            safe_title = sanitize_title(args)
            path = os.path.join("conversations", f"{sanitize_nick(nick)}.{safe_title}.json")
            try:
                with open(path, "w", encoding="utf-8") as f:
                    json.dump(self.user_contexts.get(key, []), f, ensure_ascii=False, indent=2)
                self.privmsg(target, f"{nick}: 💾 Sauvé sous « {safe_title} ».")
            except Exception as e:
                logger.error(f"Save error: {e}")
                self.privmsg(target, f"{nick}: ❌ Erreur lors de la sauvegarde.")
            return

        elif cmd == "load":
            if not args:
                self.privmsg(target, f"{nick}: Titre requis.")
                return
            # FIX: sanitisation du titre
            safe_title = sanitize_title(args)
            path = os.path.join("conversations", f"{sanitize_nick(nick)}.{safe_title}.json")
            if os.path.exists(path):
                try:
                    with open(path, "r", encoding="utf-8") as f:
                        self.user_contexts[key] = json.load(f)
                    count = len(self.user_contexts[key])
                    self.privmsg(target, f"{nick}: 📂 Chargé « {safe_title} » ({count} messages).")
                except Exception as e:
                    logger.error(f"Load error: {e}")
                    self.privmsg(target, f"{nick}: ❌ Erreur lors du chargement.")
            else:
                self.privmsg(target, f"{nick}: Fichier inconnu (titre : {safe_title}).")
            return
        elif cmd == "delete":
            if not args:
                self.privmsg(target, f"{nick}: Titre requis pour la suppression.")
                return
            
            # Utilisation de la sanitisation pour la sécurité
            safe_title = sanitize_title(args)
            prefix = sanitize_nick(nick)
            path = os.path.join("conversations", f"{prefix}.{safe_title}.json")
            
            if os.path.exists(path):
                try:
                    os.remove(path)
                    logger.info(f"Fichier supprimé par {nick} : {path}")
                    self.privmsg(target, f"{nick}: 🗑️ Sauvegarde « {safe_title} » supprimée.")
                except Exception as e:
                    logger.error(f"Delete error for {nick}: {e}")
                    self.privmsg(target, f"{nick}: ❌ Erreur technique lors de la suppression.")
            else:
                self.privmsg(target, f"{nick}: Aucun fichier trouvé avec le titre « {safe_title} ».")
            return
        elif cmd == "list":
            # Nouvelle commande : liste les sauvegardes du nick
            prefix = sanitize_nick(nick)
            try:
                files = [
                    f.replace(f"{prefix}.", "").replace(".json", "")
                    for f in os.listdir("conversations")
                    if f.startswith(prefix) and f.endswith(".json")
                ]
                if files:
                    self.privmsg(target, f"{nick}: 📋 Sauvegardes : {', '.join(files)}")
                else:
                    self.privmsg(target, f"{nick}: Aucune sauvegarde trouvée.")
            except Exception as e:
                logger.error(f"List error: {e}")
                self.privmsg(target, f"{nick}: ❌ Erreur lors du listage.")
            return

        elif cmd == "help":
            self.privmsg(target,
                f"{nick}: Commandes : raz | save [titre] | load [titre] | list | "
                "image [prompt] | video [prompt] | music [prompt] | "
                "vision [url] [prompt] | url [lien] [question]"
            )
            return

        # ---- Chat textuel ----
        async with self._get_context_lock(key):
            ctx = self.user_contexts.setdefault(key, [])

            # Suggestion d'utiliser vision si l'utilisateur colle une image dans le chat
            if re.search(r'https?://\S+\.(png|jpg|jpeg|webp)', cmd_raw, re.I):
                self.privmsg(target, f"{nick}: Utilise la commande 'vision' pour analyser des images.")
                return

            ctx.append({"role": "user", "text": cmd_raw})
            # Fenêtre glissante
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
                answer = self._clean(resp.text or "")
                ctx.append({"role": "model", "text": answer})
                await self.send_chunks(target, f"{nick}: {answer}")

            except Exception as e:
                logger.error(f"Chat error: {e}")
                # On retire le message utilisateur si l'API a échoué
                if ctx and ctx[-1]["role"] == "user":
                    ctx.pop()
                self.privmsg(target, f"{nick}: ❌ Erreur chat : {type(e).__name__}")

    @irc3.event(irc3.rfc.JOIN)
    async def on_join(self, mask, channel, **kwargs):
        if mask.nick == self.bot.nick:
            logger.info(f"Rejoint {channel}")

    @irc3.event(irc3.rfc.KICK)
    async def on_kick(self, mask, channel, target, **kwargs):
        if target == self.bot.nick:
            logger.info(f"Expulsé de {channel}, tentative de retour dans 5s...")
            await asyncio.sleep(5)
            self.bot.join(channel)


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
