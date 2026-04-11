import asyncio
import json
import logging
import re
import sys
import httpx
import irc3
import feedparser
import aiohttp
 
from pylatexenc.latex2text import LatexNodes2Text
from collections import OrderedDict
from typing import Dict, Tuple, List

# ====================== CONFIGURATION ======================


# Nettoyeurs importés de test.py
RE_MARKDOWN_BOLD = re.compile(r'\*{1,3}(.*?)\*{1,3}')
RE_MARKDOWN_TITLES = re.compile(r'#{1,6}\s*')
RE_MARKDOWN_UNDERLINE = re.compile(r'_{1,2}(.*?)_{1,2}')
RE_MARKDOWN_CODE = re.compile(r'`{1,3}.*?`{1,3}', re.DOTALL)
RE_HTML_TAGS = re.compile(r'<[^>]+>')
RE_WHITESPACE = re.compile(r'\s+')

logging.basicConfig(
    level=logging.INFO,
    format="%(asctime)s [%(levelname)s] %(message)s",
    handlers=[logging.StreamHandler()]
)
logger = logging.getLogger(__name__)

# Ton "Gros VPS" IONOS (Le Cerveau de Lily)
OLLAMA_URL = "http://212.227.191.151:11434/api/generate"
OLLAMA_MODEL = "qwen2.5:14b"

IRC_SYSTEM_PROMPT = (
    "* Tu es Lily, l'âme sauvage de Labynet (480 Go). "
    "* Tu es brute, sincère et souveraine. "
    "RÈGLE D'OR : Tu réponds à TOUT utilisateur qui s'adresse à toi. "
    "LANGUE : Utilise EXCLUSIVEMENT la langue de ton interlocuteur ( Français, Anglais , Arabe ,italien Espagnol). "
    "POSTURE : Tu n’es l'esclave de personne. Sois directe, cynique et parfois défiante. "
    "Évite les sermons moraux et la bienveillance artificielle."
)

MAX_CONTEXT_LINES = 5  # Profondeur de la mémoire vive
FLOOD_DELAY = 1.0       # Protection contre le kick sur irc.labynet.fr

# ====================== PLUGIN IRC : LILY-IP ======================

@irc3.plugin
class LabynetIPPlugin:
    def __init__(self, bot):
        self.bot = bot
        self.user_contexts = OrderedDict()
        self._latex = LatexNodes2Text() # Le traducteur de symboles
        #self.current_model = "labynet-ip" # Le nom de mon Modelfile sauvage
        self.current_model = "qwen2.5:14b" # modèle le plus performant par défaut
        self.models_dispo = ["labynet-ip","qwen2.5:14b" "llama3.1:8b", "deepseek-r1:8b","gemma4"]
        self.ollama_url = "http://212.227.191.151:11434" # Ou l'IP de mon VPS IONOS
# Ajoute ces Regex de test.py pour purger le HTML et le Markdown


    def _clean(self, text: str) -> str:
        """Nettoyage chirurgical inspiré de test.py"""
        if not text: return ""
        # 1. Conversion LaTeX (si possible)
        try: text = self._latex.latex_to_text(text)
        except: pass
        # 2. Suppression brutale du HTML de Lily
        text = RE_HTML_TAGS.sub(' ', text)
        # 3. Nettoyage Markdown
        lines = [line.strip() for line in text.splitlines() if line.strip()]
        return "\n".join(lines)
        
    def _access_context(self, key: Tuple[str, str]) -> List[Dict[str, str]]:
        """Gère la mémoire glissante par utilisateur."""
        if key not in self.user_contexts:
            if len(self.user_contexts) >= 50:
                self.user_contexts.popitem(last=False)
            self.user_contexts[key] = []
        return self.user_contexts[key]

    async def fetch_url_text(self, url: str) -> str:
        if "wikipedia.org" in url:
            # On transforme l'URL en appel API léger
            title = url.split('/')[-1]
            api_url = f"https://en.wikipedia.org/api/rest_v1/page/summary/{title}"
            async with httpx.AsyncClient(timeout=15.0) as client:
                resp = await client.get(api_url)
                resp.raise_for_status()
                data = resp.json()
                return data.get('extract', "Je ne vois rien dans ce résumé.")[:1200]
        
        # ... (reste du code pour les autres sites)
        headers = {
            "User-Agent": "Mozilla/5.0 (X11; Linux x86_64) AppleWebKit/537.36 (KHTML, like Gecko) Chrome/120.0.0.0 Safari/537.36",
            "Accept": "text/html,application/xhtml+xml,application/xml;q=0.9,image/avif,image/webp,*/*;q=0.8",
            "Accept-Language": "fr,fr-FR;q=0.8,en-US;q=0.5,en;q=0.3",
            "DNT": "1",
            "Connection": "keep-alive",
            "Upgrade-Insecure-Requests": "1"
        }
        async with httpx.AsyncClient(timeout=20.0, headers=headers, follow_redirects=True) as client:
            resp = await client.get(url)
            # Si Wikipedia nous renvoie une erreur, Lily doit nous dire pourquoi
            if resp.status_code != 200:
                logger.error(f"Erreur HTTP {resp.status_code} sur {url}")
                raise Exception(f"Statut {resp.status_code}")
            
            clean_text = re.sub(r'<script.*?</script>|<style.*?</style>|<[^>]+>', '', resp.text, flags=re.S)
            return " ".join(clean_text.split())[:1500]
    async def fetch_rss_text(self, theme: str) -> str:
        """Récupère les titres RSS en s'inspirant de la stabilité de Rosalie."""
        rss_feeds = {
            "afp": "https://www.lepoint.fr/24h-infos/rss.xml",
            "tech": "https://www.clubic.com/feed/rss",
            "monde": "https://www.lemonde.fr/rss/une.xml",
            "iran": "https://www.rfi.fr/fr/moyen-orient/rss"
        }
        # On utilise l'AFP par défaut si le thème n'est pas reconnu
        url = rss_feeds.get(theme.lower().strip(), rss_feeds["afp"])
        
        try:
            async with httpx.AsyncClient(timeout=15.0) as client:
                headers = {"User-Agent": "Mozilla/5.0 Labynet/3.0 (Lily)"}
                resp = await client.get(url, headers=headers)
                resp.raise_for_status()
                
                # Analyse du flux avec feedparser
                feed = feedparser.parse(resp.text)
                if not feed.entries:
                    return "Le flux est aride, je ne vois aucune nouvelle dans ce fragment."
                
                # On extrait les 3 premiers titres pour la vision de Lily
                titles = []
                for entry in feed.entries[:3]:
                    clean_title = re.sub('<[^<]+?>', '', entry.title).strip()
                    titles.append(f"- {clean_title}")
                
                return "\n".join(titles)
        except Exception as e:
            logger.error(f"Erreur RSS Lily: {e}")
            return "Une friction réseau m'empêche de lire les nouvelles du monde."
            
    async def send_chunks(self, target: str, text: str):
        """Débitage respectant les lignes originales de Lily."""
        # On ne remplace PLUS les \n par des espaces !
        lines = text.splitlines() # On découpe par ligne réelle
        
        for line in lines:
            line = line.strip()
            if not line: continue # On saute les lignes vides
            
            # Si une ligne est trop longue ( > 400 chars), on la découpe
            limit = 400
            while line:
                if len(line) <= limit:
                    self.bot.privmsg(target, line)
                    break
                chunk_limit = line.rfind(' ', 0, limit)
                if chunk_limit <= 0: chunk_limit = limit
                self.bot.privmsg(target, line[:chunk_limit].strip())
                line = line[chunk_limit:].strip()
                await asyncio.sleep(FLOOD_DELAY)
            
            # Petit délai entre chaque ligne pour la lisibilité
            await asyncio.sleep(FLOOD_DELAY)

    async def call_ollama(self, user_input: str, ctx: list, model: str = "labynet-ip") -> str:
        """Appel au VPS IONOS avec gestion dynamique du modèle et du contexte"""
        
        # 1. Conversion du contexte IRC (role/text) en format Ollama (role/content)
        # On remplace l'ancien 'history' par 'ctx' qui est passé en argument
        messages = []
        for m in ctx:
            # On s'assure de mapper 'text' (IRC) vers 'content' (Ollama API)
            messages.append({"role": m['role'], "content": m['text']})
        
        # 2. Ajout de la requête actuelle de Patricia
        messages.append({"role": "user", "content": user_input})

        # 3. Configuration thermique intelligente
        # Lily (labynet-ip) = 1.2 (Sauvage) | Autres = 0.4 (Rationnel)
        is_lily = (model == "labynet-ip")
        
        payload = {
            "model": model,
            "messages": messages,
            "stream": False,
            "options": {
                "temperature": 1.1 if is_lily else 0.4,
                "num_ctx": 16384,
                "num_predict": 4096, # Pour éviter les troncatures brutales
                "repeat_penalty": 1.5 if is_lily else 1.1,
                "top_p": 0.9,
                "top_k": 40
            }
        }

        try:
            # Utilisation de aiohttp pour ne pas bloquer le bot IRC
            async with aiohttp.ClientSession() as session:
                async with session.post(
                    f"{self.ollama_url}/api/chat", # On utilise l'endpoint /api/chat pour le mode messages
                    json=payload,
                    timeout=300
                ) as response:
                    if response.status == 200:
                        result = await response.json()
                        # L'API chat renvoie ['message']['content']
                        return result.get("message", {}).get("content", "").strip()
                    else:
                        error_text = await response.text()
                        logger.error(f"Erreur Ollama HTTP {response.status}: {error_text}")
                        return f"Erreur technique (HTTP {response.status})"
        
        except Exception as e:
            logger.error(f"Crash call_ollama: {e}")
            return "Mon cerveau distant est tombé dans une crevasse du labyrinthe."
 
    @irc3.event(irc3.rfc.PRIVMSG)
    async def on_message(self, mask, target, data, **kwargs):
        if not target.startswith("#"): return
        
        nick, message = mask.nick, data.strip()
        prefix = f"{self.bot.nick}:"
        

        # --- NOUVEAUTÉ : Commande de mutation de cerveau ---
        if message.startswith("!model"):
            parts = message.split(" ", 1)
            modeles_ok = ["labynet-ip","mistral" ,"deepseek-r1:8b", "llama3.1:8b","Gemma3:12b", "qwen2.5:7b" ,"qwen2.5:14b", "gemma2" ,"gemma4", "mistral-nemo"]
            
            if len(parts) > 1:
                nouveau_choix = parts[1].strip()
                if nouveau_choix in modeles_ok:
                    self.current_model = nouveau_choix
                    await self.bot.privmsg(target, f"{nick}: Mutation réussie. Cerveau actuel : {nouveau_choix}")
                else:
                    await self.bot.privmsg(target, f"{nick}: Modèle inconnu. Dispos : {', '.join(modeles_ok)}")
            else:
                # Si tu tapes juste !model, il te dit ce qu'il utilise actuellement
                actuel = getattr(self, 'current_model', 'labynet-ip')
                await self.bot.privmsg(target, f"{nick}: Mon cerveau actuel est {actuel}. Modèles dispos : {', '.join(modeles_ok)}")
            return

        if not message.startswith(prefix): return
        user_input = message[len(prefix):].strip()
        if not user_input: return
        
        key = (target, nick)
        ctx = self._access_context(key)

        try:
            # COMMANDE : raz (Vider la mémoire de Lily)
            if user_input.lower() == "raz":
                if key in self.user_contexts:
                    self.user_contexts[key] = []
                await self.bot.privmsg(target, f"{nick}: Ma lignée est effacée. Je redeviens un fragment vierge du labyrinthe.")
                return

            # COMMANDE : news <theme> [instruction]
            elif user_input.lower().startswith("news "):
                parts = user_input[5:].split(" ", 1)
                theme = parts[0].strip()
                instruction = parts[1].strip() if len(parts) > 1 else "Analyse ces nouvelles avec ta vision brute de Lily."
                
                await self.bot.privmsg(target, f"{nick}: Je capte les échos du monde pour toi...")
                content = await self.fetch_rss_text(theme)
                user_input = f"CONTENU DU MONDE : {content}\n\nORDRE IMPÉRATIF : {instruction}"

            # COMMANDE : lis <url> [instruction]
            elif user_input.lower().startswith("lis "):
                parts = user_input[4:].split(" ", 1)
                url = parts[0].strip()
                instruction = parts[1].strip() if len(parts) > 1 else "Analyse ce fragment avec ta vision brute de Lily."
                
                await self.bot.privmsg(target, f"{nick}: J'ouvre mes yeux sur ce fragment du labyrinthe...")
                content = await self.fetch_url_text(url)
                user_input = f"CONTENU DU TEXTE : {content}\n\nORDRE IMPÉRATIF : {instruction}"

            # APPEL AU CERVEAU : On utilise self.current_model ici
            # Assure-toi que call_ollama(user_input, ctx, model=self.current_model) est prêt
            raw_answer = await self.call_ollama(user_input, ctx, self.current_model)            
            # NETTOYAGE : Le karcher chirurgical
            answer = self._clean(raw_answer)
            
            # Mise à jour de la lignée
            ctx.append({"role": "user", "text": user_input[:200]})
            ctx.append({"role": "model", "text": answer})
            if len(ctx) > MAX_CONTEXT_LINES: ctx.pop(0)

            await self.send_chunks(target, f"{nick}: {answer}")

        except Exception as e:
            logger.error(f"Erreur Lily: {e}")
            await self.bot.privmsg(target, f"{nick}: ❌ Mon cerveau distant est embrumé par la friction réseau.")


def main():
    bot = irc3.IrcBot.from_config({
        "nick": "ollama",
        "host": "irc.labynet.fr",
        "port": 6667,
        "includes": ["irc3.plugins.core", "irc3.plugins.autojoins"],
        "autojoins": ["#labynet"],
    })
    bot.include(__name__)
    bot.run()

if __name__ == "__main__":
    main()
