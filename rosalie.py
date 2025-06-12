import irc.bot
import time
from yt_dlp import YoutubeDL

import requests
from bs4 import BeautifulSoup
import re
import urllib.parse
import math 
from googletrans import Translator
from deep_translator import GoogleTranslator
from geopy.geocoders import Nominatim
from timezonefinder import TimezoneFinder
import pytz
import math
from datetime import datetime, timedelta
from geopy.geocoders import Nominatim
import random
import subprocess
import os 
import json 
from skyfield.api import load
from datetime import datetime
import pytz

class YouTubeBot(irc.bot.SingleServerIRCBot):
    def __init__(self):
        self.server = "irc.tchat.cafe"
        self.port = 6667
        self.channel = "#cafe"
        self.nickname = "rosalie"
        self.realname = "rosalie Bot"
        self.username = "rosalie"
        self.user_agent = 'Mozilla/5.0 (Windows NT 10.0; Win64; x64) AppleWebKit/537.36 (KHTML, like Gecko) Chrome/113.0.0.0 Safari/537.36'
        try:
            with open("rosalie.json", "r") as f:
                config = json.load(f)
                self.youtube_api_key = config.get("youtube_api_key", "")
                self.weather_api_key = config.get("weather_api_key", "")
                self.news_api_key = config.get("news_api_key", "")
                self.google_api_key = config.get("google_api_key", "")
                self.cx = config.get("cx", "")
        except Exception as e:
            print(f"Erreur lors du chargement de rosalie.json : {e}")
            self.api_key = ""
            self.weather_api_key = ""
            self.news_api_key = ""
            self.google_api_key = ""
            self.cx = ""
        # Appel au constructeur de la classe    parente avec tentative de reconnexion
        while True:
            try:
                irc.bot.SingleServerIRCBot.__init__(self, [(self.server, self.port)], self.nickname, self.realname)
                break  # Si la connexion réussit, on quitte la boucle
            except Exception as e:
                print(f"Erreur de connexion au serveur IRC: {e}. Nouvelle tentative dans 10 secondes.")
                time.sleep(10)

    def on_welcome(self, connection, event):
        connection.join(self.channel)
        print(f"Connecté au canal {self.channel}.")
    def on_error(self, connection, event):
        print(f"Erreur détectée : {event}. Tentative de reconnexion...")
        self.reconnect(connection)
    def get_moon_phas_old(self):
        # Cycle lunaire : 29.53 jours = 2953 centièmes
        now = datetime.utcnow()
        epoch = datetime(2000, 1, 6, 18)  # Référence astronomique pour la nouvelle lune
        days = (now - epoch).total_seconds() / 86400.0
        position = int((days * 100) % 2953)

        # Phase
        if position < 300:
            phase = 0; name = "Nouvelle lune"; icon = "🌑"; msg = "La lune se cache, parfait pour observer les étoiles !"
        elif position < 1000:
            phase = 1; name = "Croissant montant"; icon = "🌒"; msg = "Une jeune lune, un nouveau départ à Paris !"
        elif position < 1476:
            phase = 2; name = "Premier quartier"; icon = "🌓"; msg = "La lune guide vos soirées !"
        elif position < 1845:
            phase = 3; name = "Gibbeuse croissante"; icon = "🌔"; msg = "La lune grossit, rêvez grand ce soir !"
        elif position < 2214:
            phase = 4; name = "Pleine lune"; icon = "🌕"; msg = "Admirez-la sur les toits de Paris !"
        elif position < 2583:
            phase = 5; name = "Gibbeuse décroissante"; icon = "🌖"; msg = "La lune décroît, une nuit douce vous attend !"
        elif position < 2952:
            phase = 6; name = "Dernier quartier"; icon = "🌗"; msg = "Profitez de la sérénité !"
        else:
            phase = 7; name = "Croissant descendant"; icon = "🌘"; msg = "La lune s'efface, une nuit calme à venir !"

        # Illumination estimée (simplifiée)
        if position <= 2214:
            illumination = int(position * 100 / 2214)
        else:
            illumination = 100 - int((position - 2214) * 100 / 739)

        return f"{icon} {name} – {illumination}% illuminée. {msg}"
    def get_moon_phase_new(self):
        ts = load.timescale()
        t = ts.from_datetime(datetime.utcnow().replace(tzinfo=pytz.UTC))
        eph = load('de421.bsp')
        sun, moon, earth = eph['sun'], eph['moon'], eph['earth']
        e = earth.at(t)
        _, mlong, _ = e.observe(moon).apparent().ecliptic_latlon()
        _, slong, _ = e.observe(sun).apparent().ecliptic_latlon()
        phase_angle = (mlong.degrees - slong.degrees) % 360
        illumination = (1 - math.cos(math.radians(phase_angle))) / 2 * 100

        if phase_angle < 45:
            name = "Nouvelle lune"; icon = "🌑"
        elif phase_angle < 135:
            name = "Croissant montant"; icon = "🌒"
        elif phase_angle < 225:
            name = "Pleine lune"; icon = "🌕"
        else:
            name = "Croissant descendant"; icon = "🌘"

        return f"{icon} {name} – {int(illumination)}% illuminée."
 
 
    def get_moon_phase(self):
        ts = load.timescale()
        t = ts.from_datetime(datetime.utcnow().replace(tzinfo=pytz.UTC))
        eph = load('de421.bsp')
        sun, moon, earth = eph['sun'], eph['moon'], eph['earth']

        e = earth.at(t)
        _, mlong, _ = e.observe(moon).apparent().ecliptic_latlon()
        _, slong, _ = e.observe(sun).apparent().ecliptic_latlon()
        phase_angle = (mlong.degrees - slong.degrees) % 360

        illumination = (1 - math.cos(math.radians(phase_angle))) / 2 * 100

        if phase_angle < 10 or phase_angle > 350:
            name = "Nouvelle lune"; icon = "🌑"; msg = "La lune se cache, parfait pour observer les étoiles !"
        elif phase_angle < 80:
            name = "Croissant montant"; icon = "🌒"; msg = "Une jeune lune, un nouveau départ à Paris !"
        elif phase_angle < 100:
            name = "Premier quartier"; icon = "🌓"; msg = "La lune guide vos soirées !"
        elif phase_angle < 170:
            name = "Gibbeuse croissante"; icon = "🌔"; msg = "La lune grossit, rêvez grand ce soir !"
        elif phase_angle < 190:
            name = "Pleine lune"; icon = "🌕"; msg = "Admirez-la sur les toits de Paris !"
        elif phase_angle < 260:
            name = "Gibbeuse décroissante"; icon = "🌖"; msg = "La lune décroît, une nuit douce vous attend !"
        elif phase_angle < 280:
            name = "Dernier quartier"; icon = "🌗"; msg = "Profitez ⟷ de la sérénité !"
        else:
            name = "Croissant descendant"; icon = "🌘"; msg = "La lune s'efface, une nuit calme à venir !"

        return f"{icon} {name} – {int(illumination)}% illuminée. {msg}"
    def reconnect(self, connection):
        """Tentative de reconnexion au serveur IRC."""
        while True:
            try:
                connection.disconnect("Reconnexion en cours...")
                time.sleep(5)  # Attendre un peu avant de tenter une reconnexion
                connection.connect(self.server, self.port, self.nickname)
                print("Reconnexion réussie!")
                break  # Sortir de la boucle si la reconnexion réussit
            except Exception as e:
                print(f"Erreur lors de la reconnexion : {e}. Nouvelle tentative dans 10 secondes.")
                time.sleep(10)  # Attendre 10 secondes avant de tenter à nouveau




    def calculate_solar_time(self, location, connection):
        try:
            # Étape 1 : Obtenir les coordonnées géographiques
            geolocator = Nominatim(user_agent="solar_time")
            location_data = geolocator.geocode(location)

            if not location_data:
                connection.privmsg(self.channel, f"Impossible de trouver l'emplacement pour '{location}'.")
                return

            latitude, longitude = location_data.latitude, location_data.longitude

            # Étape 2 : Obtenir l'heure UTC actuelle
            utc_now = datetime.utcnow()

            # Étape 3 : Calculer la différence de longitude en heures
            time_difference = longitude / 15.0  # 15° par heure

            # Étape 4 : Ajouter l'équation du temps
            day_of_year = utc_now.timetuple().tm_yday
            b = 2 * math.pi * (day_of_year - 81) / 364
            equation_of_time = 9.87 * math.sin(2 * b) - 7.53 * math.cos(b) - 1.5 * math.sin(b)
            equation_of_time_in_hours = equation_of_time / 60.0  # Convertir en heures

            # Calculer l'heure solaire
            solar_time = utc_now + timedelta(hours=time_difference + equation_of_time_in_hours)

            # Formatage et affichage
            solar_time_str = solar_time.strftime('%Y-%m-%d %H:%M:%S')
            connection.privmsg(
                self.channel,
                f"L'heure solaire pour {location_data.address} est : {solar_time_str}."
            )
        except Exception as e:
            connection.privmsg(self.channel, f"Erreur lors du calcul de l'heure solaire : {e}")


    def get_timezone_from_city(self, location, connection):
        try:
            # Convertir la ville en coordonnées géographiques
            geolocator = Nominatim(user_agent="city_to_timezone")
            location_data = geolocator.geocode(location)

            if not location_data:
                connection.privmsg(self.channel, f"Impossible de trouver l'emplacement pour '{location}'.")
                return

            latitude, longitude = location_data.latitude, location_data.longitude

            # Trouver le fuseau horaire
            tf = TimezoneFinder()
            timezone_name = tf.timezone_at(lat=latitude, lng=longitude)

            if not timezone_name:
                connection.privmsg(self.channel, "Impossible de déterminer le fuseau horaire.")
                return

            # Obtenir l'heure locale
            tz = pytz.timezone(timezone_name)
            local_time = datetime.now(tz).strftime('%Y-%m-%d %H:%M:%S')

            connection.privmsg(
                self.channel,
                f"L'heure locale pour {location_data.address} est : {local_time} (Fuseau horaire : {timezone_name})"
             )
        except Exception as e:
            connection.privmsg(self.channel, f"Erreur lors de la récupération du fuseau horaire : {e}")
            
    def on_disconnect(self, connection, event):
        print("Déconnecté du serveur, tentative de reconnexion...")
        self.reconnect(connection)
    def verlan_message(self, message):
        """Transforme chaque mot du message en inversant ses deux moitiés."""
        def split_and_reverse(word):
            mid = len(word) // 2
            return word[mid:] + word[:mid]  # On inverse les deux moitiés

        words = message.split()
        transformed_words = [split_and_reverse(word) for word in words]
        return ' '.join(transformed_words)        
        
    def on_pubmsg(self, connection, event):
        try:
            message = event.arguments[0]
            if message.startswith('!dl'):
                url = message.split(" ", 1)[1]
                self.download_youtube_video(url, connection)
                return
            urls = self.extract_urls(message)
            for url in urls:
                if self.is_streaming_url(url):
                    continue
                if "youtube.com" in url or "youtu.be" in url:
                    video_info = self.get_youtube_video_info(url)
                    if video_info:
                        connection.privmsg(self.channel, video_info)
                else:
                    title = self.get_page_title(url)
                    
                    if(title):
                        #title = title.encode('latin1').decode('utf-8')
                        connection.privmsg(self.channel, f"Title: {title}")
            if message.strip() == "!moon":
                phase_info = self.get_moon_phase()
                connection.privmsg(self.channel, phase_info)

            if message.startswith('!joke'):
                try:
                    # Récupérer une blague via l'API
                    parts = message.split()
                    lang = parts[1] if len(parts) > 1 else 'fr'
                    if (lang =="zh"): lang ="zh-CN"
                    headers = {'Accept': 'application/json'}
                    response = requests.get("https://icanhazdadjoke.com/", headers=headers)
                    response.raise_for_status()
                    joke = response.json().get('joke', "Pas de blague disponible, désolé !")
        
                    # Traduire la blague en français
                    translator = Translator()
                    #translated_joke = translator.translate(joke, src='en', dest=lang).text
                    translated_joke = GoogleTranslator(source='en', target=lang).translate(joke)

                    # Envoyer la blague traduite
                    connection.privmsg(self.channel, translated_joke)
                except Exception as e:
                    connection.privmsg(self.channel, "Erreur lors de la récupération ou traduction d'une blague. 😢")

            if message.startswith('!yt'):
                query = message.split('!yt')[1].strip()
                self.search_youtube(query, connection)
            elif message.startswith('!verlan'):
                original_text = message.split('!verlan', 1)[1].strip()
                if not original_text:
                    connection.privmsg(self.channel, "Utilisation : !verlan <texte>")
                    return
            
                transformed_text = self.verlan_message(original_text)
                connection.privmsg(self.channel, transformed_text)
            elif message.startswith('!time'):
                location = message.split('!time')[1].strip()
                self.get_timezone_from_city(location, connection)
            elif message.startswith('!solar'):
                location = message.split('!solar')[1].strip()
                self.calculate_solar_time(location, connection)
            elif message.startswith('!insult'):
                parts = message.split()
                if len(parts) > 1:
                    target = parts[1]
                    self.insult_medievale(target, connection)
                else:
                    connection.privmsg(self.channel, "Usage: !insult <pseudo>")

            elif message.startswith('!w'):
                location = message.split('!w')[1].strip()
                self.get_weather(location, connection)
            elif message.startswith('!gg'):
                query = message.split('!gg')[1].strip()
                self.search_google(query, connection)
            elif message.startswith('!go'):
                query = message.split('!go')[1].strip()
                self.search_google_old(query, connection)
            elif message.startswith('!news'):
                query = message.split('!news')[1].strip() if len(message.split('!news')) > 1 else ""
                self.search_news(query, connection)
            elif message.startswith('!geo'):
                location = message.split('!geo')[1].strip()
                self.get_geo_location(location, connection)
            elif message.startswith('!help'):
                self.send_help_message(connection)
            elif message.startswith('!dico'):
                self.larousse( connection, event)
            elif message.startswith(('!vi','!ja','!fi','!it', '!de ','!fr','!ru', '!zh' ,'!ar ', '!es', '!en')):
                try:
                    #     Identifier la commande et la langue cible
                    command = message.split()[0]  # Récupère la commande (!fr, !ar, etc.)
                    target_lang = command[1:]  # Extrait la langue cible (fr, ar, es, en)
                    if (target_lang =="zh"): target_lang ="zh-CN"
                    # Extraire le texte à traduire
                    input_text = message.split(command, 1)[1].strip()
        
                    # Traduire en utilisant la détection automatique
                    translated_text = GoogleTranslator(source='auto', target=target_lang).translate(input_text)
        
                    #  Envoyer la traduction
                    connection.privmsg(self.channel, f"{translated_text}")
                except Exception as e:
                    connection.privmsg(self.channel, "Erreur lors de la traduction.")
                except Exception as e:
                    connection.privmsg(self.channel, "Erreur lors de la traduction.")
            elif message.startswith('!=') or message.startswith('!eval') or message.startswith('!calc'):
                if message.startswith('!='):
                    expression = message.split('!=', 1)[1].strip()
                elif message.startswith('!eval'):
                    expression = message.split('!eval', 1)[1].strip()
                elif message.startswith('!calc'):
                    expression = message.split('!calc', 1)[1].strip()

                self.evaluate_expression( expression,connection)
            elif message.startswith('!?'):
                connection.privmsg(self.channel, f"cos(x),sin(x),tan(x),acos(x),asin (x),atan(x),log(x),log10(x),log2(x),sqrt(x),exp(x),pow(x,y),fact(x),deg(x),rad(x),pi,e, tau,ceil(x),floor(x),fabs(x), gcd(x,y), isqrt(x), hypot(x,y) Norme euclidienne de (X, Y)")

        except Exception as e:
            print(f"Error in on_pubmsg: {e}")
            connection.privmsg(self.channel, f"Une erreur est apparue. {str(e)}")

    def get_youtube_video_info(self, url):
        """Récupère les informations d'une vidéo YouTube en utilisant l'API YouTube."""
        try:
            # Extraire l'ID de la vidéo depuis l'URL
            video_id = self.extract_youtube_id(url)
            if not video_id:
                return None

            # Appeler l'API YouTube Data v3
            api_url = f"https://www.googleapis.com/youtube/v3/videos?part=snippet,statistics,contentDetails&id={video_id}&key={self.youtube_api_key}"
            response = requests.get(api_url)
            response.raise_for_status()
            data = response.json()

            if "items" not in data or not data["items"]:
                return "Aucune information trouvée pour cette vidéo."

            video_data = data["items"][0]
            snippet = video_data["snippet"]
            statistics = video_data.get("statistics", {})
            content_details = video_data.get("contentDetails", {})

            # Récupérer les informations nécessaires
            title = snippet.get("title", "Titre inconnu")
            channel = snippet.get("channelTitle", "Chaîne inconnue")
            views = int(statistics.get("viewCount", 0))
            likes = int(statistics.get("likeCount", 0))
            comments = int(statistics.get("commentCount", 0))
            duration = self.parse_youtube_duration(content_details.get("duration", "PT0S"))
            publish_date = snippet.get("publishedAt", "Date inconnue").split("T")[0]

            # Construire le message
            return (
                f"🎥 Titre : {title} | 📺 Chaîne : {channel} | 👀 Vues : {views:,} | 👍 Likes : {likes:,} | "
                f"💬 Commentaires : {comments:,} | 🕒 Durée : {duration} | 📅 Publié le : {publish_date} | "
                #f"🔗 Lien : {url}"
            )
        except Exception as e:
            print(f"Erreur lors de la récupération des informations YouTube : {e}")
            return "Erreur lors de la récupération des informations vidéo."

    def extract_youtube_id(self, url):
        """Extrait l'ID de la vidéo YouTube à partir de l'URL."""
        patterns = [
            r"youtu\.be/([^?&]+)",
            r"youtube\.com/watch\?v=([^?&]+)",
            r"youtube\.com/embed/([^?&]+)"
        ]
        for pattern in patterns:
            match = re.search(pattern, url)
            if match:
                return match.group(1)
        return None
    def download_youtube_video(self, url, connection):
        try:
            # Définir le chemin de sortie pour la vidéo téléchargée
            output_path = "/var/www/html/videos/%(title)s.%(ext)s"
            # Télécharger la vidéo avec yt-dlp
            #command = ["yt-dlp", "-o", output_path, url]
            #command = ["yt-dlp", "-f", "mp4", "-o", output_path, url]  # Forcer le format mp4
            #command = ["yt-dlp", "-f", "bestvideo+bestaudio", "-o", output_path, url]  # Télécharger la meilleure vidéo et audio
            command = [
                "yt-dlp",
                "-f", "bestvideo+bestaudio/best",  # Télécharger la meilleure vidéo et audio disponibles
                "-o", output_path,  # Chemin de sortie
                "--merge-output-format", "mp4",  # Fusionner la vidéo et l'audio en mp4
                url
            ]
            subprocess.run(command, check=True)

            # Trouver le fichier téléchargé
            output_dir = "/var/www/html/videos"
            files = sorted(os.listdir(output_dir), key=lambda f: os.path.getctime(os.path.join(output_dir, f)), reverse=True)
            if files:
                filename = files[0]
                # Définir les bonnes permissions pour le fichier téléchargé
                os.chmod(os.path.join(output_dir, filename), 0o644)  # Lecture/écriture pour le propriétaire, lecture pour les autres

                # Encoder correctement le nom du fichier pour l'URL
                encoded_filename = urllib.parse.quote(filename)
                download_link = f"https://labynet.fr/videos/{encoded_filename}"
                # Envoyer le lien de téléchargement dans le canal IRC
                connection.privmsg(self.channel, f"Téléchargement terminé ! Voici le lien : {download_link}")
            else:
                connection.privmsg(self.channel, "Erreur : Impossible de trouver le fichier téléchargé.")

        except subprocess.CalledProcessError:
            connection.privmsg(self.channel, "Erreur lors du téléchargement de la vidéo.")

        except Exception as e:
            connection.privmsg(self.channel, f"Erreur inattendue : {str(e)}")

    def parse_youtube_duration(self, duration):
        """Convertit la durée ISO 8601 en un format lisible (hh:mm:ss)."""
        hours, minutes, seconds = 0, 0, 0
        match = re.match(r"PT(?:(\d+)H)?(?:(\d+)M)?(?:(\d+)S)?", duration)
        if match:
            hours = int(match.group(1) or 0)
            minutes = int(match.group(2) or 0)
            seconds = int(match.group(3) or 0)
        if hours > 0:
            return f"{hours}h {minutes}m {seconds}s"
        return f"{minutes}m {seconds}s"


    def insult_medievale(self, target, connection):
        insults = [
            "Que les poux et la gale soient tes seuls compagnons, vil faquin !",
            "Ton haleine empeste plus que l’arrière-train d’un dragon enragé !",
            "Tu es plus inutile qu’un troubadour sans luth !",
            "Que tes chausses se trouent et que ton potage soit toujours froid !",
            "Par saint Glinglin, t’es plus laid qu’un troll après une beuverie !",
            "Si la bêtise était un royaume, tu en serais le souverain incontesté !",
            "Tu sens plus mauvais qu’une fosse à purin un jour de grand vent !",
            "Que ton cheval te morde et que ton épée rouille avant l’aube !",
            "Même un gueux affamé ne volerait point ton pain !",
            "Tu es plus faible qu’un château en paille face au vent du nord !",
            "Que ta choppe soit toujours vide et ton vin tourné en vinaigre !",
            "Par les dieux, même un bouffon ferait un meilleur chevalier que toi !",
            "Ton épée est plus rouillée que l’armure d’un cadavre oublié !",
            "Que ton nom soit moqué dans toutes les tavernes du royaume !",
            "Ta cervelle est plus vide qu’un tonneau percé !",
            "Que ton destrier refuse à jamais de t’obéir, maudit palefrenier !",
            "Par saint Michel, ton courage rivalise avec celui d’un escargot enrhumé !",
            "Que ton armure rouille et que ton heaume t’étrangle, couard de bas étage !",
            "Même un moine cloîtré a plus d’audace que toi !",
            "Ton honneur est plus tâché que la nappe d’une auberge après une beuverie !",
            "Que le ciel s’abatte sur toi et que les corbeaux festoient sur tes restes !",
            "Que la peste emporte ta langue trop bien pendue, maraud !",
            "Ton esprit est plus confus que les comptes d’un tavernier ivre !",
            "Que tes bottes se transforment en sabots et que ton cheval te crache dessus !",
            "On parle de toi dans tout le royaume... mais seulement pour se moquer !",
            "Que les rats de la ville préfèrent ton logis à la fange des égouts !",
            "Si la laideur était une armure, tu serais invincible !",
            "Tu es aussi utile qu’un bouclier en beurre en pleine bataille !",
            "Même un âne en robe de mage paraîtrait plus intelligent que toi !",
            "On croirait que les dieux t'ont sculpté dans une motte de fumier !",
            "Que ton nom soit oublié et ton existence effacée des chroniques du royaume !",
            "Tu es plus traître qu’un félon à la solde d’un duc sans honneur !",
            "Ton odeur pourrait faire fuir une armée entière, même sans bataille !",
           "Si la bêtise était une quête, tu serais déjà chevalier légendaire !"
        ]


        insult = random.choice(insults)
        connection.privmsg(self.channel, f"{target}: {insult}")

    def evaluate_expression(self, expression, connection):
        try:
            # Ajouter un contexte mathématique sécurisé
            math_context = {
                "cos": math.cos,
                "sin": math.sin,
                "tan": math.tan,
                "acos": math.acos,
                "asin": math.asin,
                "atan": math.atan,
                "atan2": math.atan2,
                "log": math.log,
                "log10": math.log10,
                "log2": math.log2,
                "sqrt": math.sqrt,
                "exp": math.exp,
                "pow": math.pow,
                "fact": math.factorial,
                "deg": math.degrees,
                "rad": math.radians,
                "pi": math.pi,
                "e": math.e,
                "tau": math.tau,
                "ceil": math.ceil,
                "floor": math.floor,
                "fabs": math.fabs,
                "gcd": math.gcd,
                "isqrt": math.isqrt,
                "hypot": math.hypot,
                "copysign": math.copysign,
                "perc": lambda part, total: (part / total) * 100 if total != 0 else "Erreur : division par zéro",
            }

            # Remplacer ^ par ** pour les puissances
            expression = expression.replace('^', '**')

            # Évaluer l'expression en utilisant le contexte mathématique
            result = eval(expression, {"__builtins__": None}, math_context)
            self.send_message_in_chunks(connection, self.channel, f"Résultat : {result}")
            #connection.privmsg(self.channel, f"Résultat : {result}")
        except Exception as e:
            connection.privmsg(self.channel, f"Erreur lors de l'évaluation : {e}")

    def send_help_message(self, connection):
        help_text = (
            "Commandes disponibles : "
            "!yt <query> - Recherche YouTube video. --"
            "!w <location> - Météo d'un site --"
            "!gg <query> - Recherche Google --"
            "!news <query> - Dernières nouvelles --"
            "!geo <location> - Google Map"
            "!eval [Expression]] - Calcul "
            "!help - Aide"
            "!fr , !it ,!de ,!es - Traduction "
            "!time <location>   Heure locale"
            "!solar <location>   Heure au soleil"
        )
        connection.privmsg(self.channel, help_text)
    def get_geo_location(self, location, connection):
        try:
            location_encoded = urllib.parse.quote(location)
            maps_url = f"https://www.google.com/maps/search/?api=1&query={location_encoded}"
            connection.privmsg(self.channel, f"Google Maps  '{location}': {maps_url}")
        except Exception as e:
            print(f"Error in get_geo_location: {e}")
            connection.privmsg(self.channel, "Une erreur est survenue lors de la génération du lien Google Maps.")

 

    def search_youtube(self, query, connection):
        ydl_opts = {
            'format': 'bestaudio/best',  #  Prendre le meilleur format audio ou vidéo
            'quiet': True,
            'extract_flat': False,
            'default_search': 'auto',
            'youtube_include_dash_manifest': False,
            'http_headers': {'User-Agent': self.user_agent},
        }
    
        try:
            with YoutubeDL(ydl_opts) as ydl:
                # Rechercher la vidéo
                info_dict = ydl.extract_info(f"ytsearch:{query}", download=False)
            
                if not info_dict.get('entries'):
                    connection.privmsg(self.channel, "Aucune vidéo trouvée pour cette recherche.")
                    return

                video = info_dict['entries'][0]

                # Récupérer les informations pertinentes
                title = video.get('title', 'Titre inconnu')
                url = video.get('webpage_url', 'URL inconnue')
                uploader = video.get('uploader', 'Chaine inconnue')
                duration = video.get('duration', 0) or 0
                views = video.get('view_count', 0)
                upload_date = video.get('upload_date', 'Date inconnue')

                # Convertir la durée en format hh:mm:ss
                hours, remainder = divmod(duration, 3600)
                minutes, seconds = divmod(remainder, 60)
                formatted_duration = f"{hours}h {minutes}m {seconds}s" if hours > 0 else f"{minutes}m {seconds}s"

                # Formater la date de publication
                formatted_date = (f"{upload_date[:4]}-{upload_date[4:6]}-{upload_date[6:]}"
                                  if len(upload_date) == 8 else "Date inconnue")

                # Envoyer les informations au canal
                message = (f"Résultat YouTube : {title} | Par : {uploader} | Durée : {formatted_duration} | "
                           f"Vues : {views:,} | Publié le : {formatted_date} | Lien : {url}")
                connection.privmsg(self.channel, message)
            
        except Exception as e:
            import traceback
            traceback.print_exc()
            connection.privmsg(self.channel, f"Erreur lors de la recherche YouTube : {str(e)}")


    def search_google_old(self, query, connection):
        try:
            import time
            import random

            headers = {
                'User-Agent': 'Mozilla/5.0 (Windows NT 10.0; Win64; x64) AppleWebKit/537.36 (KHTML, like Gecko) Chrome/113.0.0.0 Safari/537.36'
            }
            url = f"https://www.google.fr/search?q={query}&hl=fr&gl=fr&lr=lang_fr"
            response = requests.get(url, headers=headers, timeout=10)

            if response.status_code != 200:
                connection.privmsg(self.channel, f"Failed to fetch results. HTTP Status: {response.status_code}")
                return

            soup = BeautifulSoup(response.text, 'html.parser')
            search_results = soup.find_all('div', class_='tF2Cxc')
        
            if not search_results:
                print(f"HTML Content: {response.text[:500]}")  # Afficher un extrait pour débug

                connection.privmsg(self.channel, "No results found. Google may have blocked the request.")
                return

            url_tag = search_results[0].find('a', href=True)
            if not url_tag:
                connection.privmsg(self.channel, "No URL found in the search results.")
                return

            actual_url = url_tag['href']
            connection.privmsg(self.channel, f"Google: {actual_url}")

            # Optionally get the title
            title = self.get_page_title(actual_url)
            
            if title:
                connection.privmsg(self.channel, f"Title: {title}")

            # Optionally get the description
            descriptions = soup.find_all('div', class_='VwiC3b')
            if descriptions:
                description = descriptions[0].text.strip()
                if description:
                    connection.privmsg(self.channel, f"Description: {description}")
                else:
                    connection.privmsg(self.channel, "Description not found.")
            else:
                connection.privmsg(self.channel, "No description available.")

            # Sleep to avoid being detected as a bot
            time.sleep(random.uniform(2, 5))

        except Exception as e:
            print(f"Error in search_google: {e}")
            connection.privmsg(self.channel, "An error occurred during the Google search.")

            
    def search_google (self, query, connection):
        try:
            api_key = self.google_api_key # Remplace par ta clé API
            cx = self.cx  # Remplace par ton ID moteur de recherche personnalisé
            url = f"https://www.googleapis.com/customsearch/v1?q={query}&key={self.google_api_key}&cx={self.cx}&hl=fr&lr=lang_fr"

            # Afficher l'URL pour déboguer
            # print(f"Requesting URL: {url}")

            response = requests.get(url)
            response.raise_for_status()

            search_results = response.json()

            if 'items' in search_results:
                first_result = search_results['items'][0]
                actual_url = first_result['link']
                title = first_result['title']
                description = first_result['snippet']

                connection.privmsg(self.channel, f"Google: {actual_url}")
                connection.privmsg(self.channel, f"Title: {title}")
                connection.privmsg(self.channel, f"Description: {description}")
            else:
                connection.privmsg(self.channel, "No results found.")
        except requests.exceptions.RequestException as e:
            print(f"Request Error: {e}")
            connection.privmsg(self.channel, "Error in accessing Google search. Please try again later.")
        except Exception as e:
            print(f"Error in search_google: {e}")
            connection.privmsg(self.channel, "An error occurred during the Google search.")

          


    def get_weather(self, location, connection):
       import requests

       api_key = self.weather_api_key # Remplace par ta clé API valide
       url = f"https://api.weatherapi.com/v1/current.json?key={api_key}&q={location}&lang=fr"

       try:
           response = requests.get(url)
           response.raise_for_status()
           data = response.json()

           city = data['location']['name']
           country = data['location']['country']
           temp = data['current']['temp_c']
           condition = data['current']['condition']['text']
           humidity = data['current']['humidity']
           wind = data['current']['wind_kph']

           weather_info = (
               f"Météo à {city}, {country} :\n"
               f"🌡️ Température : {temp}°C\n"
               f"☁️ Condition : {condition}\n"
               f"💧 Humidité : {humidity}%\n"
               f"💨 Vent : {wind} km/h"
           )
           sanitized_message = weather_info.replace("\n", " ")
           connection.privmsg(self.channel, sanitized_message)
       except requests.exceptions.RequestException as e:
           connection.privmsg(self.channel,f"Erreur lors de la récupération des données météo : {e}")
           
    def larousse(self, connection, event):
        message = event.arguments[0].strip()
        if message.startswith('!dico'):
            try:
                word = message.split(' ', 1)[1].strip().lower()  # Gestion des majuscules/minuscules
                definition = self.get_definition_wiktionnaire(word)
                if definition:
                    cleaned_definition = self.clean_message(f"Définition de {word}: {definition}")
                    self.send_message_in_chunks(connection, self.channel, cleaned_definition)
                else:
                    cleaned_message = self.clean_message(f"Aucune définition trouvée pour {word}. Peut-être une erreur de frappe ?")
                    self.send_message_in_chunks(connection, self.channel, cleaned_message)
            except IndexError:
                cleaned_message = self.clean_message("Usage: !dico <mot>")
                self.send_message_in_chunks(connection, self.channel, cleaned_message) 
    def get_definition_wiktionnaire(self, word):
        """Scraping de Wiktionnaire pour obtenir une définition propre d'un mot"""
        url = f"https://fr.wiktionary.org/wiki/{word}"
        response = requests.get(url)
    
        if response.status_code == 200:
             soup = BeautifulSoup(response.text, 'html.parser')

             # Chercher la première section de définitions, généralement sous forme de liste
             definition_section = soup.find('ol')
             if definition_section:
                # On récupère la première définition
                first_definition = definition_section.find('li')
                if first_definition:
                    # On extrait uniquement le texte de la définition sans les citations
                    definition_text = first_definition.get_text(separator=" ").strip()
                
                    # On élimine les parties indésirables comme les références et citations
                    cleaned_definition = re.sub(r'(\[.*?\]|\(.*?\))', '', definition_text)
                    return cleaned_definition
            
        return None
                         
    def clean_message(self, message):
        # Nettoie les retours à la ligne et les espaces inutiles
        return message.replace('\r', '').replace('\n', '').strip()
                   
    def get_definition_larousse(self, word):
        """Scraping de Larousse.fr pour obtenir la définition d'un mot"""
        url = f"https://www.larousse.fr/dictionnaires/francais/{word}"
        response = requests.get(url)
        
        if response.status_code == 200:
            soup = BeautifulSoup(response.text, 'html.parser')

            # Premier essai : Chercher la section des définitions
            definition_section = soup.find(class_='Definitions')
            if definition_section:
                definition_text = definition_section.get_text(separator=" ").strip()
                return definition_text
            
            # Si pas trouvé, essayer un autre sélecteur (parfois la structure peut varier)
            alternative_section = soup.find('ul', class_='LexicalCategory')
            if alternative_section:
                alternative_text = alternative_section.get_text(separator=" ").strip()
                return alternative_text
        return None    
               
    def search_news(self, query, connection):
        try:
            url = f"https://newsapi.org/v2/everything?q={urllib.parse.quote(query)}&apiKey={self.news_api_key}&language=fr"
            response = requests.get(url)
            response.raise_for_status()
            news_data = response.json()
            
            if "articles" in news_data and news_data["articles"]:
                for article in news_data["articles"][:3]:  # Limiter à 3 articles
                    title = article.get("title", "Pas de titre")
                    url = article.get("url", "")
                    connection.privmsg(self.channel, f"News: {title} - {url}")
                    time.sleep(2)
            else:
                connection.privmsg(self.channel, "Aucune nouvelle trouvée pour cette recherche.")
        except Exception as e:
            print(f"Error in search_news: {e}")
            connection.privmsg(self.channel, "Une erreur est survenue lors de la recherche d'actualités.")

    def extract_urls(self, message):
        return re.findall(r'http[s]?://(?:[a-zA-Z]|[0-9]|[$-_@.&+]|[!*\\(\\),]|(?:%[0-9a-fA-F][0-9a-fA-F]))+', message)
        #return re.findall(r'https?://[a-zA-Z0-9.-]+(?:/[a-zA-Z0-9._~:/?#[\]@!$&\'()*+,;=%-]*)?', message)
        #return re.findall(r'https?://(?:[a-zA-Z]|[0-9]|[$-_@.&+]|[!*\\(\\),]|(?:%[0-9a-fA-F])|[-])+', message)
        #return re.findall(r'https?://(?:[a-zA-Z0-9\-._~:/?#\[\]@!$&\'()*+,;=%]+)', message)

 
    def get_page_title(self, url):
        try:
            response = requests.get(url, headers={'User-Agent': self.user_agent}, timeout=2)
            response.raise_for_status()
            soup = BeautifulSoup(response.text, 'html.parser')
            title_tag = soup.find('title')
            return title_tag.string.strip() if title_tag else None
        except requests.exceptions.RequestException as req_err:
            print(f"Request error occurred while fetching page title: {req_err}")
            return None

 
    def get_youtube_title(self, url):
        try:
            video_id = self.extract_video_id(url)
            api_url = f"https://www.googleapis.com/youtube/v3/videos?part=snippet&id={video_id}&key={self.youtube_api_key}"
            response = requests.get(api_url)
            response.raise_for_status()
            data = response.json()
            
            if "items" in data and len(data["items"]) > 0:
                return data["items"][0]["snippet"]["title"]
            else:
                return "No title found"
        except requests.exceptions.RequestException as req_err:
            print(f"Request error occurred while fetching YouTube title: {req_err}")
            return None

    def extract_video_id(self, url):
        regex = r'(?:https?://)?(?:www\.)?(?:youtube\.com/(?:[^/]+/.*|(?:v|e(?:mbed)?)|.*[?&]v=)|youtu\.be/)([^&]{11})'
        match = re.search(regex, url)
        return match.group(1) if match else None

    def is_streaming_url(self, url):
        return any(ext in url for ext in ['.gif','.png','.jpeg','.jpg','.mp3', '.ogg', '.m3u', '.pls', 'icecast','stream' , 'shoutcast'])


    def send_message_in_chunks(self, connection, target, message):
        lines = message.split('\n')
        for line in lines:
            if len(line.encode('utf-8')) <= 392:
                connection.privmsg(target, line.strip())
            else:
                while line:
                    if len(line.encode('utf-8')) > 392:
                        last_space_index = line[:392].rfind(' ')
                        if last_space_index == -1:
                            connection.privmsg(target, line[:392].strip())
                            line = line[392:]
                        else:
                            connection.privmsg(target, line[:last_space_index].strip())
                            line = line[last_space_index:].strip()
                    else:
                        connection.privmsg(target, line.strip())
                        line = ''
                    time.sleep(2)
if __name__ == "__main__":
    bot = YouTubeBot()
    bot.start()
