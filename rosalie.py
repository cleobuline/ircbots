import irc.bot
import time
from yt_dlp import YoutubeDL
import requests
from bs4 import BeautifulSoup
import re
import urllib.parse
import math 

class YouTubeBot(irc.bot.SingleServerIRCBot):
    def __init__(self):
        self.api_key = "YOUR YOUTUBE API"  # Clé API YouTube
        self.weather_api_key = "YOUR WEATHER API"  # Remplacez par votre clé API WeatherAPI
        self.news_api_key = "YOUR NEWS API"  # Remplacez par votre clé API NewsAPI
        self.server = "labynet.fr"
        self.port = 6667
        self.channel = "#labynet"
        self.nickname = "rosalie"
        self.realname = "rosalie Bot"
        self.username = "rosalie"
        self.user_agent = "Mozilla/5.0 (Macintosh; Intel Mac OS X 10_15_7) AppleWebKit/537.36 (KHTML, like Gecko) Chrome/122.0.0.0 Safari/537.36"

        # Appel au constructeur de la classe parente avec tentative de reconnexion
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
        """
        Cette méthode est appelée lors d'une erreur de connexion (ping timeout, read error, etc.).
        Elle va tenter de reconnecter le bot automatiquement.
        """
        print(f"Erreur détectée: {event}. Tentative de reconnexion...")
        self.reconnect(connection)
    def reconnect(self, connection):
        """Tentative de reconnexion au serveur IRC."""
        try:
            connection.disconnect("Reconnexion en cours...")
            time.sleep(5)  # Attendre un peu avant de tenter une reconnexion
            connection.connect(self.server, self.port, self.nickname)
            print("Reconnexion réussie!")
        except Exception as e:
            print(f"Erreur lors de la reconnexion : {e}. Nouvelle tentative dans 10 secondes.")
            time.sleep(10)  # Attendre 10 secondes avant de tenter à nouveau

    def on_disconnect(self, connection, event):
        print("Déconnecté du serveur, tentative de reconnexion...")
        try:
            connection.disconnect("Reconnexion en cours...")
        except Exception as e:
            print(f"Erreur lors de la tentative de déconnexion propre : {e}")
    
        while True:
            try:
                self.connection.connect(self.server, self.port, self.nickname)
                print("Reconnexion réussie!")
                break
            except Exception as e:
                print(f"Erreur lors de la tentative de reconnexion : {e}. Nouvelle tentative dans 10 secondes.")
                time.sleep(10)

    def on_pubmsg(self, connection, event):
        try:
            message = event.arguments[0]
            urls = self.extract_urls(message)
            for url in urls:
                if self.is_streaming_url(url):
                    continue
                
                if "youtube.com" in url or "youtu.be" in url or "music.youtube.com" in url:
                    title = self.get_youtube_title(url)
                else:
                    title = self.get_page_title(url)
                
                if title:
                    connection.privmsg(self.channel, f"Title: {title}")

            if message.startswith('!yt'):
                query = message.split('!yt')[1].strip()
                self.search_youtube(query, connection)
            elif message.startswith('!w'):
                location = message.split('!w')[1].strip()
                self.get_weather(location, connection)
            elif message.startswith('!gg'):
                query = message.split('!gg')[1].strip()
                self.search_google(query, connection)
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
            expression =expression.replace('x', '*')
            # Évaluer l'expression en utilisant le contexte mathématique
            result = eval(expression, {"__builtins__": None}, math_context)
            connection.privmsg(self.channel, f"Résultat : {result}")
        except Exception as e:
            connection.privmsg(self.channel, f"Erreur lors de l'évaluation : {e}")

    def send_help_message(self, connection):
        help_text = (
            "Commandes disponibles : "
            "!yt <query> - Recherche YouTube video. --"
            "!w <location> - Météo d'un site --"
            "!gg <query> - Recherche Google --"
            "!news <query> - Dernières nouvelles --"
            "!geo <location≥ - Google Map"
            "!help - Aide"
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
            'format': 'best',
            'quiet': True,
            'extract_flat': 0,
            'default_search': 'auto',
            'youtube_include_dash_manifest': False,
            'youtube_api_key': self.api_key,
            'user_agent': self.user_agent
        }
        with YoutubeDL(ydl_opts) as ydl:
            try:
                # Rechercher la vidéo
                info_dict = ydl.extract_info("ytsearch:" + query, download=False)
                video = info_dict['entries'][0]

                # Récupérer les informations pertinentes
                title = video.get('title', 'Titre inconnu')
                url = video.get('webpage_url', 'URL inconnue')
                uploader = video.get('uploader', 'Chaine inconnue')
                duration = video.get('duration', 0)  # Durée en secondes
                views = video.get('view_count', 0)  # Nombre de vues
                upload_date = video.get('upload_date', 'Date inconnue')

                # Convertir la durée en format hh:mm:ss
                hours, remainder = divmod(duration, 3600)
                minutes, seconds = divmod(remainder, 60)
                formatted_duration = f"{hours}h {minutes}m {seconds}s" if hours > 0 else f"{minutes}m {seconds}s"

                # Formater la date de publication
                formatted_date = f"{upload_date[:4]}-{upload_date[4:6]}-{upload_date[6:]}" if upload_date != 'Date inconnue' else 'Date inconnue'

                # Envoyer les informations au canal
                message = (f"Résultat YouTube : {title} | Par : {uploader} | Durée : {formatted_duration} | "
                           f"Vues : {views:,} | Publié le : {formatted_date} | Lien : {url}")
                connection.privmsg(self.channel, message)
            except Exception as e:
                print(f"Erreur dans search_youtube : {e}")
                connection.privmsg(self.channel, "Erreur lors de la recherche YouTube.")


    def search_google(self, query, connection):
        try:
            headers = {'User-Agent': self.user_agent}
            url = f"https://www.google.fr/search?q={query}&hl=fr&gl=fr&lr=lang_fr"
            response = requests.get(url, headers=headers)
            response.raise_for_status()
            soup = BeautifulSoup(response.text, 'html.parser')
            search_results = soup.find_all('div', class_='tF2Cxc')
            descriptions = soup.find_all('div', class_='VwiC3b')

            if search_results:
                url_tag = search_results[0].find('a', href=True)
                if url_tag:
                    actual_url = url_tag['href']
                    connection.privmsg(self.channel, f"Google: {actual_url}")

                    title = self.get_page_title(actual_url)
                    if title:
                        connection.privmsg(self.channel, f"Title: {title}")

                    if descriptions:
                        description = descriptions[0].text.strip()
                        if description:
                            connection.privmsg(self.channel, f"Description: {description}")
                        else:
                            connection.privmsg(self.channel, "Description not found.")
                    else:
                        connection.privmsg(self.channel, "No description available.")
                else:
                    connection.privmsg(self.channel, "No URL found in the search results.")
            else:
                connection.privmsg(self.channel, "No corresponding URL found on Google.")
        except Exception as e:
            print(f"Error in search_google: {e}")
            connection.privmsg(self.channel, "An error occurred during the Google search.")
            
    def search_google_new (self, query, connection):
        try:
            api_key = 'YOUR GOOGLE SEARCH API'  # Remplace par ta clé API
            cx = 'YOUR ID SEARCH ENGINE'  # Remplace par ton ID moteur de recherche personnalisé
            url = f"https://www.googleapis.com/customsearch/v1?q={query}&key={api_key}&cx={cx}&hl=fr&lr=lang_fr"

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
                definition = self.get_definition_larousse(word)
                if definition:
                    cleaned_definition = self.clean_message(f"Définition de {word}: {definition}")
                    self.send_message_in_chunks(connection, self.channel, cleaned_definition)
                else:
                    cleaned_message = self.clean_message(f"Aucune définition trouvée pour {word}. Peut-être une erreur de frappe ?")
                    self.send_message_in_chunks(connection, self.channel, cleaned_message)
            except IndexError:
                cleaned_message = self.clean_message("Usage: !dico <mot>")
                self.send_message_in_chunks(connection, self.channel, cleaned_message) 
                          
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
            else:
                connection.privmsg(self.channel, "Aucune nouvelle trouvée pour cette recherche.")
        except Exception as e:
            print(f"Error in search_news: {e}")
            connection.privmsg(self.channel, "Une erreur est survenue lors de la recherche d'actualités.")

    def extract_urls(self, message):
        return re.findall(r'http[s]?://(?:[a-zA-Z]|[0-9]|[$-_@.&+]|[!*\\(\\),]|(?:%[0-9a-fA-F][0-9a-fA-F]))+', message)

    def get_page_title(self, url):
        try:
            response = requests.get(url, headers={'User-Agent': self.user_agent})
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
            api_url = f"https://www.googleapis.com/youtube/v3/videos?part=snippet&id={video_id}&key={self.api_key}"
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
        return any(ext in url for ext in ['.mp3', '.ogg', '.m3u', '.pls', 'icecast','stream' , 'shoutcast'])


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
                    time.sleep(0.5)
if __name__ == "__main__":
    bot = YouTubeBot()
    bot.start()
