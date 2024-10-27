import irc.bot
import time
from youtube_dl import YoutubeDL
import requests
from bs4 import BeautifulSoup
import re
import urllib.parse

class YouTubeBot(irc.bot.SingleServerIRCBot):
    def __init__(self):
        self.api_key = "You tube API "  # Clé API YouTube
        self.weather_api_key = "Your API wetherAPI "  # Remplacez par votre clé API WeatherAPI
        self.news_api_key = "News API"  # Remplacez par votre clé API NewsAPI
        self.server = "labynet.fr" #choose your server to test or use this one 
        self.port = 6667
        self.channel = "#test"
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

    def on_disconnect(self, connection, event):
        print("Déconnecté du serveur, tentative de reconnexion...")
        while True:
            try:
                self.reconnect()
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

        except Exception as e:
            print(f"Error in on_pubmsg: {e}")
            connection.privmsg(self.channel, f"Une erreur est apparue. {str(e)}")

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
            'extract_flat': True,
            'default_search': 'auto',
            'youtube_include_dash_manifest': False,
            'youtube_api_key': self.api_key,
            'user_agent': self.user_agent
        }
        with YoutubeDL(ydl_opts) as ydl:
            try:
                info_dict = ydl.extract_info("ytsearch:" + query, download=False)
                if 'entries' in info_dict:
                    video_info = info_dict['entries'][0]
                    video_id = video_info['id']
                    complete_url = f"https://www.youtube.com/watch?v={video_id}"
                    connection.privmsg(self.channel, f"YouTube: {complete_url}")
                    title = video_info.get('title', None)
                    if title:
                        connection.privmsg(self.channel, f"Title: {title}")
                else:
                    connection.privmsg(self.channel, "Pas de vidéo correspondante.")
            except Exception as e:
                print(f"Error in search_youtube: {e}")
                connection.privmsg(self.channel, "Une erreur dans YouTube search.")

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

    def get_weather(self, location, connection):
        try:
            url = f"http://api.weatherapi.com/v1/current.json?key={self.weather_api_key}&q={urllib.parse.quote(location)}&lang=fr"
            response = requests.get(url)
            response.raise_for_status()
            weather_data = response.json()
            
            if "current" in weather_data:
                condition = weather_data["current"]["condition"]["text"]
                temp_c = weather_data["current"]["temp_c"]
                feelslike_c = weather_data["current"]["feelslike_c"]
                humidity = weather_data["current"]["humidity"]
                
                weather_message = (f"Météo à {location} : {condition}, "
                                   f"Température : {temp_c}°C, Ressenti : {feelslike_c}°C, "
                                   f"Humidité : {humidity}%")
                connection.privmsg(self.channel, weather_message)
            else:
                connection.privmsg(self.channel, "Impossible de récupérer les données météo pour cet emplacement.")
        except Exception as e:
            print(f"Error in get_weather: {e}")
            connection.privmsg(self.channel, "An error occurred while fetching the weather information.")

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
        streaming_services = ['.mp3', '.ogg', '.m3u', '.pls', 'icecast', 'shoutcast']
        return any(service in url for service in streaming_services)

if __name__ == "__main__":
    bot = YouTubeBot()
    bot.start()
