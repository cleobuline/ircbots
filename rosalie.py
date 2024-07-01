import irc.bot
from youtube_dl import YoutubeDL
import requests
from bs4 import BeautifulSoup
import re
import urllib.parse

class YouTubeBot(irc.bot.SingleServerIRCBot):
    def __init__(self):
        # YouTube API key and IRC server details
        self.api_key = "YOUR_YOUTUBE_API_KEY"
        self.server = "labynet.fr"  # set your preferred IRC server
        self.port = 6667
        self.channel = "#labynet"  # set your preferred channel
        self.nickname = "rosalie"  # set your preferred nick name
        self.realname = "rosalie Bot"
        self.username = "rosalie"
        self.user_agent = "Mozilla/5.0 (Macintosh; Intel Mac OS X 10_15_7) AppleWebKit/537.36 (KHTML, like Gecko) Chrome/122.0.0.0 Safari/537.36"

        irc.bot.SingleServerIRCBot.__init__(self, [(self.server, self.port)], self.nickname, self.realname)

    def on_welcome(self, connection, event):
        # Join the specified channel upon successfully connecting to the IRC server
        connection.join(self.channel)

    def on_pubmsg(self, connection, event):
        try:
            # Extract the message content
            message = event.arguments[0]

            # Check for URLs in the message and get their titles
            urls = self.extract_urls(message)
            for url in urls:
                if self.is_streaming_url(url):
                    connection.privmsg(self.channel, f"Skipping streaming URL: {url}")
                    continue

                title = self.get_page_title(url)
                if title:
                    connection.privmsg(self.channel, f"Title: {title}")
                else:
                    connection.privmsg(self.channel, f"Could not retrieve title for: {url}")

            # Respond to messages starting with !yt
            if message.startswith('!yt'):
                query = message.split('!yt')[1].strip()
                self.search_youtube(query, connection)

            # Respond to messages starting with !gg
            elif message.startswith('!gg'):
                query = message.split('!gg')[1].strip()
                self.search_google(query, connection)
        except Exception as e:
            print(f"Error in on_pubmsg: {e}")
            connection.privmsg(self.channel, "An unexpected error occurred. Please try again.")

    def search_youtube(self, query, connection):
        # Options for youtube_dl to search YouTube
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
                # Extract information from YouTube based on the query
                info_dict = ydl.extract_info("ytsearch:" + query, download=False)
                if 'entries' in info_dict:
                    # Retrieve the ID and title of the first matching video
                    video_info = info_dict['entries'][0]
                    video_id = video_info['id']
                    complete_url = f"https://www.youtube.com/watch?v={video_id}"
                    connection.privmsg(self.channel, f"YouTube: {complete_url}")
                    title = video_info.get('title', None)
                    if title:
                        connection.privmsg(self.channel, f"Title: {title}")
                else:
                    connection.privmsg(self.channel, "No corresponding video found.")
            except Exception as e:
                print(f"Error in search_youtube: {e}")
                connection.privmsg(self.channel, "An error occurred during the YouTube search.")

    def search_google(self, query, connection):
        try:
            # Search Google and retrieve the first URL
            headers = {'User-Agent': self.user_agent}
            url = f"https://www.google.com/search?q={query}"
            response = requests.get(url, headers=headers)
            response.raise_for_status()
            soup = BeautifulSoup(response.text, 'html.parser')
            # Find the first non-Google link in the search results
            search_results = soup.find_all('div', class_='tF2Cxc')
            descriptions = soup.find_all('div', class_='VwiC3b')
            if search_results:
                # Extract the URL from the first search result
                url_tag = search_results[0].find('a', href=True)
                if url_tag:
                    actual_url = url_tag['href']
                    connection.privmsg(self.channel, f"Google: {actual_url}")
                    title = self.get_page_title(actual_url)
                    if descriptions and descriptions[0]:
                        connection.privmsg(self.channel, f"Description: {descriptions[0].text}")
                    if title:
                        connection.privmsg(self.channel, f"Title: {title}")
            else:
                connection.privmsg(self.channel, "No corresponding URL found on Google.")
        except Exception as e:
            print(f"Error in search_google: {e}")
            connection.privmsg(self.channel, "An error occurred during the Google search.")

    def extract_urls(self, message):
        # Extract URLs from the message
        return re.findall(r'http[s]?://(?:[a-zA-Z]|[0-9]|[$-_@.&+]|[!*\\(\\),]|(?:%[0-9a-fA-F][0-9a-fA-F]))+', message)

    def get_page_title(self, url):
        # Retrieve the title of the webpage with timeout and error handling
        try:
            response = requests.get(url, headers={'User-Agent': self.user_agent}, timeout=10)
            response.raise_for_status()  # Raise an exception for HTTP errors
            response.encoding = 'utf-8'
            
            # Check content type to handle non-HTML responses
            content_type = response.headers.get('Content-Type', '').lower()
            if 'text/html' in content_type:
                soup = BeautifulSoup(response.text, 'html.parser')
                return soup.title.string if soup.title else 'No title found'
            else:
                print(f"Skipping non-HTML content for URL: {url}")
                return None
        except requests.exceptions.HTTPError as http_err:
            print(f"HTTP error occurred: {http_err}")  # HTTP error
            return None
        except requests.exceptions.Timeout as timeout_err:
            print(f"Timeout error occurred: {timeout_err}")  # Timeout error
            return None
        except requests.exceptions.RequestException as req_err:
            print(f"Request error occurred: {req_err}")  # Other requests errors
            return None
        except Exception as e:
            print(f"An error occurred in get_page_title: {e}")  # Other errors
            return None

    def is_streaming_url(self, url):
        # Check if the URL contains "stream" in its path
        parsed_url = urllib.parse.urlparse(url)
        return 'stream' in parsed_url.path.lower()

if __name__ == "__main__":
    # Create an instance of the YouTubeBot and start it
    bot = YouTubeBot()
    bot.start()
