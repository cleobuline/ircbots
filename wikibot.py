import irc.bot
import wikipediaapi
from html.parser import HTMLParser

class HTMLTextExtractor(HTMLParser):
    def __init__(self):
        super().__init__()
        self.result = []
    
    def handle_data(self, data):
        self.result.append(data)

    def get_text(self):
        return ''.join(self.result)

class WikipediaBot(irc.bot.SingleServerIRCBot):
    def __init__(self, channel, nickname, server, port=6667):
        super().__init__([(server, port)], nickname, nickname)
        self.channel = channel
        self.nickname = nickname  # Store bot's nickname

        self.wikipedia = wikipediaapi.Wikipedia(
            language='fr',
            extract_format=wikipediaapi.ExtractFormat.HTML,
            user_agent='Mozilla/5.0 (Macintosh; Intel Mac OS X 10_15_7) AppleWebKit/537.36 (KHTML, like Gecko) Chrome/122.0.0.0 Safari/537.36'
        )

    def on_welcome(self, connection, event):
        connection.join(self.channel)

    def on_pubmsg(self, connection, event):
        message = event.arguments[0]
        prefix = ''

        # Check if message starts with "<nickname>: "
        if message.lower().startswith(f"{self.nickname.lower()}:"):
            query = message.split(f"{self.nickname}:", 1)[-1].strip()
            response = self.search_wikipedia(query)
            
            if response:
                # Replace newline characters and other problematic characters
                response = response.replace('\n', ' ').replace('\r', '')
                self.send_long_message(connection, prefix, response)

    def search_wikipedia(self, query):
        try:
            page = self.wikipedia.page(query)
            if page.exists():
                sections = page.sections
                if sections:
                    summary = self.clean_html(page.summary)
                    sections_text = "\n\n".join([f"{section.title}:\n{self.clean_html(section.text)}" if section.text else "" for section in sections[:3]])
                    full_response = f"{summary}\n\nSections:\n{sections_text}"
                else:
                    full_response = self.clean_html(page.summary)
                
                return full_response
            else:
                return f"Sorry, I couldn't find information on '{query}'."
        except wikipediaapi.exceptions.DisambiguationError as e:
            return f"Ambiguous search query: {e.options}"
        except wikipediaapi.exceptions.PageError:
            return f"Sorry, I couldn't find information on '{query}'."
        except wikipediaapi.exceptions.WikipediaException as e:
            return f"Error: {str(e)}"

    def clean_html(self, html_text):
        parser = HTMLTextExtractor()
        parser.feed(html_text)
        return parser.get_text()

    def send_long_message(self, connection, prefix, message):
        max_length = 400  # Maximum length per message
        if len(message) <= max_length:
            connection.privmsg(self.channel, f"{prefix}{message}")
        else:
            chunks = self.split_message(message, max_length)
            for chunk in chunks:
                connection.privmsg(self.channel, f"{prefix}{chunk}")

    def split_message(self, message, max_length):
        if len(message) <= max_length:
            return [message]
        
        chunks = []
        current_chunk = ""
        words = message.split()

        for word in words:
            if len(current_chunk) + len(word) <= max_length:
                if current_chunk:
                    current_chunk += ' ' + word
                else:
                    current_chunk = word
            else:
                chunks.append(current_chunk)
                current_chunk = word
        
        if current_chunk:
            chunks.append(current_chunk)

        return chunks

if __name__ == "__main__":
    bot = WikipediaBot("#labynet", "wikibot", "labynet.fr")
    bot.start()
