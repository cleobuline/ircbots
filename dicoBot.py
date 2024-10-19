import irc.bot
import requests
from bs4 import BeautifulSoup
import time  # Nécessaire pour la pause entre les envois de messages
import socket

class DicoBot(irc.bot.SingleServerIRCBot):
    def __init__(self, server, port, channel, nickname):
        self.server = server
        self.port = port
        self.channel = channel
        self.nickname = nickname
        super().__init__([(server, port)], nickname, nickname)
        self.reconnect_delay = 5  # Délai de reconnexion en secondes

    def on_welcome(self, connection, event):
        connection.join(self.channel)
        print(f"Rejoint le canal {self.channel}")

    def on_disconnect(self, connection, event):
        print("Déconnecté du serveur. Tentative de reconnexion dans 5 secondes...")
        time.sleep(self.reconnect_delay)
        self.reconnect()

    def on_pubmsg(self, connection, event):
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
        try:
            response = requests.get(url)
            response.raise_for_status()  # Vérifie si la requête a échoué
            
            soup = BeautifulSoup(response.text, 'html.parser')

            # Premier essai : Chercher la section des définitions
            definition_section = soup.find(class_='Definitions')
            if definition_section:
                definition_text = definition_section.get_text(separator=" ").strip()
                return definition_text
            
            # Si pas trouvé, essayer un autre sélecteur
            alternative_section = soup.find('ul', class_='LexicalCategory')
            if alternative_section:
                alternative_text = alternative_section.get_text(separator=" ").strip()
                return alternative_text

            # Ajouter une vérification supplémentaire pour une nouvelle structure potentielle
            new_definition_section = soup.find('section', class_='DivisionDefinition')
            if new_definition_section:
                new_definition_text = new_definition_section.get_text(separator=" ").strip()
                return new_definition_text

        except requests.exceptions.RequestException as e:
            print(f"Erreur lors de la requête : {e}")
        
        return None  # Si aucune définition n'est trouvée ou en cas d'erreur

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

def run_bot():
    server = "your irc serveur"  # Remplacez par votre serveur IRC
    port = 6667  # Port du serveur IRC
    channel = "#your channel"  # Remplacez par votre canal IRC
    nickname = "DicoBot"  # Nom du bot

    while True:
        try:
            print(f"Tentative de connexion à {server}:{port}...")
            bot = DicoBot(server, port, channel, nickname)
            bot.start()
        except (socket.error, irc.client.ServerConnectionError) as e:
            print(f"Erreur de connexion : {e}. Nouvelle tentative dans 5 secondes...")
            time.sleep(5)  # Attente avant la nouvelle tentative de connexion
        except Exception as e:
            print(f"Erreur inattendue : {e}. Nouvelle tentative dans 5 secondes...")
            time.sleep(5)

if __name__ == "__main__":
    run_bot()
