
import json
import irc.bot
import openai
import requests
import time
from pylatexenc.latex2text import LatexNodes2Text
from collections import defaultdict, deque
from pylatexenc.latex2text import LatexNodes2Text

class ChatGPTBot(irc.bot.SingleServerIRCBot):
    def __init__(self, config_file):
        # Charger la configuration
        with open(config_file, "r") as f:
            config = json.load(f)

        # Initialiser les variables
        self.server = config.get("server", "localhost")
        self.port = config.get("port", 6667)
        self.nickname = config.get("nickname", "ChatGPTBot")
        self.channel_list = [channel.strip() for channel in config["channels"].split(",")]
        self.api_key = config.get("api_key", "")  # Utiliser une variable d'environnement pour la clé API
        self.contexts = defaultdict(lambda: deque(maxlen=5))  # Contexte avec une taille maximale de 5 messages

        # Initialiser le client OpenAI
        openai.api_key = self.api_key

        # Appeler l'initialiseur de la classe parente
        super().__init__([(self.server, self.port)], self.nickname, self.nickname)

    def on_welcome(self, connection, event):
        try:
            for channel in self.channel_list:
                connection.join(channel)
                
        except Exception as e:
            print(f"Erreur lors de la connexion au canal: {e}")

    def on_privmsg(self, connection, event):
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
        try:
            user = event.source.nick
            message = event.arguments[0]
            

            # Ajouter le message de l'utilisateur au contexte
            self.contexts[user].append(f"{user}: {message}")

            # Construire le contexte pour l'API
            context = "\n".join(self.contexts[user])
            
            if message.strip().lower() == "!reset":
                self.reset_context(user)
                connection.privmsg(user, "Contexte réinitialisé.")
                return
            # Appeler l'API de ChatGPT avec le contexte
            response = openai.ChatCompletion.create(
                model="gpt-4o-mini",
                messages=[{"role": "user", "content": context}]
            )

            # Vérifier la réponse de l'API
            if response.choices and response.choices[0].message:
                bot_response = response.choices[0].message.content.strip()
                readable_text = LatexNodes2Text().latex_to_text(bot_response)
                self.contexts[user].append(f"Bot: {readable_text}")
                

                # Envoyer la réponse à l'utilisateur
                #connection.privmsg(user, bot_response)
                send_message_in_chunks(self, connection, user, readable_text)
                

        except Exception as e:
            print(f"Erreur lors du traitement du message: {e}")
            

    def reset_context(self, user):
        # Réinitialiser le contexte pour un utilisateur spécifique
        self.contexts[user].clear()

# Exemple d'utilisation
if __name__ == "__main__":
    bot = ChatGPTBot("config.json")
    try:
        bot.start()
    except KeyboardInterrupt:
        print("Bot arrêté par l'utilisateur")
