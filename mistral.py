import irc.bot
import json
import os
import time
import threading
from mistralai import Mistral

class ChatGPTBot(irc.bot.SingleServerIRCBot):
    def __init__(self, config_file):
        # Charger la configuration
        with open(config_file, "r") as f:
            config = json.load(f)

        # Initialiser les variables
        self.channel_list = [channel.strip() for channel in config["channels"].split(",")]
        self.nickname = config["nickname"]
        self.server = config["server"]
        self.port = config["port"]
        self.api_key = config["api_key"]
        self.max_num_line = config["max_num_line"]
        self.admin_user = config["admin_user"]
        self.blocked_users = set()
        self.user_contexts = {}
        self.model = "mistral-small"
        self.api_lock = threading.Lock()
        self.last_request_time = 0  # Dernier timestamp de requête

        # Vérifier si l'API key est définie
        if not self.api_key:
            raise ValueError("MISTRAL_API_KEY environment variable is not set")

        # Initialiser le client Mistral
        self.client = Mistral(api_key=self.api_key)

        # Initialiser le bot IRC
        irc.bot.SingleServerIRCBot.__init__(self, [(self.server, self.port)], self.nickname, self.nickname)

    def on_welcome(self, connection, event):
        for channel in self.channel_list:
            connection.join(channel)

    def on_pubmsg(self, connection, event):
        message = event.arguments[0]
        user = event.source.nick
        channel = event.target
        bot_nickname = self.connection.get_nickname()

        if message.strip().startswith(bot_nickname + ":"):
            message = message[len(bot_nickname) + 1:].strip()
            command, args = self.parse_command(message)

            if command == "help":
                self.send_help_message(connection, channel)
            elif command == "raz":
                self.reset_user_context(channel, user)
                connection.privmsg(channel, "Conversation oubliée ...")
            elif command == "vision":
                self.generate_image_description(connection, channel, args)
            else:
                if user in self.blocked_users:
                    connection.privmsg(channel, "Vous êtes bloqué et ne pouvez pas recevoir de réponses.")
                else:
                    self.update_context(channel, user, message)
                    response = self.generate_response(channel, user, message)
                    self.send_message_in_chunks(connection, channel, response)

    def parse_command(self, message):
        parts = message.strip().split(" ", 1)
        command = parts[0].lower()
        args = parts[1] if len(parts) > 1 else ""
        return command, args

    def send_help_message(self, connection, channel):
        help_message = (
            "'raz' oublie la conversation, 'vision [image URL]' pour décrire une image."
        )
        connection.privmsg(channel, help_message)

    def update_context(self, channel, user, message):
        context = self.user_contexts.get((channel, user), [])
        context.append(message + ".\n")
        if len(context) > self.max_num_line:
            context = context[-self.max_num_line:]
        self.user_contexts[(channel, user)] = context

    def generate_response(self, channel, user, message):
        context = "\n".join(self.user_contexts[(channel, user)][:-1])
        last_message = self.user_contexts[(channel, user)][-1]
        prompt_text = f"Contexte:\n{context}\n\nRépond seulement à la dernière ligne en tenant compte du contexte précédent.\nDernière ligne: {last_message}"
        return self.mistral_api_request(prompt_text)

    def mistral_api_request(self, prompt):
        with self.api_lock:
            current_time = time.time()
            time_since_last_request = current_time - self.last_request_time
            if time_since_last_request < 2:  # Délai minimum de 2 secondes entre les requêtes
                time.sleep(2 - time_since_last_request)
            self.last_request_time = time.time()

            try:
                chat_response = self.client.chat.complete(
                    model=self.model,
                    messages=[{"role": "user", "content": prompt}],
                )
                return chat_response.choices[0].message.content
            except Exception as e:
                print(f"Erreur: {e}")
                return f"Erreur: {e}"

    def reset_user_context(self, channel, user):
        self.user_contexts[(channel, user)] = ["Bonjour"]

    def generate_image_description(self, connection, channel, image_url):
        try:
            messages = [
                {
                    "role": "user",
                    "content": [
                        {"type": "text", "text": "Décrit moi cette image en détails"},
                        {"type": "image_url", "image_url": image_url}
                    ]
                }
            ]
            prompt = str(messages)
            response = self.mistral_api_request(prompt)
            self.send_message_in_chunks(connection, channel, f"Description de l'image : {response}")
        except Exception as e:
            connection.privmsg(channel, f"Erreur: {str(e)}")

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
    bot = ChatGPTBot("mistral.json")
    bot.start()
