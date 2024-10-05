import irc.bot
import json
import os
import requests
import pyshorteners
import imgbbpy
import time
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
        self.imgbb_api_key = config["imgbb_api_key"]
        self.admin_user = config["admin_user"]
        self.blocked_users = set()
        self.user_context = {}
        self.model = "mistral-large-latest"

        # Paramètres de génération de réponse
        self.temperature = config.get("temperature", 0.7)
        self.top_p = config.get("top_p", 1.0)
        self.top_k = config.get("top_k", 50)
        self.max_tokens = config.get("max_tokens", 1500)
        self.presence_penalty = config.get("presence_penalty", 0.0)
        self.frequency_penalty = config.get("frequency_penalty", 0.0)

        # Vérifier si l'API key est définie
        if not self.api_key:
            raise ValueError("MISTRAL_API_KEY environment variable is not set")

        # Initialiser le client Mistral
        self.client = Mistral(api_key=self.api_key)

        # Initialiser le client imgbb
        self.imgbb_client = imgbbpy.SyncClient(self.imgbb_api_key)

        # Créer le dossier de conversations s'il n'existe pas
        if not os.path.exists("conversations"):
            os.makedirs("conversations")

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
            elif command == "save":
                self.save_user_context(channel, user, args)
            elif command == "load":
                self.load_user_context(channel, user, args)
            elif command == "files":
                self.list_user_files(channel, user)
            elif command == "delete":
                self.delete_user_context(channel, user, args)
            elif command == "block" and user == self.admin_user:
                self.block_user(args)
                connection.privmsg(channel, f"Utilisateur {args} bloqué.")
            elif command == "unblock" and user == self.admin_user:
                self.unblock_user(args)
                connection.privmsg(channel, f"Utilisateur {args} débloqué.")
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
        help_message = ("'raz' oublie la conversation, 'save [titre]', 'load [titre]', "
                        "'delete [titre]', 'files' liste les conversations, 'block [user]' "
                        "bloque un utilisateur, 'unblock [user]' débloque un utilisateur, "
                        "'vision [image URL]' pour décrire une image.")
        connection.privmsg(channel, help_message)

    def update_context(self, channel, user, message):
        context = self.user_context.get((channel, user), [])
        context.append(message + ".\n")

        if len(context) > self.max_num_line:
            context = context[-self.max_num_line:]

        self.user_context[(channel, user)] = context
        print(f"context updated for {user} in {channel}: {context}")  # Log for debugging

    def generate_response(self, channel, user, message):
        context = "\n".join(self.user_context[(channel, user)][:-1])
        last_message = self.user_context[(channel, user)][-1]

        prompt_text = f"contexte:\n{context}\n\nRépond seulement à la dernière ligne en tenant compte du contexte précédent.\nDernière ligne: {last_message}"

        try:
            response = self.mistral_api_request(prompt_text)
            generated_text = response.strip()
            return generated_text
        except Exception as e:
            print(f"Error generating response: {e}")  # Log for debugging
            return f"Erreur lors de la génération de la réponse: {str(e)}"

    def mistral_api_request(self, prompt):
        try:
            chat_response = self.client.chat.complete(
                model=self.model,
                messages=[
                    {
                        "role": "user",
                        "content": prompt,
                    },
                ],
                temperature=self.temperature,
                top_p=self.top_p,
                max_tokens=self.max_tokens
            )
            return chat_response.choices[0].message.content
        except Exception as e:
            print(f"Erreur: {e}")
            return f"Erreur: {e}"

    def reset_user_context(self, channel, user):
        self.user_context[(channel, user)] = ["Bonjour"]
        print(f"context reset for {user} in {channel}")  # Log for debugging

    def save_user_context(self, channel, user, title):
        if (channel, user) in self.user_context:
            context = self.user_context[(channel, user)]
            filename = os.path.join("conversations", f"{user}.{title}.context.json")
            with open(filename, "w") as file:
                json.dump(context, file)
            self.connection.privmsg(channel, f"contexte utilisateur de {user} sauvegardé sous le titre {title} dans {filename}.")
            print(f"context saved for {user} in {channel} with title {title}")  # Log for debugging
        else:
            self.connection.privmsg(channel, f"Aucun contexte à sauvegarder pour {user}.")

    def load_user_context(self, channel, user, title):
        filename = os.path.join("conversations", f"{user}.{title}.context.json")
        if os.path.exists(filename):
            with open(filename, "r") as file:
                context = json.load(file)
            self.user_context[(channel, user)] = context
            self.connection.privmsg(channel, f"contexte utilisateur de {user} avec le titre {title} chargé depuis {filename}.")
            print(f"context loaded for {user} in {channel} with title {title}")  # Log for debugging
        else:
            self.connection.privmsg(channel, f"Aucun fichier de contexte trouvé pour {user} avec le titre {title}.")

    def list_user_files(self, channel, user):
        files = [filename.split(".")[1].replace(".context.json", "") for filename in os.listdir("conversations") if filename.startswith(user) and filename.endswith(".context.json")]
        files.sort()
        if files:
            self.connection.privmsg(channel, f"Fichiers de contexte disponibles pour {user} :")
            for file in files:
                self.connection.privmsg(channel, f"- {file}")
        else:
            self.connection.privmsg(channel, f"Aucun fichier de contexte disponible pour {user}.")

    def delete_user_context(self, channel, user, title):
        filename = os.path.join("conversations", f"{user}.{title}.context.json")
        if os.path.exists(filename):
            os.remove(filename)
            self.connection.privmsg(channel, f"Fichier de contexte '{title}' supprimé pour l'utilisateur {user}.")
            print(f"context deleted for {user} in {channel} with title {title}")  # Log for debugging
        else:
            self.connection.privmsg(channel, f"Aucun fichier de contexte trouvé pour {user} avec le titre {title}.")

    def block_user(self, user):
        self.blocked_users.add(user)
        print(f"User {user} blocked")  # Log for debugging

    def unblock_user(self, user):
        self.blocked_users.discard(user)
        print(f"User {user} unblocked")  # Log for debugging

    def generate_image_description(self, connection, channel, image_url):
        try:
            messages = [
                {
                    "role": "user",
                    "content": [
                        {
                            "type": "text",
                            "text": "Décrit moi cette image en détails"
                        },
                        {
                            "type": "image_url",
                            "image_url": image_url
                        }
                    ]
                }
            ]
            chat_response = self.client.chat.complete(
                model="pixtral-12b-2409",
                messages=messages,
                temperature=self.temperature,
                max_tokens=self.max_tokens
            )
            description = chat_response.choices[0].message.content
            self.send_message_in_chunks(connection, channel, f"Description de l'image : {description}")
        except Exception as e:
            connection.privmsg(channel, f"Une erreur inattendue est survenue: {str(e)}")
            print(f"Error generating image description: {e}")  # Log for debugging

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
