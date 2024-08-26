import irc.bot
import openai
import json
import os
import requests
import pyshorteners
import imgbbpy
import time 

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
        self.user_contexts = {}
        self.model = "gpt-4o-mini"

        # Initialiser les clients OpenAI et imgbb
        openai.api_key = self.api_key
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
            elif command == "model":
                self.change_model(channel, user, args)
            elif command == "list-models":
                self.list_models(channel)
            elif command == "local":
                self.generate_image_local(connection, channel, args)
            elif command == "image":
                self.generate_image_tiny(connection, channel, args)
            elif command == "imgbb":
                self.generate_image_imgbb(connection, channel, args)
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
                        "'model [model_name]' pour changer le modèle, 'list-models' liste les modèles valides, "
                        "'image [prompt]' pour générer une image.")
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
        
        response = openai.ChatCompletion.create(
            model=self.model,
            messages=[{"role": "user", "content": prompt_text}]
        )
        generated_text = response.choices[0].message.content.strip()
        return generated_text

    def reset_user_context(self, channel, user):
        self.user_contexts[(channel, user)] = ["Bonjour"]

    def save_user_context(self, channel, user, title):
        if (channel, user) in self.user_contexts:
            context = self.user_contexts[(channel, user)]
            filename = os.path.join("conversations", f"{user}.{title}.context.json")
            with open(filename, "w") as file:
                json.dump(context, file)
            self.connection.privmsg(channel, f"Contexte utilisateur de {user} sauvegardé sous le titre {title} dans {filename}.")
        else:
            self.connection.privmsg(channel, f"Aucun contexte à sauvegarder pour {user}.")

    def load_user_context(self, channel, user, title):
        filename = os.path.join("conversations", f"{user}.{title}.context.json")
        if os.path.exists(filename):
            with open(filename, "r") as file:
                context = json.load(file)
            self.user_contexts[(channel, user)] = context
            self.connection.privmsg(channel, f"Contexte utilisateur de {user} avec le titre {title} chargé depuis {filename}.")
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
        else:
            self.connection.privmsg(channel, f"Aucun fichier de contexte trouvé pour {user} avec le titre {title}.")

    def block_user(self, user):
        self.blocked_users.add(user)

    def unblock_user(self, user):
        self.blocked_users.discard(user)

    def change_model(self, channel, user, model):
        valid_models = ["gpt-3.5-turbo", "gpt-4", "gpt-4o", "gpt-4o-mini", "gpt-3.5-turbo-16k", "gpt-4-32k"]
        if model in valid_models:
            self.model = model
            self.connection.privmsg(channel, f"Le modèle a été changé en {model}.")
        else:
            self.connection.privmsg(channel, f"Modèle invalide. Les modèles valides sont: {', '.join(valid_models)}.")

    def list_models(self, channel):
        valid_models = ["gpt-3.5-turbo", "gpt-4", "gpt-4o", "gpt-4o-mini", "gpt-3.5-turbo-16k", "gpt-4-32k"]
        self.connection.privmsg(channel, f"Modèles valides: {', '.join(valid_models)}")

    def generate_image_tiny(self, connection, channel, prompt):
        response = openai.Image.create(
            prompt=prompt,
            n=1,
            size="512x512"
        )
        image_url = response['data'][0]['url']
        shortener = pyshorteners.Shortener()
        short_url = shortener.tinyurl.short(image_url)
        connection.privmsg(channel, short_url)

    def generate_image_imgbb(self, connection, channel, prompt):
        response = openai.Image.create(
            prompt=prompt,
            n=1,
            size="512x512"
        )
        image_url = response['data'][0]['url']

        # Télécharger l'image
        image_data = requests.get(image_url).content

        # Enregistrer l'image temporairement
        temp_image_path = 'temp_image.png'
        with open(temp_image_path, 'wb') as f:
            f.write(image_data)

        # Uploader l'image sur imgbb
        imgbb_response = self.imgbb_client.upload(file=temp_image_path)

        # Supprimer l'image temporaire
        os.remove(temp_image_path)

        # Obtenir l'URL courte
        short_url = imgbb_response.url
        connection.privmsg(channel, short_url)

    def generate_image_local(self, connection, channel, prompt):

        response = openai.Image.create(prompt=prompt, n=1, size="1024x1024")
        image_url = response['data'][0]['url']

        image_response = requests.get(image_url)
        image_filename = "/PUT HERE YOUR SERVER FOLDER/generated_image.png"

        with open(image_filename, "wb") as image_file:
                image_file.write(image_response.content)

        web_url = "http://PUT HERE YOUR URL /generated_image.png"
        connection.privmsg(channel, web_url)

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
    bot = ChatGPTBot("zozo.json")
    bot.start()
