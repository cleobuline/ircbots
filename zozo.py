import irc.bot
import irc.strings
import openai
import json
import os
import textwrap  # Importation de la bibliothèque textwrap

class ChatGPTBot(irc.bot.SingleServerIRCBot):
    def __init__(self, config_file):
        with open(config_file, "r") as f:
            config = json.load(f)

        channel_list = config["channels"].split(",")
        nickname = config["nickname"]
        server = config["server"]
        port = config["port"]
        api_key = config["api_key"]
        max_num_line = config["max_num_line"]
        self.admin_user = config["admin_user"]

        irc.bot.SingleServerIRCBot.__init__(self, [(server, port)], nickname, nickname)
        self.channel_list = [channel.strip() for channel in channel_list]
        self.api_key = api_key
        openai.api_key = self.api_key
        self.user_contexts = {}
        self.max_num_line = max_num_line
        self.blocked_users = set()
        self.model = "gpt-4o-mini"  # Default model
        
        if not os.path.exists("conversations"):
            os.makedirs("conversations")

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
            if message.strip().lower().startswith("help"):
                connection.privmsg(channel, "'raz' oublie la conversation , 'save [titre]', 'load [titre]', 'delete [titre]' , 'files' liste les conversations , 'block [user]' bloque un utilisateur, 'unblock [user]' débloque un utilisateur, 'model [model_name]' pour changer le modèle, 'list-models' liste les modèles valides")
                return
            elif message.strip().lower().startswith("raz"):
                self.reset_user_context(channel, user)
                connection.privmsg(channel, "Conversation oubliée ...")
                return
            elif message.strip().lower().startswith("save"):
                parts = message.strip().split(" ", 1)
                if len(parts) > 1:
                    title = parts[1]
                    self.save_user_context(channel, user, title)
                else:
                    connection.privmsg(channel, "Veuillez spécifier un titre pour la sauvegarde.")
                return
            elif message.strip().lower().startswith("load"):
                parts = message.strip().split(" ", 1)
                if len(parts) > 1:
                    title = parts[1]
                    self.load_user_context(channel, user, title)
                else:
                    connection.privmsg(channel, "Veuillez spécifier un titre pour le chargement.")
                return
            elif message.strip().lower().startswith("files"):
                self.list_user_files(channel, user)
                return
            elif message.strip().lower().startswith("delete"):
                parts = message.strip().split(" ", 1)
                if len(parts) > 1:
                    title = parts[1]
                    self.delete_user_context(channel, user, title)
                else:
                    connection.privmsg(channel, "Veuillez spécifier un titre pour la suppression.")
                return
            elif message.strip().lower().startswith("block"):
                if user == self.admin_user:  # Vérifier l'autorisation de l'utilisateur
                    parts = message.strip().split(" ", 1)
                    if len(parts) > 1:
                        blocked_user = parts[1]
                        self.block_user(blocked_user)
                        connection.privmsg(channel, f"Utilisateur {blocked_user} bloqué.")
                    else:
                        connection.privmsg(channel, "Veuillez spécifier un utilisateur à bloquer.")
                else:
                    connection.privmsg(channel, "Vous n'avez pas l'autorisation de bloquer des utilisateurs.")
                return
            elif message.strip().lower().startswith("unblock"):
                if user == self.admin_user:  # Vérifier l'autorisation de l'utilisateur
                    parts = message.strip().split(" ", 1)
                    if len(parts) > 1:
                        unblocked_user = parts[1]
                        self.unblock_user(unblocked_user)
                        connection.privmsg(channel, f"Utilisateur {unblocked_user} débloqué.")
                    else:
                        connection.privmsg(channel, "Veuillez spécifier un utilisateur à débloquer.")
                return
            elif message.strip().lower().startswith("model"):
                parts = message.strip().split(" ", 1)
                if len(parts) > 1:
                    model = parts[1].strip()
                    self.change_model(channel, user, model)
                else:
                    connection.privmsg(channel, "Veuillez spécifier un modèle à utiliser.")
                return
            elif message.strip().lower().startswith("list-models"):
                self.list_models(channel)
                return

            if user in self.blocked_users:
                connection.privmsg(channel, f"Vous êtes bloqué et ne pouvez pas recevoir de réponses.")
                return

            self.update_context(channel, user, message)

            try:
                response = self.generate_response(channel, user, message)
            except Exception as e:
                response = "Une erreur s'est produite lors de la génération de la réponse."
                self.connection.privmsg(channel, f"Erreur OpenAI: {e}")

            self.send_message_in_chunks(connection, channel, response)

    def update_context(self, channel, user, message):
        if (channel, user) not in self.user_contexts:
            self.user_contexts[(channel, user)] = []
        
        self.user_contexts[(channel, user)].append(message + ".\n")

        if len(self.user_contexts[(channel, user)]) > self.max_num_line:
            context_list = self.user_contexts[(channel, user)]
            self.user_contexts[(channel, user)] = context_list[1:]

    def generate_response(self, channel, user, message):
        context = "\n".join(self.user_contexts[(channel, user)][:-1])  # Contexte sans la dernière ligne
        last_message = self.user_contexts[(channel, user)][-1]  # Dernière ligne du contexte (la nouvelle requête)

        prompt_text = f"Contexte:\n{context}\n\nRépond seulement à la dernière ligne en tenant compte du contexte précédent.\nDernière ligne: {last_message}"
        
        response = openai.ChatCompletion.create(
            model=self.model,
            messages=[
                {"role": "user", "content": prompt_text}
            ]
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
        files.sort()  # Trie les fichiers par ordre alphabétique
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
        valid_models = ["gpt-3.5-turbo", "gpt-4", "gpt-4o", "gpt-4o-mini", "gpt-3.5-turbo-16k", "gpt-4-32k"]  # Add other valid models as needed
        if model in valid_models:
            self.model = model
            self.connection.privmsg(channel, f"Le modèle a été changé en {model}.")
        else:
            self.connection.privmsg(channel, f"Modèle invalide. Les modèles valides sont: {', '.join(valid_models)}")

    def list_models(self, channel):
        valid_models = ["gpt-3.5-turbo", "gpt-4", "gpt-4o", "gpt-4o-mini", "gpt-3.5-turbo-16k", "gpt-4-32k"]  # List of valid models
        self.connection.privmsg(channel, f"Modèles valides: {', '.join(valid_models)}")

    def send_message_in_chunks(self, connection, target, message):
        # Nombre maximum de caractères pour chaque message IRC
        MAX_MESSAGE_LENGTH = 392
        message = message.replace('\n', ' ').replace('\r', ' ')
        # Utilise textwrap pour découper le message en morceaux adaptés
        wrapped_lines = textwrap.wrap(message, width=MAX_MESSAGE_LENGTH, break_long_words=False, replace_whitespace=False)
        
        for line in wrapped_lines:
            text=line.strip()
            connection.privmsg(target, text)

if __name__ == "__main__":
    bot = ChatGPTBot("zozo.json")
    bot.start()
