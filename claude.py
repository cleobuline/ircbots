import irc.bot
import json
import os
import time
import anthropic
from pylatexenc.latex2text import LatexNodes2Text

class ChatGPTBot(irc.bot.SingleServerIRCBot):
    def __init__(self, config_file):
        # Charger la configuration
        with open(config_file, "r") as f:
            config = json.load(f)
        # Initialiser les variables
        self.channel_list = [channel.strip() for channel in config["channels"].split(",")]
        self.nickname = "claude" 
        self.server = config["server"]
        self.port = config["port"]
        self.api_key = config["api_key"]
        self.max_num_line = config["max_num_line"]
        self.admin_user = config["admin_user"]
        self.claude_api_key = config["claude_api_key"]
        self.blocked_users = set()
        self.user_contexts = []  # List to store context information
        self.tag = ""
        self.claude = anthropic.Anthropic(api_key=self.claude_api_key)
       
        if not os.path.exists("conversations"):
            os.makedirs("conversations")
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
            else:
                if user in self.blocked_users:
                    connection.privmsg(channel, "Vous êtes bloqué et ne pouvez pas recevoir de réponses.")
                else:
                    self.update_context(channel, user, message)
                    response = self.generate_response_claude(channel, user, message)
                    self.send_message_in_chunks(connection, channel, response)

    def parse_command(self, message):
        parts = message.strip().split(" ", 1)
        command = parts[0].lower()
        args = parts[1] if len(parts) > 1 else ""
        return command, args

    def send_help_message(self, connection, channel):
        help_message = ("'raz' oublie la conversation, 'save [titre]', 'load [titre]', "
                        "'delete [titre]', 'files' liste les conversations, 'block [user]' "
                        "bloque un utilisateur, 'unblock [user]' débloque un utilisateur, ")
        connection.privmsg(channel, help_message)
    def reset_user_context(self, channel, user):
        context_entry = next((entry for entry in self.user_contexts if entry[0] == channel and entry[1] == user), None)      
        if context_entry:
            context_entry[2] = ["Bonjour"]
        else:
            self.user_contexts.append([channel, user, ["Bonjour"]])
    def update_context(self, channel, user, message):
        context_entry = next((entry for entry in self.user_contexts if entry[0] == channel and entry[1] == user), None)
        
        if context_entry:
            context = context_entry[2]
            context.append(message + ".")

            if len(context) > self.max_num_line:
                context = context[-self.max_num_line:]         
            context_entry[2] = context
        else:
            self.user_contexts.append([channel, user, [message + "."]])

        
    def generate_response_claude(self, channel, user, message):
        context_entry = next((entry for entry in self.user_contexts if entry[0] == channel and entry[1] == user), None)

        if context_entry:
            context = "\n".join(context_entry[2][:-1])
            #print (context)
            last_message = context_entry[2][-1]
            #print (last_message)
            prompt_text = f"Contexte:\n{context}\n\nRépond seulement à la dernière ligne en tenant compte du contexte précédent.\nDernière ligne: {last_message}"
            messages = []
            #messages.append({"role": "user", "content": context})
            messages.append({"role": "user", "content": prompt_text})
           
            response = self.claude.messages.create(
                model="claude-3-opus-20240229",
                max_tokens=1024,
                messages=messages
            )
            
            generated_text=  response.content[0].text
            readable_text = LatexNodes2Text().latex_to_text(generated_text)
            return readable_text
        return ""

    def save_user_context(self, channel, user, title):
        context_entry = next((entry for entry in self.user_contexts if entry[0] == channel and entry[1] == user), None)     
        if context_entry:
            context = context_entry[2]
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
            context_entry = next((entry for entry in self.user_contexts if entry[0] == channel and entry[1] == user), None)       
            if context_entry:
                context_entry[2] = context
            else:
                self.user_contexts.append([channel, user, context])        
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
