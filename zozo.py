import irc.bot
import irc.strings
from irc.client import ip_numstr_to_quad, ip_quad_to_numstr
import openai
import time
import json
import os

class ChatGPTBot(irc.bot.SingleServerIRCBot):
    def __init__(self, channel, nickname, server, port=6667):
        irc.bot.SingleServerIRCBot.__init__(self, [(server, port)], nickname, nickname)
        self.channel = channel
        self.api_key = "PUT YOUR OPENAI API KEY HERE "
        openai.api_key = self.api_key
        self.user_contexts = {}
        self.max_num_line = 50
        
    def on_welcome(self, connection, event):
        connection.join(self.channel)

    def on_pubmsg(self, connection, event):
        message = event.arguments[0]
        user = event.source.nick

        bot_nickname = self.connection.get_nickname()
        if message.strip().startswith(bot_nickname + ":"):
            message = message[len(bot_nickname) + 1:].strip()
            if message.strip().lower().startswith("raz"):
                if (self.channel, user) in self.user_contexts:
                    self.user_contexts[(self.channel, user)] = {user: ["Bonjour"]}
                else:
                    self.user_contexts[(self.channel, user)] = {user: ["Bonjour"]}
                return
            elif message.strip().lower().startswith("save"):
                self.save_user_context(user)
                return
            elif message.strip().lower().startswith("load"):
                self.load_user_context(user)
                return

            if (self.channel, user) not in self.user_contexts:
                self.user_contexts[(self.channel, user)] = {user: []}
            if user not in self.user_contexts[(self.channel, user)]:
                self.user_contexts[(self.channel, user)][user] = []

            self.user_contexts[(self.channel, user)][user].append(message +"\n")

            if len(self.user_contexts[(self.channel, user)][user]) > self.max_num_line:
                context_list = self.user_contexts[(self.channel, user)][user]
                self.user_contexts[(self.channel, user)][user] = context_list[1:]

            try:
                response = self.generate_response(user, message, self.channel)
            except Exception as e:
                response = "Une erreur s'est produite lors de la génération de la réponse."
                print(f"Erreur OpenAI: {e}")

            self.send_message_in_chunks(connection, self.channel, response)

    def generate_response(self, user, message, channel):
        prompt_text = "répond  à la dernière ligne du message en tenant compte du contexte \n".join(self.user_contexts[(channel, user)][user])
        response = openai.ChatCompletion.create(
            model="gpt-4",
            messages=[
                {"role": "system", "content": prompt_text}
            ]
        )

        generated_text = response.choices[0].message.content.strip()
        return generated_text

    def save_user_context(self, user):
        if (self.channel, user) in self.user_contexts and user in self.user_contexts[(self.channel, user)]:
            context = self.user_contexts[(self.channel, user)][user]
            filename = f"{user}_context.json"
            with open(filename, "w") as file:
                json.dump(context, file)
            print(f"Contexte utilisateur de {user} sauvegardé dans {filename}.")
        else:
            print(f"Aucun contexte à sauvegarder pour {user}.")

    def load_user_context(self, user):
        filename = f"{user}_context.json"
        if os.path.exists(filename):
            with open(filename, "r") as file:
                context = json.load(file)
            if (self.channel, user) not in self.user_contexts:
                self.user_contexts[(self.channel, user)] = {user: context}
            else:
                self.user_contexts[(self.channel, user)][user] = context
            print(f"Contexte utilisateur de {user} chargé depuis {filename}.")
        else:
            print(f"Aucun fichier de contexte trouvé pour {user}.")

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
    bot = ChatGPTBot("#labynet", "zozo", "labynet.fr")
    bot.start()
