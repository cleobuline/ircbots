import os
import json
import irc.bot
import google.generativeai as genai
import re
import time

# Remplacez par votre clé API Google Generative AI
os.environ["API_KEY"] = "GOOGLE API KEY"
genai.configure(api_key=os.environ["API_KEY"])

# Configuration du serveur IRC
SERVER = "labynet.fr"   # Remplacez par l'adresse du serveur IRC
PORT = 6667
CHANNEL = "#labynet"    # Remplacez par le canal IRC
NICKNAME = "gemini"  # Nom du bot

# Créer le modèle Google Generative AI (Gemini 1.5 Flash)
model = genai.GenerativeModel("gemini-1.5-flash")

# Fonction pour découper et envoyer des messages longs en morceaux
def send_message_in_chunks(connection, target, message):
    lines = message.split('\n')
    for line in lines:
        while line:
            # Vérifiez si la ligne est plus longue que la limite
            if len(line.encode('utf-8')) > 392:
                # Trouver le dernier espace dans la limite
                last_space_index = line[:392].rfind(' ')
                if last_space_index == -1:
                    # Si pas d'espace trouvé, couper à la limite
                    connection.privmsg(target, line[:392].strip())
                    line = line[392:]
                else:
                    # Envoyer jusqu'à l'espace trouvé
                    connection.privmsg(target, line[:last_space_index].strip())
                    line = line[last_space_index:].strip()
            else:
                # Si la ligne est dans la limite, envoyer le reste
                connection.privmsg(target, line.strip())
                line = ''
            time.sleep(0.5)  # Pause pour éviter de flooder le serveur

# Classe du bot IRC
class GeminiBot(irc.bot.SingleServerIRCBot):
    def __init__(self, channel, nickname, model):
        self.channel = channel
        self.nickname = nickname
        self.model = model
        self.contexts = {}  # Dictionnaire pour gérer les contextes des utilisateurs
        irc.bot.SingleServerIRCBot.__init__(self, [(SERVER, PORT)], self.nickname, self.nickname)

    # Méthode pour charger le contexte à partir d'un fichier
    def load_context(self):
        try:
            with open('context.json', 'r') as f:
                self.contexts = json.load(f)
        except FileNotFoundError:
            self.contexts = {}  # Si le fichier n'existe pas, initialiser un dictionnaire vide

    # Méthode pour sauvegarder le contexte dans un fichier
    def save_context(self):
        with open('context.json', 'w') as f:
            json.dump(self.contexts, f)

    # Action à effectuer lors de la connexion au serveur IRC
    def on_welcome(self, c, e):
        c.join(self.channel)
        send_message_in_chunks(c, self.channel, f"{self.nickname} est prêt à recevoir des commandes !")
        self.load_context()  # Charger le contexte à la connexion

    # Réception des messages publics dans le canal
    def on_pubmsg(self, c, e):
        if e.arguments[0].startswith(f"{self.nickname}: "):
            command = e.arguments[0][len(self.nickname) + 2:].strip()
            user = e.source.nick

            if command == "quit":
                c.quit("Le bot se déconnecte...")
                self.save_context()  # Sauvegarder le contexte avant de quitter
                exit()
            elif command.startswith("save"):
                self.save_context()
                send_message_in_chunks(c, self.channel, "Contexte sauvegardé.")
            elif command.startswith("load"):
                self.load_context()
                send_message_in_chunks(c, self.channel, "Contexte chargé.")
            elif command.startswith("raz"):
                # Remettre à zéro le contexte de l'utilisateur
                self.contexts[user] = []
                send_message_in_chunks(c, self.channel, f"Contexte de {user} réinitialisé.")
            else:
                context = self.contexts.get(user, [])
                # Traiter la commande avec l'API Google Generative AI
                try:
                    # Combiner le contexte avec la commande
                    full_input = ' '.join(context + [command])
                    response = self.model.generate_content([full_input])
                    send_message_in_chunks(c, self.channel, response.text)  # Envoi de la réponse en morceaux
                    # Mettre à jour le contexte
                    self.contexts[user] = context + [f"Nous avons parlé de : {command}. Réponse : {response.text}"]  # Ajouter tout le contexte
                except Exception as err:
                    send_message_in_chunks(c, self.channel, "Erreur : Impossible de générer une réponse.")

    # Réception des messages privés
    def on_privmsg(self, c, e):
        command = e.arguments[0].strip()
        user = e.source.nick
        context = self.contexts.get(user, [])

        try:
            # Combiner le contexte avec la commande
            full_input = ' '.join(context + [command])
            response = self.model.generate_content([full_input])
            send_message_in_chunks(c, e.source.nick, response.text)  # Envoi de la réponse en morceaux
            # Mettre à jour le contexte
            self.contexts[user] = context + [f"Nous avons parlé de : {command}. Réponse : {response.text}"]  # Ajouter tout le contexte
        except Exception as err:
            send_message_in_chunks(c, e.source.nick, "Erreur : Impossible de générer une réponse.")

# Lancement du bot
if __name__ == "__main__":
    bot = GeminiBot(CHANNEL, NICKNAME, model)
    bot.start()
