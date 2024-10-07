import os
import json
import irc.bot
import google.generativeai as genai
import re
import time

# Remplacez par votre clé API Google Generative AI
os.environ["API_KEY"] = "API KEY"
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

# Classe du bot IRC
class GeminiBot(irc.bot.SingleServerIRCBot):
    def __init__(self, channel, nickname, model):
        self.channel = channel
        self.nickname = nickname
        self.model = model
        self.contexts = {}
        irc.bot.SingleServerIRCBot.__init__(self, [(SERVER, PORT)], self.nickname, self.nickname)

    # Méthode pour obtenir le nom de fichier pour un utilisateur et un titre
    def get_context_filename(self, user, title):
        return os.path.join("gemini-conversations", f"{user}_{title}.json")

    # Méthode pour charger le contexte d'un utilisateur à partir d'un fichier
    def load_context(self, connection, user, title):
        filename = self.get_context_filename(user, title)
        try:
            with open(filename, 'r') as f:
                self.contexts[user] = json.load(f)
            send_message_in_chunks(connection, self.channel, f"Le fichier '{filename}' a été chargé avec succès pour {user}.")
        except FileNotFoundError:
            self.contexts[user] = []
            send_message_in_chunks(connection, self.channel, f"Aucun fichier de contexte trouvé pour {user} sous le titre '{title}', contexte initialisé.")

    # Méthode pour sauvegarder le contexte d'un utilisateur dans un fichier
    def save_context(self, connection, user, title):
        filename = self.get_context_filename(user, title)
        with open(filename, 'w') as f:
            json.dump(self.contexts.get(user, []), f)
        send_message_in_chunks(connection, self.channel, f"Contexte de {user} sauvegardé sous le titre '{title}'.")

    # Méthode pour lister les fichiers de conversations dans gemini-conversations
    def list_conversations(self, connection, target):
        conversations_folder = 'gemini-conversations'
        try:
            files = os.listdir(conversations_folder)
            # Filtrer pour ne garder que les fichiers JSON et extraire les titres
            titles = sorted(set([f.split('_', 1)[1].replace('.json', '') for f in files if f.endswith('.json')]))

            if titles:
                send_message_in_chunks(connection, target, "Liste des conversations :")
                for title in titles:
                    send_message_in_chunks(connection, target, f"- {title}")
            else:
                send_message_in_chunks(connection, target, "Aucune conversation trouvée.")
        except FileNotFoundError:
            send_message_in_chunks(connection, target, "Le dossier gemini-conversations n'existe pas.")

    # Méthode pour supprimer un fichier de conversation
    def delete_conversation(self, connection, target, title):
        conversations_folder = 'gemini-conversations'
        files = os.listdir(conversations_folder)

        # Trouver le fichier correspondant au titre fourni
        for file in files:
            if file.endswith(".json") and file.split('_', 1)[1].replace('.json', '') == title:
                file_path = os.path.join(conversations_folder, file)
                try:
                    os.remove(file_path)
                    send_message_in_chunks(connection, target, f"Le fichier de conversation '{title}' a été supprimé.")
                except Exception as e:
                    send_message_in_chunks(connection, target, f"Erreur lors de la suppression du fichier '{title}': {e}")
                return
        
        send_message_in_chunks(connection, target, f"Aucun fichier de conversation correspondant à '{title}' n'a été trouvé.")

    # Action à effectuer lors de la connexion au serveur IRC
    def on_welcome(self, connection, e):
        connection.join(self.channel)
        send_message_in_chunks(connection, self.channel, f"{self.nickname} est prêt à recevoir des commandes !")

    # Réception des messages publics dans le canal
    def on_pubmsg(self, connection, e):
        if e.arguments[0].startswith(f"{self.nickname}: "):
            command = e.arguments[0][len(self.nickname) + 2:].strip()
            user = e.source.nick

            if command == "list":
                self.list_conversations(connection, self.channel)
            elif command.startswith("delete"):
                title = command.split(" ", 1)[1] if len(command.split()) > 1 else None
                if title:
                    self.delete_conversation(connection, self.channel, title)
                else:
                    send_message_in_chunks(connection, self.channel, "Veuillez spécifier un fichier de conversation à supprimer.")
            elif command == "quit":
                connection.quit("Le bot se déconnecte à bientôt Labynet...")
                self.save_context(connection, user, "default")
                exit()
            elif command.startswith("save"):
                title = command.split(" ", 1)[1] if len(command.split()) > 1 else "default"
                self.save_context(connection, user, title)
            elif command.startswith("load"):
                title = command.split(" ", 1)[1] if len(command.split()) > 1 else "default"
                self.load_context(connection, user, title)
            elif command.startswith("raz"):
                self.contexts[user] = []
                send_message_in_chunks(connection, self.channel, f"Contexte de {user} réinitialisé.")
            else:
                context = self.contexts.get(user, [])
                try:
                    full_input = ' '.join(context + [command])
                    response = self.model.generate_content([full_input])
                    send_message_in_chunks(connection, self.channel, response.text)
                    self.contexts[user] = (context + [f"Nous avons parlé de : {command}. Réponse : {response.text}"])[-10:]
                except Exception as err:
                    send_message_in_chunks(connection, self.channel, "Erreur : Impossible de générer une réponse.")

    # Réception des messages privés
    def on_privmsg(self, connection, e):
        command = e.arguments[0].strip()
        user = e.source.nick
        context = self.contexts.get(user, [])

        try:
            full_input = ' '.join(context + [command])
            response = self.model.generate_content([full_input])
            send_message_in_chunks(connection, e.source.nick, response.text)
            self.contexts[user] = context + [f"Nous avons parlé de : {command}. Réponse : {response.text}"]
        except Exception as err:
            send_message_in_chunks(connection, e.source.nick, "Erreur : Impossible de générer une réponse.")

    # Action à effectuer lors de la déconnexion d'un utilisateur
    def on_part(self, connection, e):
        user = e.source.nick
        self.save_context(connection, user, "default")

    # Action à effectuer lors de la reconnexion d'un utilisateur
    def on_join(self, connection, e):
        user = e.source.nick
        self.load_context(connection, user, "default")

# Lancement du bot
if __name__ == "__main__":
    bot = GeminiBot(CHANNEL, NICKNAME, model)
    bot.start()
