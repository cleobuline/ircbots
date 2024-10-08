import irc.bot
import irc.client
import threading
import hashlib
import logging
import time
import signal
import sys

# Configurer le logging de base

# Dictionnaires globaux pour stocker les informations des utilisateurs
registered_users = {}  # Cela stocke les mots de passe hachés et les indices
authenticated_users = {}

def save_registered_users():
    """Sauvegarder les utilisateurs enregistrés dans le fichier."""
    try:
        with open("registered_users.txt", "w") as f:
            for nick, (hashed_password, hint) in registered_users.items():
                f.write(f"{nick}:{hashed_password}:{hint}\n")
    except IOError as e:
        logging.error(f"Erreur lors de la sauvegarde des utilisateurs enregistrés : {e}")

def load_registered_users():
    """Charger les utilisateurs enregistrés depuis le fichier."""
    global registered_users
    try:
        with open("registered_users.txt", "r") as f:
            for line in f:
                nick, hashed_password, hint = line.strip().split(":")
                registered_users[nick] = (hashed_password, hint)
    except FileNotFoundError:
        logging.warning("Le fichier des utilisateurs enregistrés est introuvable. Création d'un nouveau fichier.")
    except IOError as e:
        logging.error(f"Erreur lors du chargement des utilisateurs enregistrés : {e}")

def hash_password(password):
    """Retourner le hash MD5 du mot de passe donné."""
    return hashlib.md5(password.encode()).hexdigest()

def generate_hint(password):
    """Générer un indice (les 3 premiers caractères du mot de passe)."""
    return password[:3] if len(password) >= 3 else password

def normalize_nick(nick):
    """Normaliser le pseudo en supprimant les traits de soulignement finaux."""
    return nick.rstrip('_').lower()

class RegisterBot(irc.bot.SingleServerIRCBot):
    def __init__(self, channel, nickname, server, oper_user, oper_pass, port=6667):
        super().__init__([(server, port)], nickname, nickname)
        self.channel = channel
        self.oper_user = oper_user
        self.oper_pass = oper_pass

    def on_welcome(self, connection, event):
        connection.send_raw(f"OPER {self.oper_user} {self.oper_pass}")
        connection.join(self.channel)
        connection.mode(self.channel, f"+o {self._nickname}")
        load_registered_users()

    def on_disconnect(self, connection, event):
        while True:
            try:
                self.connection.reconnect()
                break;
            except irc.client.ServerConnectionError:
                logging.warning("Échec de la reconnexion, nouvelle tentative dans 5 secondes...")
                time.sleep(5)

    def on_join(self, connection, event):
        nick = normalize_nick(event.source.split('!')[0])
        if nick in registered_users and nick not in authenticated_users:
            authenticated_users[nick] = False
            connection.privmsg(nick, "Bienvenue de nouveau ! Veuillez vous authentifier en fournissant votre mot de passe avec /msg gardien !auth <mot_de_passe>")

    def on_privmsg(self, connection, event):
        original_nick = event.source.split('!')[0]
        message = event.arguments[0].strip()
        nick = normalize_nick(original_nick)


        help_messages = [
            "Commandes:", 
            "!register <mot_de_passe> <confirmation_mot_de_passe> - Enregistrement", 
            "!auth <mot_de_passe> - Authentification",
            "!unauth - Désauthentification",
            "!unregister - Désinscription.",
            "!pwd <nouveau_mot_de_passe> <confirmation_nouveau_mot_de_passe> - Changer de mot de passe",
            "!help - Aide."
        ]

        if message.startswith("!help"):
            for line in help_messages:
                connection.privmsg(original_nick, line)

        elif message.startswith("!register"):
            if nick in registered_users:
                connection.privmsg(original_nick, "Vous êtes déjà enregistré. Veuillez utiliser !auth pour vous authentifier.")
            else:
                try:
                    parts = message.split()
                    if len(parts) != 3:
                        connection.privmsg(original_nick, "Utilisation : !register <mot_de_passe> <confirmation_mot_de_passe>")
                        return

                    _, password, confirm_password = parts
                    if password == confirm_password:
                        hashed_password = hash_password(password)
                        hint = generate_hint(password)
                        registered_users[nick] = (hashed_password, hint)
                        save_registered_users()
                        connection.privmsg(original_nick, "Vous avez été enregistré avec succès !")
                    else:
                        connection.privmsg(original_nick, "Les mots de passe ne correspondent pas.")
                except ValueError:
                    connection.privmsg(original_nick, "Utilisation : !register <mot_de_passe> <confirmation_mot_de_passe>")

        elif message.startswith("!auth"):
            if nick in registered_users:
                if nick not in authenticated_users:
                    authenticated_users[nick] = False

                try:
                    parts = message.split()
                    if len(parts) != 2:
                        connection.privmsg(original_nick, "Utilisation : !auth <mot_de_passe>")
                        return

                    _, password = parts
                    hashed_password = hash_password(password)
                    stored_hashed_password, stored_hint = registered_users.get(nick, (None, None))

                    if stored_hashed_password == hashed_password:
                        connection.privmsg(original_nick, "Authentification réussie ! Bienvenue !")
                        time.sleep(2)
                        connection.mode(self.channel, f"+v {original_nick}")
                        authenticated_users[nick] = True
                    else:
                        connection.privmsg(original_nick, f"Mot de passe incorrect. Les trois premiers caractères de votre mot de passe sont : {stored_hint}. Veuillez réessayer.")
                except ValueError:
                    connection.privmsg(original_nick, "Utilisation : !auth <mot_de_passe>")
            else:
                connection.privmsg(original_nick, "Vous n'êtes pas enregistré. Veuillez vous enregistrer d'abord en utilisant !register <mot_de_passe> <confirmation_mot_de_passe>")

        elif message.startswith("!unauth"):
            if nick in authenticated_users and authenticated_users[nick]:
                connection.mode(self.channel, f"-v {original_nick}")
                authenticated_users[nick] = False
                connection.privmsg(original_nick, "Vous avez été désauthentifié.")
            else:
                connection.privmsg(original_nick, "Vous n'êtes pas actuellement authentifié ou non enregistré.")

        elif message.startswith("!unregister"):
            if nick in registered_users:
                if authenticated_users.get(nick, False):
                    del registered_users[nick]
                    save_registered_users()
                    connection.mode(self.channel, f"-v {original_nick}")
                    del authenticated_users[nick]
                    connection.privmsg(original_nick, "Vous avez été désinscrit et désauthentifié.")
                else:
                    connection.privmsg(original_nick, "Vous devez être authentifié pour vous désinscrire.")
            else:
                connection.privmsg(original_nick, "Vous n'êtes pas enregistré, donc vous ne pouvez pas vous désinscrire.")

        elif message.startswith("!pwd"):
            if nick in authenticated_users and authenticated_users[nick]:
                try:
                    parts = message.split()
                    if len(parts) != 3:
                        connection.privmsg(original_nick, "Utilisation : !psw <nouveau_mot_de_passe> <confirmation_nouveau_mot_de_passe>")
                        return

                    _, new_password, confirm_new_password = parts
                    if new_password == confirm_new_password:
                        new_hashed_password = hash_password(new_password)
                        new_hint = generate_hint(new_password)
                        registered_users[nick] = (new_hashed_password, new_hint)
                        save_registered_users()
                        connection.privmsg(original_nick, "Votre mot de passe a été changé avec succès !")
                    else:
                        connection.privmsg(original_nick, "Les mots de passe ne correspondent pas.")

                except ValueError:
                    connection.privmsg(original_nick, "Utilisation : !psw <nouveau_mot_de_passe> <confirmation_nouveau_mot_de_passe>")
            else:
                connection.privmsg(original_nick, "Vous devez être authentifié pour changer votre mot de passe.")

def signal_handler(sig, frame):
    logging.info("Signal reçu, arrêt du bot.")
    sys.exit(0)

def main():
    signal.signal(signal.SIGINT, signal_handler)

    server = "labynet.fr"  # Remplacez par le serveur IRC approprié
    port = 6667
    channel = "#labynet"  # Remplacez par le canal approprié
    nickname = "gardien"
    oper_user = "ooer user name "  # Nom d'utilisateur opérateur
    oper_pass = "oper user password"  # Mot de passe opérateur

    bot = RegisterBot(channel, nickname, server, oper_user, oper_pass, port)
    bot_thread = threading.Thread(target=bot.start)
    bot_thread.start()

if __name__ == "__main__":
    main()
