import irc.bot
import irc.strings
import threading
import hashlib

registered_users = {}
authenticated_users = {}

def save_registered_users():
    """Save the registered users to the file."""
    with open("registered_users.txt", "w") as f:
        for nick, password in registered_users.items():
            f.write(f"{nick}:{password}\n")

def load_registered_users():
    """Load the registered users from the file."""
    global registered_users
    try:
        with open("registered_users.txt", "r") as f:
            for line in f:
                nick, password = line.strip().split(":")
                registered_users[nick] = password
    except FileNotFoundError:
        pass

def hash_password(password):
    """Return the MD5 hash of the given password."""
    return hashlib.md5(password.encode()).hexdigest()

def normalize_nick(nick):
    """Normalize the nickname by removing trailing underscores."""
    return nick.rstrip('_').lower()

class RegisterBot(irc.bot.SingleServerIRCBot):
    def __init__(self, channel, nickname, server, oper_user, oper_pass, port=6667):
        super().__init__([(server, port)], nickname, nickname)
        self.channel = channel
        self.oper_user = oper_user
        self.oper_pass = oper_pass

    def on_welcome(self, connection, event):
        # Authenticate as operator
        connection.send_raw(f"OPER {self.oper_user} {self.oper_pass}")
        # Join the channel and give the bot operator privileges
        connection.join(self.channel)
        connection.mode(self.channel, f"+o {self._nickname}")
        # Load registered users from the file
        load_registered_users()

    def on_join(self, connection, event):
        nick = normalize_nick(event.source.split('!')[0])
        print (nick)
        if nick in registered_users and nick not in authenticated_users:
            authenticated_users[nick] = False
            connection.privmsg(nick, "Welcome back! Please authenticate by providing your password with /msg gardien !auth <password>")

    def on_privmsg(self, connection, event):
        original_nick = event.source.split('!')[0]
        message = event.arguments[0].strip()
        nick = normalize_nick(original_nick)
        if message.startswith("!help"):
            help_messages = [ "Commandes:", 
            "!register <password> <confirm_password> - Enregistre ", 
            "!auth <password> - Authentification",
            "!unauth - Désauthentification",
            "!unregister - Désinscription.",
            "!help - Aide." ] 
        for line in help_messages: connection.privmsg(original_nick, line) 
        if message.startswith("!register"):
            try:
                _, password, confirm_password = message.split()
                if password == confirm_password:
                    hashed_password = hash_password(password)
                    registered_users[nick] = hashed_password
                    save_registered_users()
                    connection.privmsg(original_nick, "You have been successfully registered!")
                else:
                    connection.privmsg(original_nick, "Passwords do not match.")
            except ValueError:
                connection.privmsg(original_nick, "Usage: !register <password> <confirm_password>")

        elif message.startswith("!auth"):
            if nick in registered_users:
                if nick not in authenticated_users:
                    authenticated_users[nick] = False

                try:
                    _, password = message.split()
                    hashed_password = hash_password(password)
                    if registered_users.get(nick) == hashed_password:
                        connection.privmsg(original_nick, "Authentication successful! Welcome!")
                        connection.mode(self.channel, f"+v {original_nick}")  # Give voice to the user
                        authenticated_users[nick] = True
                    else:
                        connection.privmsg(original_nick, "Incorrect password. Please try again.")
                except ValueError:
                    connection.privmsg(original_nick, "Usage: !auth <password>")
            else:
                connection.privmsg(original_nick, "You are not registered. Please register first using !register <password> <confirm_password>")

        elif message.startswith("!unauth"):
            if nick in authenticated_users and authenticated_users[nick]:
                connection.mode(self.channel, f"-v {original_nick}")  # Remove voice from the user
                authenticated_users[nick] = False
                connection.privmsg(original_nick, "You have been unauthenticated.")
            else:
                connection.privmsg(original_nick, "You are not currently authenticated or not registered.")

        elif message.startswith("!unregister"):
            if nick in registered_users:
                del registered_users[nick]
                save_registered_users()
                if nick in authenticated_users and authenticated_users[nick]:
                    connection.mode(self.channel, f"-v {original_nick}")  # Remove voice from the user if they were authenticated
                if nick in authenticated_users:
                    del authenticated_users[nick]
                connection.privmsg(original_nick, "You have been unregistered and unauthenticated.")
            else:
                connection.privmsg(original_nick, "You are not registered.")

def main():
    server = "irc.sample.net"  # Replace with the appropriate IRC server
    port = 6667
    channel = "#labynet"  # Replace with the appropriate channel
    nickname = "gardien"
    oper_user = "operator_username_here"  # Operator username
    oper_pass = "operator_password_here"  # Operator password

    bot = RegisterBot(channel, nickname, server, oper_user, oper_pass, port)
    bot_thread = threading.Thread(target=bot.start)
    bot_thread.start()

if __name__ == "__main__":
    main()
