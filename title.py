import irc.bot
import irc.strings
import subprocess
import re
import threading
import time

class MusicBot(irc.bot.SingleServerIRCBot):
    def __init__(self, server, port, channel, nickname):
        irc.bot.SingleServerIRCBot.__init__(self, [(server, port)], nickname, nickname)
        self.channel = channel
        self.current_title = None
        self.polling_thread = threading.Thread(target=self.poll_current_track)
        self.polling_thread.daemon = True

    def on_welcome(self, connection, event):
        connection.join(self.channel)
        self.polling_thread.start()

    def on_join(self, connection, event):
        connection.privmsg(self.channel, "Bonjour ! Tapez !help pour obtenir la liste des commandes disponibles.")

    def on_privmsg(self, connection, event):
        self.do_command(event, event.arguments[0])

    def on_pubmsg(self, connection, event):
        self.do_command(event, event.arguments[0])

    def do_command(self, event, command):
        nick = event.source.nick
        if nick == self.connection.get_nickname():
            return  # Ignore les messages provenant du bot lui-même

        connection = self.connection

        if command.startswith('!title'):
            self.handle_title_command(connection)
        elif command.startswith('!next'):
            self.handle_next_command(connection)
        elif command.startswith('!prev'):
            self.handle_prev_command(connection)
        elif command.startswith('!pause'):
            self.handle_pause_command(connection)
        elif command.startswith('!play'):
            self.handle_play_command(connection)
        elif command.startswith('!playlist'):
            self.handle_playlist_command(connection, command)
        elif command.startswith('!track'):
            self.handle_track_command(connection, command)
        elif command.startswith('!genre'):
            self.handle_genre_command(connection, command)
        elif command.startswith('!artist'):
            self.handle_artist_command(connection, command)
        elif command.startswith('!say'):
            self.handle_say_command(connection, command)
        elif command.startswith('!help'):
            self.handle_help_command(connection)
        else:
            pass  # Ne rien faire si la commande n'est pas reconnue

    def execute_applescript(self, script):
        process = subprocess.Popen(['osascript', '-e', script], stdout=subprocess.PIPE, stderr=subprocess.PIPE)
        output, error = process.communicate()
        if error:
            return None, error.decode()
        return output.decode().strip(), None

    def poll_current_track(self):
        while True:
            title, error = self.execute_applescript('tell application "Music" to get name of current track')
            if title and title != self.current_title:
                self.current_title = title
                artist, _ = self.execute_applescript('tell application "Music" to get artist of current track')
                album, _ = self.execute_applescript('tell application "Music" to get album of current track')
                self.connection.privmsg(self.channel, f"Nouvelle chanson : TITRE: {title} ARTIST: {artist} ALBUM: {album}")
            time.sleep(5)

    def handle_title_command(self, connection):
        title, error = self.execute_applescript('tell application "Music" to get name of current track')
        artist, error = self.execute_applescript('tell application "Music" to get artist of current track')
        album, error = self.execute_applescript('tell application "Music" to get album of current track')
        if title:
            connection.privmsg(self.channel, f"TITRE: {title} ARTIST: {artist} ALBUM: {album}")
        else:
            connection.privmsg(self.channel, f"Erreur: {error}")

    def handle_next_command(self, connection):
        _, error = self.execute_applescript('tell application "Music" to next track')
        if error:
            connection.privmsg(self.channel, f"Erreur: {error}")
        else:
            connection.privmsg(self.channel, "Piste suivante en cours de lecture")

    def handle_prev_command(self, connection):
        _, error = self.execute_applescript('tell application "Music" to previous track')
        if error:
            connection.privmsg(self.channel, f"Erreur: {error}")
        else:
            connection.privmsg(self.channel, "Piste précédente en cours de lecture")

    def handle_pause_command(self, connection):
        _, error = self.execute_applescript('tell application "Music" to pause')
        if error:
            connection.privmsg(self.channel, f"Erreur: {error}")
        else:
            connection.privmsg(self.channel, "La lecture de la musique a été mise en pause.")

    def handle_play_command(self, connection):
        _, error = self.execute_applescript('tell application "Music" to play')
        if error:
            connection.privmsg(self.channel, f"Erreur: {error}")
        else:
            connection.privmsg(self.channel, "La lecture de la musique a été démarrée.")

    def handle_playlist_command(self, connection, command):
        playlist_name_match = re.search(r'!playlist\s+(.+)', command)
        if playlist_name_match:
            playlist_name = playlist_name_match.group(1)
        else:
            connection.privmsg(self.channel, "Erreur: Nom de la playlist manquant.")
            return

        all_playlists_script = 'tell application "Music" to get name of playlists'
        all_playlists, error = self.execute_applescript(all_playlists_script)
        if error:
            connection.privmsg(self.channel, f"Erreur: {error}")
        else:
            all_playlists = all_playlists.split(', ')
            partial_matches = self.find_partial_playlist(playlist_name, all_playlists)
            if partial_matches:
                selected_playlist = partial_matches[0]
                script = f'tell application "Music" to play playlist "{selected_playlist}"'
                _, error = self.execute_applescript(script)
                if error:
                    connection.privmsg(self.channel, f"Erreur: {error}")
                else:
                    connection.privmsg(self.channel, f"Lecture de la playlist : {selected_playlist}")
            else:
                connection.privmsg(self.channel, f"Aucune playlist trouvée proche de : {playlist_name}")

    def handle_track_command(self, connection, command):
        track_name_partial = command.split('!track', 1)[1].strip()
        script = f'tell application "Music"\n'
        script += f'set found_tracks to search playlist "général" for "{track_name_partial}"\n'
        script += f'if found_tracks is not {{}} then\n'
        script += f'    play item 1 of found_tracks\n'
        script += f'else\n'
        script += f'    set error_message to "Piste non trouvée : {track_name_partial}"\n'
        script += f'end if\n'
        script += f'end tell\n'
        _, error = self.execute_applescript(script)
        if error:
            connection.privmsg(self.channel, f"Erreur: {error}")
        else:
            connection.privmsg(self.channel, f"Lecture de la piste contenant : {track_name_partial}")

    def handle_genre_command(self, connection, command):
        genre_name = command.split('!genre', 1)[1].strip()
        script = f'set genre_name to "{genre_name}"\n'
        script += f'tell application "Music"\n'
        script += f'set genre_tracks to every track of playlist "général" whose genre is genre_name\n'
        script += f'if genre_tracks is not {{}} then\n'
        script += f'    try\n'
        script += f'        delete every playlist whose name is "temp"\n'
        script += f'    end try\n'
        script += f'    set temp_playlist to make new playlist with properties {{name:"temp"}}\n'
        script += f'    repeat with a_track in genre_tracks\n'
        script += f'        duplicate a_track to temp_playlist\n'
        script += f'    end repeat\n'
        script += f'    play temp_playlist\n'
        script += f'else\n'
        script += f'end if\n'
        script += f'end tell\n'
        _, error = self.execute_applescript(script)
        if error:
            connection.privmsg(self.channel, f"Erreur: {error}")
        else:
            connection.privmsg(self.channel, f"Playlist temporaire créée avec les pistes du genre {genre_name}")

    def handle_artist_command(self, connection, command):
        artist_name = command.split('!artist', 1)[1].strip()
        script = f'tell application "Music"\n'
        script += f'set artist_tracks to search playlist "général" for "{artist_name}" only artists\n'
        script += f'if artist_tracks is not {{}} then\n'
        script += f'    try\n'
        script += f'        delete every playlist whose name is "temp"\n'
        script += f'    end try\n'
        script += f'    set temp_playlist to make new playlist with properties {{name:"temp"}}\n'
        script += f'    repeat with a_track in artist_tracks\n'
        script += f'        duplicate a_track to temp_playlist\n'
        script += f'    end repeat\n'
        script += f'    play temp_playlist\n'
        script += f'else\n'
        script += f'end if\n'
        script += f'end tell\n'
        _, error = self.execute_applescript(script)
        if error:
            connection.privmsg(self.channel, f"Erreur: {error}")
        else:
            connection.privmsg(self.channel, f"Playlist temporaire créée avec les pistes de {artist_name}")

    def handle_say_command(self, connection, command):
        text_to_say = command.split(' ', 1)[1]
        set_volume_script = 'tell application "Music" to set sound volume to 50'
        _, error = self.execute_applescript(set_volume_script)
        if error:
            connection.privmsg(self.channel, f"Erreur lors de la modification du volume : {error}")
        else:
            subprocess.run(['say', text_to_say])
            set_volume_script = 'tell application "Music" to set sound volume to 100'
            _, error = self.execute_applescript(set_volume_script)
            if error:
                connection.privmsg(self.channel, f"Erreur lors de la modification du volume : {error}")

    def handle_help_command(self, connection):
        help_message = "Commandes disponibles : !title, !next, !prev, !pause, !play, !playlist <nom>, !track <nom>, !genre <nom>, !artist <nom>, !say <message>"
        connection.privmsg(self.channel, help_message)

if __name__ == "__main__":
    server = "labynet.fr"
    port = 6667
    channel = "#labynet"
    nickname = "title"
    bot = MusicBot(server, port, channel, nickname)
    bot.start()
