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
        self.polling_active = threading.Event()
        self.polling_active.set()  # Start with polling active

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
            return  # Ignore messages from the bot itself

        connection = self.connection

        if command.startswith('!title'):
            self.handle_title_command(connection)
        elif command.startswith('!next'):
            self.handle_next_command(connection)
        elif command.startswith('!prev'):
            self.handle_prev_command(connection)
        elif command.startswith('!pause'):
            self.handle_pause_command(connection)
        elif command.startswith('!playlist'):
            self.handle_playlist_command(connection, command)
        elif command.startswith('!play'):
            self.handle_play_command(connection)
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
        elif command.startswith('!polloff'):
            self.handle_polloff_command(connection)
        elif command.startswith('!pollon'):
            self.handle_pollon_command(connection)
        else:
            pass  # Do nothing if the command is not recognized

    def execute_applescript(self, script):
        try:
            process = subprocess.Popen(['osascript', '-e', script], stdout=subprocess.PIPE, stderr=subprocess.PIPE)
            output, error = process.communicate()
            if error:
                return None, error.decode().strip()
            return output.decode().strip(), None
        except Exception as e:
            return None, str(e)

    def poll_current_track(self):
        while True:
            self.polling_active.wait()  # Wait here if polling is paused
            title, error = self.execute_applescript('tell application "Music" to get name of current track')
            if title and title != self.current_title:
                self.current_title = title
                artist, _ = self.execute_applescript('tell application "Music" to get artist of current track')
                album, _ = self.execute_applescript('tell application "Music" to get album of current track')
                self.connection.privmsg(self.channel, f"http://labynet.fr:8000/stream : TITRE: {title} ARTIST: {artist} ALBUM: {album}")
            time.sleep(5)

    def handle_title_command(self, connection):
        title, error = self.execute_applescript('tell application "Music" to get name of current track')
        artist, _ = self.execute_applescript('tell application "Music" to get artist of current track')
        album, _ = self.execute_applescript('tell application "Music" to get album of current track')
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
        script = (
            f'tell application "Music"\n'
            f'set found_tracks to search playlist "général" for "{track_name_partial}"\n'
            f'if found_tracks is not {{}} then\n'
            f'    play item 1 of found_tracks\n'
            f'else\n'
            f'    set error_message to "Piste non trouvée : {track_name_partial}"\n'
            f'end if\n'
            f'end tell\n'
        )
        _, error = self.execute_applescript(script)
        if error:
            connection.privmsg(self.channel, f"Erreur: {error}")
        else:
            connection.privmsg(self.channel, f"Lecture de la piste contenant : {track_name_partial}")

    def handle_genre_command(self, connection, command):
        genre_name = command.split('!genre', 1)[1].strip()
        script = (
            f'set genre_name to "{genre_name}"\n'
            f'tell application "Music"\n'
            f'set genre_tracks to every track of playlist "général" whose genre is genre_name\n'
            f'if genre_tracks is not {{}} then\n'
            f'    try\n'
            f'        delete every playlist whose name is "temp"\n'
            f'    end try\n'
            f'    set temp_playlist to make new playlist with properties {{name:"temp"}}\n'
            f'    repeat with a_track in genre_tracks\n'
            f'        duplicate a_track to temp_playlist\n'
            f'    end repeat\n'
            f'    play temp_playlist\n'
            f'else\n'
            f'end if\n'
            f'end tell\n'
        )
        _, error = self.execute_applescript(script)
        if error:
            connection.privmsg(self.channel, f"Erreur: {error}")
        else:
            connection.privmsg(self.channel, f"Playlist temporaire créée avec les pistes du genre {genre_name}")

    def handle_artist_command(self, connection, command):
        artist_name = command.split('!artist', 1)[1].strip()
        script = (
            f'tell application "Music"\n'
            f'set artist_tracks to search playlist "général" for "{artist_name}" only artists\n'
            f'if artist_tracks is not {{}} then\n'
            f'    try\n'
            f'        delete every playlist whose name is "temp"\n'
            f'    end try\n'
            f'    set temp_playlist to make new playlist with properties {{name:"temp"}}\n'
            f'    repeat with a_track in artist_tracks\n'
            f'        duplicate a_track to temp_playlist\n'
            f'    end repeat\n'
            f'    play temp_playlist\n'
            f'else\n'
            f'end if\n'
            f'end tell\n'
        )
        _, error = self.execute_applescript(script)
        if error:
            connection.privmsg(self.channel, f"Erreur: {error}")
        else:
            connection.privmsg(self.channel, f"Playlist temporaire créée avec les pistes de {artist_name}")

    def handle_say_command(self, connection, command):
        text_to_say = command.split(' ', 1)[1]
        if not text_to_say:
            connection.privmsg(self.channel, "Erreur: Message manquant.")
            return
        
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
        help_message = (
            "Commandes disponibles : !title, !next, !prev, !pause, !play, !playlist <nom>, !track <nom>, "
            "!genre <nom>, !artist <nom>, !say <message>, !polloff, !pollon"
        )
        connection.privmsg(self.channel, help_message)

    def handle_polloff_command(self, connection):
        self.polling_active.clear()
        connection.privmsg(self.channel, "Polling arrêté.")

    def handle_pollon_command(self, connection):
        self.polling_active.set()
        connection.privmsg(self.channel, "Polling démarré.")

    def find_partial_playlist(self, partial_name, playlists):
        partial_name_lower = partial_name.lower()
        return [playlist for playlist in playlists if partial_name_lower in playlist.lower()]

    def stop_polling_thread(self):
        self.polling_thread_running = False
        self.polling_active.set()  # Ensure the polling thread exits if waiting
        self.polling_thread.join()

    def start(self):
        self.polling_thread_running = True
        super().start()

    def stop(self):
        self.stop_polling_thread()
        super().disconnect("Bot is stopping")


if __name__ == "__main__":
    server = "labynet.fr"
    port = 6667
    channel = "#labynet"
    nickname = "title"
    bot = MusicBot(server, port, channel, nickname)
    bot.start()
