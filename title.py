import socket
import subprocess
import re

# Paramètres pour le serveur IRC et le bot
SERVER = "labynet.fr"
PORT = 6667
CHANNEL = "#labynet"
NICK = "title"

# Connexion au serveur IRC
irc_socket = socket.socket(socket.AF_INET, socket.SOCK_STREAM)
irc_socket.connect((SERVER, PORT))
irc_socket.send(f"USER {NICK} {NICK} {NICK} :{NICK}\r\n".encode())
irc_socket.send(f"NICK {NICK}\r\n".encode())
irc_socket.send(f"JOIN {CHANNEL}\r\n".encode())

# Fonction pour exécuter un script AppleScript
def execute_applescript(script):
    process = subprocess.Popen(['osascript', '-e', script], stdout=subprocess.PIPE, stderr=subprocess.PIPE)
    output, error = process.communicate()
    if error:
        return None, error.decode()
    return output.decode().strip(), None

# Fonction pour rechercher une playlist par nom partiel
def find_partial_playlist(playlist_name, all_playlists):
    matches = [p for p in all_playlists if re.search(playlist_name, p, re.IGNORECASE)]
    return matches

# Boucle principale
while True:
    data = irc_socket.recv(4096).decode()
    if data.find('PING') != -1:
        irc_socket.send('PONG :pingis\n'.encode())
    elif data.find('PRIVMSG') != -1:
        user = data.split('!', 1)[0][1:]
        message = data.split('PRIVMSG', 1)[1].split(':', 1)[1].strip()

        if message.startswith('!title'):
            # result, error = execute_applescript('tell application "Music" to get {name, artist, album } of current track')
            title, error = execute_applescript('tell application "Music" to get {name } of current track')
            artist, error = execute_applescript('tell application "Music" to get { artist} of current track')
            album, error = execute_applescript('tell application "Music" to get { album } of current track')
            if title:
                # title, artist, album = result.split(", ")
                irc_socket.send(f"PRIVMSG {CHANNEL} :TITRE: {title} ARTIST: {artist} ALBUM: {album}\r\n".encode())
            else:
                irc_socket.send(f"PRIVMSG {CHANNEL} :Erreur: {error}\r\n".encode())

        elif message.startswith('!next'):
            _, error = execute_applescript('tell application "Music" to next track')
            if error:
                irc_socket.send(f"PRIVMSG {CHANNEL} :Erreur: {error}\r\n".encode())
            else:
                irc_socket.send(f"PRIVMSG {CHANNEL} :Piste suivante en cours de lecture\r\n".encode())

        elif message.startswith('!prev'):
            _, error = execute_applescript('tell application "Music" to previous track')
            if error:
                irc_socket.send(f"PRIVMSG {CHANNEL} :Erreur: {error}\r\n".encode())
            else:
                irc_socket.send(f"PRIVMSG {CHANNEL} :Piste précédente en cours de lecture\r\n".encode())

        elif message.startswith('!pause'):
            _, error = execute_applescript('tell application "Music" to pause')
            if error:
                irc_socket.send(f"PRIVMSG {CHANNEL} :Erreur: {error}\r\n".encode())
            else:
                irc_socket.send(f"PRIVMSG {CHANNEL} :La lecture de la musique a été mise en pause.\r\n".encode())

        elif message.startswith('!playlist'):
            playlist_name_match = re.search(r'!playlist\s+(.+)', message)
            if playlist_name_match:
                playlist_name = playlist_name_match.group(1)
            else:
                irc_socket.send(f"PRIVMSG {CHANNEL} :Erreur: Nom de la playlist manquant.\r\n".encode())
                continue

            all_playlists_script = 'tell application "Music" to get name of playlists'
            all_playlists, error = execute_applescript(all_playlists_script)
            if error:
                irc_socket.send(f"PRIVMSG {CHANNEL} :Erreur: {error}\r\n".encode())
            else:
                all_playlists = all_playlists.split(', ')
                partial_matches = find_partial_playlist(playlist_name, all_playlists)
                if partial_matches:
                    # Choisissons la première correspondance partielle trouvé
                    selected_playlist = partial_matches[0]
                    script = f'tell application "Music" to play playlist "{selected_playlist}"'
                    _, error = execute_applescript(script)
                    if error:
                        irc_socket.send(f"PRIVMSG {CHANNEL} :Erreur: {error}\r\n".encode())
                    else:
                        irc_socket.send(f"PRIVMSG {CHANNEL} :Lecture de la playlist : {selected_playlist}\r\n".encode())
                else:
                    irc_socket.send(f"PRIVMSG {CHANNEL} :Aucune playlist trouvée proche de : {playlist_name}\r\n".encode())
        elif message.startswith('!play'):
            _, error = execute_applescript('tell application "Music" to play')
            if error:
                irc_socket.send(f"PRIVMSG {CHANNEL} :Erreur: {error}\r\n".encode())
            else:
                irc_socket.send(f"PRIVMSG {CHANNEL} :La lecture de la musique a été démarrée.\r\n".encode())
        elif message.startswith('!help'):
            commands_list = "Liste des commandes disponibles : " \
                    "!title - !next - !prev - !pause - !play - !playlist [nom_playlist] - !track [nom_piste] - !genre [nom_genre] - !artist [nom_artiste]"
            irc_socket.send(f"PRIVMSG {CHANNEL} :{commands_list}\r\n".encode())

        elif message.startswith('!track'):
            track_name_partial = message.split('!track', 1)[1].strip()
            script = f'tell application "Music"\n'
            script += f'set found_tracks to search playlist "général" for "{track_name_partial}"\n'
            script += f'if found_tracks is not {{}} then\n'
            script += f'    play item 1 of found_tracks\n'  # Jouer la première piste trouvée
            script += f'else\n'
            script += f'    set error_message to "Piste non trouvée : {track_name_partial}"\n'
            script += f'end if\n'
            script += f'end tell\n'
            _, error = execute_applescript(script)
            if error:
                irc_socket.send(f"PRIVMSG {CHANNEL} :Erreur: {error}\r\n".encode())
            else:
                irc_socket.send(f"PRIVMSG {CHANNEL} :Lecture de la piste contenant : {track_name_partial}\r\n".encode())
        elif message.startswith('!genre'):
            genre_name = message.split('!genre', 1)[1].strip()
            script = f'set genre_name to "{genre_name}"\n'
            script += f'tell application "Music"\n'
            script += f'set genre_tracks to every track of playlist "général" whose genre is genre_name\n'
            script += f'if genre_tracks is not {{}} then\n'
            script += f'    try\n'
            script += f'        delete every playlist whose name is "temp"\n'  # Supprimer la playlist "temp" si elle existe
            script += f'    end try\n'
            script += f'    set temp_playlist to make new playlist with properties {{name:"temp"}}\n'
            script += f'    repeat with a_track in genre_tracks\n'
            script += f'        duplicate a_track to temp_playlist\n'
            script += f'    end repeat\n'
            script += f'    play temp_playlist\n'  # Jouer la playlist "temp"
            script += f'else\n'
            script += f'end if\n'
            script += f'end tell\n'

            _, error = execute_applescript(script)
            if error:
                irc_socket.send(f"PRIVMSG {CHANNEL} :Erreur: {error}\r\n".encode())
            else:
                irc_socket.send(f"PRIVMSG {CHANNEL} :Playlist temporaire créée avec les pistes du genre {genre_name}\r\n".encode())


        elif message.startswith('!artist'):
            artist_name = message.split('!artist', 1)[1].strip()
            script = f'tell application "Music"\n'
            script += f'set artist_tracks to search playlist "général" for "{artist_name}" only artists\n'
            script += f'if artist_tracks is not {{}} then\n'
            script += f'    try\n'
            script += f'        delete every playlist whose name is "temp"\n'  # Supprimer la playlist "temp" si elle existe
            script += f'    end try\n'
            script += f'    set temp_playlist to make new playlist with properties {{name:"temp"}}\n'
            script += f'    repeat with a_track in artist_tracks\n'
            script += f'        duplicate a_track to temp_playlist\n'
            script += f'    end repeat\n'
            script += f'    play temp_playlist\n'  # Jouer la playlist "temp"
            script += f'else\n'
            script += f'end if\n'
            script += f'end tell\n'

            _, error = execute_applescript(script)
            if error:
                irc_socket.send(f"PRIVMSG {CHANNEL} :Erreur: {error}\r\n".encode())
            else:
                irc_socket.send(f"PRIVMSG {CHANNEL} :Playlist temporaire créée avec les pistes de {artist_name}\r\n".encode())

        elif message.startswith('!say'):
            text_to_say = message.split(' ', 1)[1]  # Extract the text to be spoken
            subprocess.run(['say', text_to_say])  # Use the 'say' command to speak the text
