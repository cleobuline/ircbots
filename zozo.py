import irc.bot
import openai
import json
import os
import requests
import pyshorteners
import imgbbpy
import time
import threading
from pylatexenc.latex2text import LatexNodes2Text


class ChatGPTBot(irc.bot.SingleServerIRCBot):
    def __init__(self, config_file):
        # Charger la configuration
        with open(config_file, "r") as f:
            config = json.load(f)

        # Initialiser les variables
        self.channel_list = [channel.strip() for channel in config["channels"].split(",")]
        self.nickname = config["nickname"]
        self.server = config["server"]
        self.port = config["port"]
        self.api_key = config["api_key"]
        self.max_num_line = config["max_num_line"]
        self.imgbb_api_key = config["imgbb_api_key"]
        self.web_url = config["display_url"]
        self.image_filename = config["image_filename"]
        self.admin_user = config["admin_user"]
        self.grok_api_key = config["grok_api_key"]

        self.blocked_users = set()
        self.user_contexts = []  # List to store context information
        self.model = "gpt-4o-mini"
        self.tag = ""

        openai.api_key = self.api_key
        self.imgbb_client = imgbbpy.SyncClient(self.imgbb_api_key)

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

        # Commande Grok
        if message.startswith("grok"):
            prompt = message.split("grok", 1)[1].strip()
            self.update_context(channel, user, prompt)
            response = self.generate_response_grok(channel, user, prompt)
            if response and response.strip():
                self.send_message_in_chunks(connection, channel, response)

        # Appel direct au bot, style "Zozo: ..."
        elif message.strip().startswith(bot_nickname + ":"):
            message = message[len(bot_nickname) + 1:].strip()
            command, args = self.parse_command(message)

            if command == "tag":
                self.tag = args  # Mettre à jour le tag
                connection.privmsg(channel, f"Tag mis à jour: {self.tag}")

            elif command == "help":
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

            elif command == "model":
                self.change_model(channel, user, args)

            elif command == "list-models":
                self.list_models(channel)

            elif command == "current":
                connection.privmsg(channel, f"Modèle actuel utilisé : {self.model}")

            elif command == "local":
                self.generate_image_local(connection, channel, args)

            elif command == "image":
                self.generate_image_tiny(connection, channel, args)

            elif command == "imgbb":
                self.generate_image_imgbb(connection, channel, args)

            elif command == "vision":
                self.generate_image_description(connection, channel, args)
                
            elif command == "video":
                self.generate_video_sora(connection, channel, args)
            elif command == "url":
                self.summarize_url(connection, channel, args)

            else:
                if user in self.blocked_users:
                    connection.privmsg(channel, "Vous êtes bloqué et ne pouvez pas recevoir de réponses.")
                else:
                    self.update_context(channel, user, message)
                    response = self.generate_response(channel, user, message)
                    if response and response.strip():
                        self.send_message_in_chunks(connection, channel, response)

    def parse_command(self, message):
        parts = message.strip().split(" ", 1)
        command = parts[0].lower()
        args = parts[1] if len(parts) > 1 else ""
        return command, args

    def send_help_message(self, connection, channel):
        help_message = (
            "'raz' oublie la conversation, 'save [titre]', 'load [titre]', "
            "'delete [titre]', 'files' liste les conversations, 'block [user]' "
            "bloque un utilisateur, 'unblock [user]' débloque un utilisateur, "
            "'model [model_name]' pour changer le modèle, 'list-models' liste les modèles valides, "
            "'image [prompt]' pour générer une image, 'vision [image URL]' pour décrire une image, "
            "'url [URL]' pour décrire une page web."
        )
        connection.privmsg(channel, help_message)

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

    def generate_response(self, channel, user, message):
        context_entry = next((entry for entry in self.user_contexts if entry[0] == channel and entry[1] == user), None)

        if not context_entry:
            return ""

        context = "\n".join(context_entry[2][:-1])
        last_message = context_entry[2][-1]
        prompt_text = (
            f"Contexte:\n{context}\n\n"
            f"Répond seulement à la dernière ligne en tenant compte du contexte précédent.\n"
            f"Dernière ligne: {last_message}"
        )

        try:
            response = openai.ChatCompletion.create(
                model=self.model,
                messages=[{"role": "user", "content": prompt_text}]
            )
            generated_text = response.choices[0].message.content.strip()
            # Appliquer un éventuel tag si tu veux le réactiver :
            # if self.tag:
            #     generated_text = f"{self.tag} {generated_text}"
            readable_text = LatexNodes2Text().latex_to_text(generated_text)
            return readable_text
        except openai.error.OpenAIError as e:
            return f"[Erreur OpenAI: {e}]"
        except Exception as e:
            return f"[Erreur inattendue: {e}]"

    def generate_response_grok(self, channel, user, message):
        api_key = self.grok_api_key
        context_entry = next((entry for entry in self.user_contexts if entry[0] == channel and entry[1] == user), None)
        headers = {'Authorization': f'Bearer {api_key}'}

        if not context_entry:
            return ""

        context = "\n".join(context_entry[2][:-1])
        last_message = context_entry[2][-1]

        prompt_text = (
            f"Contexte:\n{context}\n\n"
            f"Répond seulement à la dernière ligne en tenant compte du contexte précédent.\n"
            f"Dernière ligne: {last_message}"
        )

        data = {
            "model": "grok-3",
            "messages": [{"role": "user", "content": prompt_text}],
            "max_tokens": 500,
            "temperature": 0.7
        }

        try:
            response = requests.post('https://api.x.ai/v1/chat/completions', json=data, headers=headers, timeout=20)
            data = response.json()
            if 'choices' in data and data['choices']:
                generated_text = data['choices'][0]['message']['content']
            else:
                generated_text = "[Grok] Erreur ou réponse non disponible."
            readable_text = LatexNodes2Text().latex_to_text(generated_text)
            return readable_text
        except requests.exceptions.RequestException as e:
            return f"[Erreur Grok (HTTP): {e}]"
        except Exception as e:
            return f"[Erreur Grok inattendue: {e}]"

    def reset_user_context(self, channel, user):
        context_entry = next((entry for entry in self.user_contexts if entry[0] == channel and entry[1] == user), None)
        if context_entry:
            context_entry[2] = ["Bonjour"]
        else:
            self.user_contexts.append([channel, user, ["Bonjour"]])

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
        prefix = f"{user}."
        suffix = ".context.json"
        files = []
        for filename in os.listdir("conversations"):
            if filename.startswith(prefix) and filename.endswith(suffix):
                title = filename[len(prefix):-len(suffix)]
                files.append(title)
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

    def change_model(self, channel, user, model):
        valid_models = [
            "gpt-3.5-turbo", "gpt-4", "gpt-5",
            "o1-mini", "o1-preview", "o1", "o3-mini",
            "gpt-4o", "gpt-4.5-preview", "gpt-4o-2024-08-06",
            "gpt-4o-mini", "gpt-3.5-turbo-16k", "gpt-4-16k"
        ]
        if model in valid_models:
            self.model = model
            self.connection.privmsg(channel, f"Modèle changé à {model}.")
        else:
            self.connection.privmsg(channel, f"Modèle {model} invalide. Modèles valides : {', '.join(valid_models)}.")

    def list_models(self, channel):
        valid_models = [
            "gpt-3.5-turbo", "gpt-4", "gpt-5",
            "o1-mini", "o1-preview", "o1", "o3-mini",
            "gpt-4o", "gpt-4.5-preview", "gpt-4o-2024-08-06",
            "gpt-4o-mini", "gpt-3.5-turbo-16k", "gpt-4-16k"
        ]
        self.connection.privmsg(channel, f"Modèles valides : {', '.join(valid_models)}.")

    def summarize_url(self, connection, channel, url):
        try:
            if not url.startswith("http"):
                url = "http://" + url

            # Récupérer le contenu de l'URL
            response = requests.get(url, timeout=10, headers={"User-Agent": "Mozilla/5.0"})
            response.raise_for_status()
            content = response.text

            prompt = (
                f"Voici le contenu d'une page web : {content[:4000]} \n\n"
                f"Résume ce contenu en quelques phrases claires et concises."
            )
            ai_response = openai.ChatCompletion.create(
                model=self.model,
                messages=[{"role": "user", "content": prompt}]
            )

            summary = ai_response['choices'][0]['message']['content'].strip()
            self.send_message_in_chunks(connection, channel, f"Résumé de la page : {summary}")
        except requests.exceptions.RequestException as e:
            connection.privmsg(channel, f"Erreur lors de la récupération de l'URL : {e}")
        except openai.error.OpenAIError as e:
            connection.privmsg(channel, f"Erreur lors de la génération du résumé : {e}")
        except Exception as e:
            connection.privmsg(channel, f"Une erreur inattendue est survenue : {e}")

    def generate_image_tiny(self, connection, channel, prompt):
        try:
            response = openai.Image.create(
                model="dall-e-3",
                prompt=prompt,
                n=1,
                size="1024x1024"
            )
            image_url = response['data'][0]['url']
            shortener = pyshorteners.Shortener()
            short_url = shortener.tinyurl.short(image_url)
            connection.privmsg(channel, short_url)
        except openai.error.OpenAIError as e:
            connection.privmsg(channel, f"Erreur lors de la génération de l'image: {str(e)}")
        except Exception as e:
            connection.privmsg(channel, f"Une erreur inattendue est survenue: {str(e)}")

    def generate_image_imgbb(self, connection, channel, prompt):
        try:
            response = openai.Image.create(
                model="dall-e-3",
                prompt=prompt,
                n=1,
                size="1024x1024"
            )
            image_url = response['data'][0]['url']

            # Télécharger l'image
            image_data = requests.get(image_url, timeout=20).content

            # Enregistrer l'image temporairement
            temp_image_path = 'temp_image.png'
            with open(temp_image_path, 'wb') as f:
                f.write(image_data)

            imgbb_response = self.imgbb_client.upload(file=temp_image_path)
            os.remove(temp_image_path)

            short_url = imgbb_response.url
            connection.privmsg(channel, short_url)
        except openai.error.OpenAIError as e:
            connection.privmsg(channel, f"Erreur lors de la génération de l'image: {str(e)}")
        except requests.exceptions.RequestException as e:
            connection.privmsg(channel, f"Erreur de téléchargement de l'image: {str(e)}")
        except imgbbpy.exceptions.ImgBBError as e:
            connection.privmsg(channel, f"Erreur lors du téléchargement sur ImgBB: {str(e)}")
        except Exception as e:
            connection.privmsg(channel, f"Une erreur inattendue est survenue: {str(e)}")

    def generate_image_local(self, connection, channel, prompt):
        try:
            response = openai.Image.create(
                model="dall-e-3",
                prompt=prompt,
                n=1,
                size="1024x1024"
            )
            image_url = response['data'][0]['url']

            # Télécharger l'image
            image_response = requests.get(image_url, timeout=20)

            # Sauvegarder localement dans le fichier défini dans la config
            with open(self.image_filename, "wb") as image_file:
                image_file.write(image_response.content)

            # Envoyer l'URL publique (ton site web) où l’image est affichée
            connection.privmsg(channel, self.web_url)
        except openai.error.OpenAIError as e:
            connection.privmsg(channel, f"Erreur lors de la génération de l'image: {str(e)}")
        except requests.exceptions.RequestException as e:
            connection.privmsg(channel, f"Erreur de téléchargement de l'image: {str(e)}")
        except Exception as e:
            connection.privmsg(channel, f"Une erreur inattendue est survenue: {str(e)}")

    def generate_image_description(self, connection, channel, image_url):
        try:
            response = openai.ChatCompletion.create(
                model="gpt-4o-mini",
                messages=[
                    {
                        "role": "user",
                        "content": [
                            {"type": "text", "text": "Décrit moi cette image en détails"},
                            {
                                "type": "image_url",
                                "image_url": {"url": image_url},
                            },
                        ],
                    }
                ],
                max_tokens=500,
            )

            # Compatibilité object / dict selon la version du client
            msg = response.choices[0].message
            if isinstance(msg, dict):
                description = msg.get("content", "")
            else:
                description = getattr(msg, "content", "")

            if not description:
                description = "[Impossible de décrire l'image]"

            self.send_message_in_chunks(connection, channel, f"Description de l'image : {description}")
        except openai.error.OpenAIError as e:
            connection.privmsg(channel, f"Erreur lors de l'appel à l'API OpenAI: {str(e)}")
        except Exception as e:
            connection.privmsg(channel, f"Une erreur inattendue est survenue: {str(e)}")
            


    def generate_video_sora(self, connection, channel, prompt):
        if not prompt.strip():
            connection.privmsg(channel, "Prompt vide ! Exemple : video un chien qui court après une balle dans un parc")
            return

        # Lancement en arrière-plan pour ne plus jamais avoir de Ping timeout
        thread = threading.Thread(target=self._generate_video_worker, args=(connection, channel, prompt.strip()), daemon=True)
        thread.start()
        
        connection.privmsg(channel, "Sora-2 en cours de génération… je reviens avec la vidéo dès qu’elle est prête (30-120s)")

 
 
    def _generate_video_worker(self, connection, channel, prompt):
        api_key = self.api_key
        headers_json = {
            "Authorization": f"Bearer {api_key}",
            "Content-Type": "application/json",
        }

        try:
            # Étape 1 : création du job vidéo
            payload = {
                "model": "sora-2",
                "prompt": prompt.strip(),
                # optionnel :
                 "seconds": "10",
                 "size": "1280x720",
            }
            r = requests.post(
                "https://api.openai.com/v1/videos",
                headers=headers_json,
                json=payload,
                timeout=60,
            )
            r.raise_for_status()
            data = r.json()
            video_id = data["id"]
            self.safe_privmsg(connection, channel, f"Sora-2 → job {video_id[-10:]} en file d’attente")

            # Étape 2 : polling de l’état
            start_time = time.time()
            completed = False

            while time.time() - start_time < 900:  # 15 minutes max
                time.sleep(7)

                try:
                    resp = requests.get(
                        f"https://api.openai.com/v1/videos/{video_id}",
                        headers={"Authorization": f"Bearer {api_key}"},
                        timeout=60,
                    )
                    resp.raise_for_status()
                    v = resp.json()
                except Exception:
                    continue  # glitch réseau → on retente

                status = v.get("status", "").lower()
                progress = v.get("progress", None)

                if status in ["queued", "in_progress"]:
                    if progress is not None:
                        self.safe_privmsg(connection, channel, f"{progress}% – en cours…")
                    continue

                if status == "failed":
                    error_msg = (v.get("error") or {}).get("message", "Erreur inconnue")
                    self.safe_privmsg(connection, channel, f"✖ Échec Sora-2 : {error_msg}")
                    return

                if status == "completed":
                    if not completed:
                        completed = True
                        self.safe_privmsg(connection, channel, "Vidéo générée ! Téléchargement du MP4…")

                    # Étape 3 : téléchargement binaire de la vidéo
                    try:
                        resp_video = requests.get(
                            f"https://api.openai.com/v1/videos/{video_id}/content",
                            headers={"Authorization": f"Bearer {api_key}"},
                            params={"variant": "video"},
                            timeout=300,
                            stream=True,
                        )
                        resp_video.raise_for_status()
                    except Exception as e:
                        self.safe_privmsg(connection, channel, f"✖ Erreur download Sora-2 : {e}")
                        return

                    # ADAPTE ICI : chemin réel servi par Apache/Nginx
                    local_dir = "/var/www/html/sora"
                    os.makedirs(local_dir, exist_ok=True)
                    filename = f"{video_id}.mp4"
                    local_path = os.path.join(local_dir, filename)

                    with open(local_path, "wb") as f:
                        for chunk in resp_video.iter_content(chunk_size=8192):
                            if chunk:
                                f.write(chunk)

                    # ADAPTE ICI : ton domaine/URL publique
                    public_url = f"https://new.labynet.fr/sora/{filename}"

                    # Raccourcisseur optionnel
                    try:
                        shortener = pyshorteners.Shortener()
                        public_url_short = shortener.tinyurl.short(public_url)
                    except Exception:
                        public_url_short = public_url

                    self.safe_privmsg(connection, channel, f"✔ Vidéo Sora-2 prête ! → {public_url_short}")
                    return

            # Timeout final
            self.safe_privmsg(connection, channel, "⏰ Timeout 15 min – Sora-2 trop lent ou bloqué")

        except Exception as e:
            self.safe_privmsg(connection, channel, f"✖ Erreur fatale Sora-2 : {str(e)}")

    def safe_privmsg(self, connection, target, message):
        try:
            connection.privmsg(target, message)
        except:
            pass
            
    def send_message_in_chunks(self, connection, target, message):
        if not message:
            return

        lines = message.split('\n')
        for line in lines:
            line = line.strip()
            if not line:
                continue

            if len(line.encode('utf-8')) <= 392:
                connection.privmsg(target, line)
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
