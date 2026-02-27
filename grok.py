import irc.bot
import os
import requests
import json
import time
import random
from ratelimit import limits, sleep_and_retry
import urllib.parse
import threading
import base64
from deep_translator import GoogleTranslator

class GrokBot(irc.bot.SingleServerIRCBot):
    def __init__(self, config_file="grokbot.json"):
        # Charger la config
        with open(config_file, "r") as f:
            config = json.load(f)
        
        # Variables
        self.server = config["server"]
        self.port = config["port"]
        self.channel = config["channel"]
        self.nickname = config["nickname"]
        self.grok_api_key = config["grok_api_key"]
        self.stability_api_key = config.get("stability_api_key", "")
        self.max_context_lines = config.get("max_context_lines", 10)
        self.debug_api_response = config.get("debug_api_response", True)
        self.user_contexts = []
        
        # Vérifier la clé API Stability AI
        if not self.stability_api_key:
            print("Erreur : Clé API Stability AI manquante dans grokbot.json !")
        else:
            try:
                response = requests.get(
                    "https://api.stability.ai/v1/user/balance",
                    headers={"Authorization": f"Bearer {self.stability_api_key}"}
                )
                if response.status_code == 200:
                    print("Clé API Stability AI valide. Solde de crédits :", response.json().get("credits"))
                else:
                    print("Erreur : Clé API Stability AI invalide ou compte non configuré !")
            except Exception as e:
                print(f"Erreur vérification clé API Stability AI : {str(e)}")
        
        # Dossiers pour sauvegardes et images
        if not os.path.exists("grok_contexts"):
            os.makedirs("grok_contexts")
        if not os.path.exists("/var/www/html/images"):
            os.makedirs("/var/www/html/images")
            os.chmod("/var/www/html/images", 0o755)
        
        # Init IRC
        irc.bot.SingleServerIRCBot.__init__(self, [(self.server, self.port)], self.nickname, self.nickname)
        
        # Thread pour nettoyer les images
        self.cleanup_thread = threading.Thread(target=self.run_cleanup_images)
        self.cleanup_thread.daemon = True
        self.cleanup_thread.start()

    def on_welcome(self, connection, event):
        try:
            connection.join(self.channel)
            print(f"Connecté à {self.channel}")
        except Exception as e:
            print(f"Erreur dans on_welcome : {str(e)}")

    def on_disconnect(self, connection, event):
        try:
            print(f"Déconnexion détectée : {event}. Tentative de reconnexion...")
            while True:
                try:
                    connection.connect(self.server, self.port, self.nickname)
                    connection.join(self.channel)
                    print(f"Reconnecté à {self.channel}")
                    break
                except Exception as e:
                    print(f"Erreur reconnexion : {str(e)}. Nouvelle tentative dans 5 secondes.")
                    time.sleep(5)
        except Exception as e:
            print(f"Erreur dans on_disconnect : {str(e)}")

    def on_pubmsg(self, connection, event):
        try:
            message = event.arguments[0]
            user = event.source.nick
            channel = event.target
            
            print(f"Message reçu : {message} de {user}")
            if message.startswith("!grok "):
                prompt = message.split("!grok ", 1)[1].strip()
                if not prompt:
                    connection.privmsg(channel, "Eh, donne-moi un truc à dire ! 😜")
                    return
                self.update_context(channel, user, prompt, "grok")
                response = self.generate_response(channel, user, prompt, "grok")
                self.send_message_in_chunks(connection, channel, response)
            
            elif message.startswith("!grok-image "):
                description = message.split("!grok-image ", 1)[1].strip()
                if not description:
                    connection.privmsg(channel, "Donne-moi une description pour l’image, voyons ! 🖌️")
                    return
                self.update_context(channel, user, description, "image")
                response = self.generate_image(channel, user, description)
                self.send_message_in_chunks(connection, channel, response)
            
            elif message == "!grok-joke":
                # Choisir un prompt varié aléatoirement
                joke_prompts = [
                    "Raconte une blague sur l’espace et les extraterrestres",
                    "Raconte une blague sur une planète loufoque",
                    "Raconte une blague sur un voyage intergalactique",
                    "Raconte une blague sur un trou noir avec une touche d’humour",
                    "Raconte une blague cosmique super originale"
                ]
                prompt = random.choice(joke_prompts)
                print(f"Prompt blague : {prompt}")
                self.update_context(channel, user, prompt, "joke")
                response = self.generate_response(channel, user, prompt, "joke", temperature=1.2)
                # Ajouter une intro aléatoire
                intros = [
                    "😂 Accroche-toi pour celle-ci !",
                    "😆 Prêt pour un éclat de rire cosmique ?",
                    "🚀 Une blague venue d’une autre galaxie !",
                    "🌟 Une pépite de l’univers !"
                ]
                self.send_message_in_chunks(connection, channel, f"{random.choice(intros)} {response} 🌌")
            
            elif message.startswith("!grok-insult "):
                target = message.split("!grok-insult ", 1)[1].strip()
                if not target:
                    connection.privmsg(channel, "Qui dois-je insulter, hein ? 😏")
                    return
                prompt = f"Insulte {target} dans un style médiéval"
                self.update_context(channel, user, prompt, "insult")
                response = self.generate_response(channel, user, prompt, "insult")
                self.send_message_in_chunks(connection, channel, f"💥 {target} : {response}")
            
            elif message == "!grok-raz":
                self.reset_context(channel, user)
                connection.privmsg(channel, "Contexte effacé, on repart dans l’espace ! 🚀")
            
            elif message.startswith("!grok-save "):
                title = message.split("!grok-save ", 1)[1].strip()
                self.save_context(channel, user, title)
                connection.privmsg(channel, f"Contexte sauvegardé sous '{title}'.")
            
            elif message.startswith("!grok-load "):
                title = message.split("!grok-load ", 1)[1].strip()
                self.load_context(channel, user, title)
                connection.privmsg(channel, f"Contexte '{title}' chargé.")
            
            elif message == "!grok-help":
                self.send_help(connection, channel)
        
        except Exception as e:
            print(f"Erreur dans on_pubmsg : {str(e)}")
            try:
                connection.privmsg(channel, f"Oups, erreur cosmique : {str(e)}")
            except Exception as e2:
                print(f"Erreur envoi message erreur : {str(e2)}")

    def update_context(self, channel, user, message, command_type):
        try:
            context_entry = next((entry for entry in self.user_contexts if entry[0] == channel and entry[1] == user and entry[3] == command_type), None)
            if context_entry:
                context = context_entry[2]
                context.append(message)
                if len(context) > self.max_context_lines:
                    context = context[-self.max_context_lines:]
                context_entry[2] = context
            else:
                self.user_contexts.append([channel, user, [message], command_type])
        except Exception as e:
            print(f"Erreur dans update_context : {str(e)}")

    @sleep_and_retry
    @limits(calls=60, period=3600)
    def generate_response(self, channel, user, prompt, command_type, temperature=0.8):
        try:
            context_entry = next((entry for entry in self.user_contexts if entry[0] == channel and entry[1] == user and entry[3] == command_type), None)
            if not context_entry:
                context_entry = [channel, user, ["Salut, je suis Grok !"], command_type]
                self.user_contexts.append(context_entry)
            
            context = "\n".join(context_entry[2][:-1])
            last_message = context_entry[2][-1]
            prompt_text = f"Contexte :\n{context}\n\n Répond simplement :\n{last_message}"

            headers = {'Authorization': f'Bearer {self.grok_api_key}'}
            data = {
                "model": "grok-3",
                "messages": [{"role": "user", "content": prompt_text}],
                "max_tokens": 1000,
                "temperature": temperature
            }

            response = requests.post('https://api.x.ai/v1/chat/completions', json=data, headers=headers)
            response.raise_for_status()
            result = response.json()
            if 'choices' in result and result['choices'] and 'message' in result['choices'][0] and 'content' in result['choices'][0]['message']:
                content = result['choices'][0]['message']['content'].strip()
                if content:
                    return content
                return "Désolé, j’ai buggé comme un vaisseau en panne ! 😵"
            return "Erreur : l’API m’a laissé dans le vide sidéral ! 🌌"
        except requests.exceptions.RequestException as e:
            print(f"Erreur réseau dans generate_response : {str(e)}")
            return f"Erreur réseau API : {str(e)}"
        except Exception as e:
            print(f"Erreur dans generate_response : {str(e)}")
            return f"Erreur : Impossible de générer la réponse ! 😵"
            
    @sleep_and_retry
    @limits(calls=5, period=60)
    def generate_image(self, channel, user, description):
        try:
            if not self.grok_api_key:
                return "Erreur : Clé API xAI manquante dans grokbot.json 😕"

            description = description.strip()
            if len(description) < 5:
                return "Description trop courte ! Minimum 5 caractères 🖌️"

            # Traduction en anglais → souvent meilleur résultat
            try:
                translated = GoogleTranslator(source='auto', target='en').translate(description)
                if translated and len(translated.strip()) >= 5:
                    prompt = translated
                else:
                    prompt = description
            except Exception as e:
                print(f"Traduction échouée : {e}")
                prompt = description

            print(f"Prompt pour image (xAI) : {prompt}")

            headers = {
                'Authorization': f'Bearer {self.grok_api_key}',
                'Content-Type': 'application/json'
            }

            payload = {
                "model": "grok-imagine-image",          # Modèle actuel / recommandé en fév. 2026
                "prompt": prompt,
                "n": 1,
                "aspect_ratio": "1:1",                  # Carré – remplace l'ancien "size"
                # Alternatives possibles : "16:9", "9:16", "4:3", "3:2", "2:3", "3:4", etc.
                "response_format": "url"
            }

            response = requests.post(
                'https://api.x.ai/v1/images/generations',
                headers=headers,
                json=payload,
                timeout=120                              # un peu plus long car parfois lent
            )

            if response.status_code != 200:
                try:
                    err = response.json()
                    error_msg = err.get("error", {}).get("message", "Erreur inconnue")
                    if "not supported" in error_msg.lower() and "size" in error_msg.lower():
                        error_msg += " → Utilise 'aspect_ratio' au lieu de 'size'"
                except:
                    error_msg = response.text or str(response.status_code)
                print(f"Erreur API xAI ({response.status_code}) : {error_msg}")
                return f"Erreur API xAI : {error_msg}"

            result = response.json()

            # Extraction de l'URL – plusieurs formats possibles selon l'époque
            image_url = None
            if "data" in result and result["data"] and isinstance(result["data"], list):
                image_url = result["data"][0].get("url")
            elif "images" in result and result["images"] and isinstance(result["images"], list):
                image_url = result["images"][0].get("url")
            elif isinstance(result, dict) and "url" in result:
                image_url = result.get("url")

            if not image_url:
                if self.debug_api_response:
                    print(f"Réponse API inattendue : {json.dumps(result, indent=2)}")
                return "Erreur : Impossible d’extraire l’URL de l’image depuis la réponse API 😵"

            # Téléchargement de l'image
            img_response = requests.get(image_url, timeout=40)
            img_response.raise_for_status()

            timestamp = int(time.time())
            filename = f"image_{user}_{timestamp}.png"
            output_path = os.path.join("/var/www/html/images", filename)

            with open(output_path, "wb") as f:
                f.write(img_response.content)

            os.chmod(output_path, 0o644)

            encoded_filename = urllib.parse.quote(filename)
            hosted_url = f"https://labynet.fr/images/{encoded_filename}"

            return f"Image générée pour « {description} » : {hosted_url} 🖼️ (Grok Imagine)"

        except requests.exceptions.HTTPError as e:
            error_msg = "Erreur HTTP inattendue"
            try:
                err_json = e.response.json()
                error_msg = err_json.get("error", {}).get("message", str(e))
            except:
                error_msg = str(e)
            print(f"Erreur HTTP xAI image : {error_msg}")
            return f"Erreur API xAI : {error_msg}"

        except Exception as e:
            print(f"Erreur génération image : {str(e)}")
            return f"Erreur inattendue lors de la génération : {str(e)} 😵"
               
    def cleanup_images(self):
        try:
            now = time.time()
            for filename in os.listdir("/var/www/html/images"):
                filepath = os.path.join("/var/www/html/images", filename)
                if os.path.getctime(filepath) < now - 86400:  # 24h
                    try:
                        os.remove(filepath)
                        print(f"Image supprimée : {filepath}")
                    except Exception as e:
                        print(f"Erreur suppression image {filepath} : {str(e)}")
        except Exception as e:
            print(f"Erreur dans cleanup_images : {str(e)}")

    def run_cleanup_images(self):
        try:
            while True:
                self.cleanup_images()
                time.sleep(3600)  # Toutes les heures
        except Exception as e:
            print(f"Erreur dans run_cleanup_images : {str(e)}")

    def reset_context(self, channel, user):
        try:
            self.user_contexts = [entry for entry in self.user_contexts if not (entry[0] == channel and entry[1] == user)]
            self.user_contexts.append([channel, user, ["Salut, je suis Grok !"], "grok"])
        except Exception as e:
            print(f"Erreur dans reset_context : {str(e)}")

    def save_context(self, channel, user, title):
        try:
            context_entry = next((entry for entry in self.user_contexts if entry[0] == channel and entry[1] == user and entry[3] == "grok"), None)
            if context_entry:
                filename = os.path.join("grok_contexts", f"{user}.{title}.json")
                with open(filename, "w") as f:
                    json.dump(context_entry[2], f)
            else:
                raise ValueError("Aucun contexte à sauvegarder.")
        except Exception as e:
            print(f"Erreur dans save_context : {str(e)}")

    def load_context(self, channel, user, title):
        try:
            filename = os.path.join("grok_contexts", f"{user}.{title}.json")
            if os.path.exists(filename):
                with open(filename, "r") as f:
                    context = json.load(f)
                context_entry = next((entry for entry in self.user_contexts if entry[0] == channel and entry[1] == user and entry[3] == "grok"), None)
                if context_entry:
                    context_entry[2] = context
                else:
                    self.user_contexts.append([channel, user, [context], "grok"])
            else:
                raise ValueError(f"Contexte '{title}' introuvable.")
        except Exception as e:
            print(f"Erreur dans load_context : {str(e)}")

    def send_help(self, connection, channel):
        try:
            help_text = (
                "🚀 Commandes GrokBot :\n"
                "- !grok <prompt> : Parle avec moi, Grok !\n"
                "- !grok-image <description> : Génère une image à partir d’une description\n"
                "- !grok-joke : Une blague cosmique\n"
                "- !grok-insult <pseudo> : Une insulte intergalactique\n"
                "- !grok-raz : Efface le contexte\n"
                "- !grok-save <titre> : Sauvegarde la convo\n"
                "- !grok-load <titre> : Charge une convo\n"
                "- !grok-help : Cette aide"
            )
            self.send_message_in_chunks(connection, channel, help_text)
        except Exception as e:
            print(f"Erreur dans send_help : {str(e)}")

    def send_message_in_chunks(self, connection, target, message):
        try:
            if not message or message.strip() == "":
                connection.privmsg(target, "Oups, j’ai rien à dire ! 😶")
                return
            lines = message.split('\n')
            for line in lines:
                line = line.strip()
                if not line:
                    continue
                print(f"Envoi message : {line}")
                if len(line.encode('utf-8')) <= 392:
                    connection.privmsg(target, line)
                else:
                    while line:
                        if len(line.encode('utf-8')) > 392:
                            last_space = line[:392].rfind(' ')
                            if last_space == -1:
                                connection.privmsg(target, line[:392])
                                line = line[392:]
                            else:
                                connection.privmsg(target, line[:last_space])
                                line = line[last_space:].strip()
                        else:
                            connection.privmsg(target, line)
                            line = ''
                        time.sleep(1)
        except Exception as e:
            print(f"Erreur dans send_message_in_chunks : {str(e)}")

if __name__ == "__main__":
    try:
        bot = GrokBot()
        bot.start()
    except Exception as e:
        print(f"Erreur critique dans main : {str(e)}")
        time.sleep(5)
        os._exit(1)
