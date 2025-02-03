import socket
import openai
import re
import os
from collections import deque
import time

# Configuration
SERVER = "labynet.fr"  # IRC Server
PORT = 6667  # IRC Port
CHANNEL = "#labynet"  # IRC Channel
BOTNAME = "gpt3"  # Bot's name
OPENAI_API_KEY = os.environ.get("OPENAI_API_KEY")  # Securely get API key from environment

# Initialize OpenAI API
openai.api_key = OPENAI_API_KEY

# Conversation context dictionary for each user with a maximum size of 10 exchanges (20 messages)
MAX_CONTEXT_LENGTH = 10
user_contexts = {}

def get_openai_response(user, prompt):
    """Gets a response from OpenAI API considering the conversation context for a specific user."""
    # Ensure the user has a context deque
    if user not in user_contexts:
        user_contexts[user] = deque(maxlen=MAX_CONTEXT_LENGTH)

    # Get the user's conversation context
    conversation_context = user_contexts[user]

    # Add user message to context
    conversation_context.append({"role": "user", "content": prompt})
    
    try:
        # Get response from OpenAI
        response = openai.ChatCompletion.create(
            model="gpt-3.5-turbo",
            messages=list(conversation_context)
        )
        assistant_message = response['choices'][0]['message']['content']
    except Exception as e:
        print(f"OpenAI Error: {e}")
        assistant_message = "Sorry, I can't respond right now."
    
    # Add assistant's response to context
    conversation_context.append({"role": "assistant", "content": assistant_message})
    
    # Update the global user contexts dictionary
    user_contexts[user] = conversation_context
    
    return assistant_message

def split_response(response, max_length=392):
    """Découpe un texte en morceaux intelligents sans couper les phrases ni perdre du texte."""
    chunks = []
    
    while response:
        if len(response.encode('utf-8')) <= max_length:
            chunks.append(response.strip())
            break

        # Cherche la dernière ponctuation avant la limite pour éviter de couper une phrase
        split_index = max(response.rfind('. ', 0, max_length), 
                          response.rfind('? ', 0, max_length), 
                          response.rfind('! ', 0, max_length), 
                          response.rfind('\n', 0, max_length))

        if split_index == -1:
            # Si aucune ponctuation n'est trouvée, coupe proprement au dernier espace
            split_index = response[:max_length].rfind(' ')
            if split_index == -1:
                split_index = max_length  # Coupe brutalement si aucun espace n'est trouvé

        chunks.append(response[:split_index + 1].strip())  # Ajoute le morceau avec ponctuation
        response = response[split_index + 1:].strip()  # Supprime les espaces avant la suite
    
    return chunks

def send_message(sock, message):
    """Envoie un message en gérant les limites de longueur IRC."""
    chunks = split_response(message)
    
    for chunk in chunks:
        # Envoie chaque partie avec un délai anti-flood
        sock.send(f"PRIVMSG {CHANNEL} :{chunk}\r\n".encode('utf-8'))
        time.sleep(1)  # Délai entre les messages pour éviter le flood

 
def irc_bot():
    """Main IRC bot function."""
    # Precompile regex pattern for efficiency
    channel_escaped = re.escape(CHANNEL)
    botname_escaped = re.escape(BOTNAME)
    message_pattern = re.compile(
        rf'(?i):(.*?)!.* PRIVMSG {channel_escaped}\s+:\s*{botname_escaped}\b\s*[:,]?\s*(.*)',
        re.IGNORECASE
    )
    
    try:
        sock = socket.socket(socket.AF_INET, socket.SOCK_STREAM)
        sock.connect((SERVER, PORT))
    except Exception as e:
        print(f"Connection error: {e}")
        return

    # Authenticate and join channel
    sock.send(f"NICK {BOTNAME}\r\n".encode('utf-8'))
    sock.send(f"USER {BOTNAME} 0 * :{BOTNAME}\r\n".encode('utf-8'))
    sock.send(f"JOIN {CHANNEL}\r\n".encode('utf-8'))

    while True:
        data = sock.recv(4096).decode('utf-8', errors='ignore')
        if not data:
            continue
        
        # Split data into individual lines
        lines = data.split('\r\n')
        for line in lines:
            if not line:
                continue

            # Respond to PING to stay connected
            if line.startswith('PING'):
                sock.send(line.replace('PING', 'PONG').encode('utf-8'))
                continue

            try:
                # Check if the message is addressed to the bot
                match = message_pattern.match(line)
                if match:
                    user = match.group(1)
                    message = match.group(2).strip()
                    response = get_openai_response(user, message)  # Pass the user to get a personalized context
                    send_message(sock, f"{user}: {response}")  # Appel modifié
            except Exception as e:
                print(f"Error processing message: {e}")

if __name__ == "__main__":
    irc_bot()
