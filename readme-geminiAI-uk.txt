===============================================================================
                          geminiAI - IRC BOT v3.1
===============================================================================

DESCRIPTION
-----------
geminiAI is a high-performance, multimodal IRC bot built for the Labynet 
network. It integrates the latest Google Gemini 3.1 Pro engines to provide 
scientific reasoning, image generation, video production, and music creation 
directly within an IRC channel.

CORE TECHNOLOGIES
-----------------
- Engine: Google Gemini 3.1 Pro Preview (March 2026)
- Vision: URL and Code analysis (GitHub/Pastebin support)
- Video: Google Veo 3.1 (9:16 vertical format with audio)
- Image: Google Imagen 4.0
- Music: Google Lyria 3 (High-fidelity audio generation)
- Framework: Python 3.11+ / asyncio / irc3

FEATURES & COMMANDS
-------------------

[!gemini <prompt>]
    Standard chat command. Uses a sliding context window of the last 10 
    messages. geminiAI follows a strict scientific protocol: 
    - No Markdown (no bold, no headers) for pure IRC readability.
    - Strict algebraic development for mathematical proofs.
    - Explicit parentheses for all fractions: (a+b)/(c+d).

[!image <prompt>]
    Generates a high-resolution image using Imagen 4.0.
    Output: Public URL at https://labynet.fr/images/

[!video <prompt>]
    Generates a 4 to 10-second vertical video (9:16) via Veo 3.1.
    The process includes an asynchronous polling system.
    Output: MP4 file at https://labynet.fr/videos/

[!music <prompt>]
    Creates a 30-second high-fidelity music track using Lyria 3.
    Output: MP3/WAV file at https://labynet.fr/audio/

[!url <link> [question]]
    Fetches and analyzes web content. Supports raw GitHub and Pastebin 
    fetching for code audits, vulnerability detection, or summarization.

TECHNICAL SPECIFICATIONS
------------------------

1. CLEANING PROTOCOL
The bot uses a custom filtering pipeline (pylatexenc) to convert LaTeX 
formulas into plain text and strips all Markdown artifacts to ensure 
compatibility with legacy IRC clients.

2. CONCURRENCY & LOCKING
To prevent race conditions, geminiAI implements an 'asyncio.Lock' per 
(channel, user) pair. This ensures that context history is updated 
sequentially and never corrupted by rapid-fire messaging.

3. PERFORMANCE & COMPLEXITY
- Time Complexity: O(n) where n is the number of tokens in the history.
- Resource Management: Heavy tasks (video/image/music) are guarded by 
  a global Semaphore(1) to manage API quotas and server load.

4. MULTILINGUAL SUPPORT
The bot features automatic language detection based on the user's last 
input, ensuring a seamless experience for international users.

INSTALLATION
------------
1. Clone the repository.
2. Install dependencies: pip install irc3 google-genai pylatexenc httpx
3. Configure 'config.json' with your Gemini API Key and local directories.
4. Run: python gemini.py config.json

LICENSE
-------
Distributed under the MIT License. Created for the Labynet Community.

===============================================================================
