===============================================================================
          ZOZO GEMINI - THE ROLLS-ROYCE OF MULTIMODAL IRC BOTS (2026)
===============================================================================

A high-end, robust, and elegant IRC bot integration for Google Gemini 2.5, 
featuring native support for state-of-the-art Video, Audio, and Vision models.

-------------------------------------------------------------------------------
1. KEY FEATURES
-------------------------------------------------------------------------------
* ULTRA-MODERN ENGINE: Powered by Gemini 2.5 (Text/Vision), 
    Veo 3.1 (Video Generation), and Lyria 3 (High-fidelity Music).
* CLEAN IRC OUTPUT: Custom LaTeX-to-Unicode engine and Markdown 
    sanitizer. No messy code blocks, just readable text.
* SMART MEMORY MANAGEMENT: 
    - 20-line sliding window context per user.
    - Global context pruning (500 active keys limit) to save RAM.
* PERSISTENT SESSIONS: User-controlled !save and !load commands 
    to manage long-term memory via JSON files.
* STABILITY: Asyncio-based with semaphores for heavy tasks and 
    per-user thread locks to prevent flooding.

-------------------------------------------------------------------------------
2. DEPENDENCIES
-------------------------------------------------------------------------------
Python 3.10+ is required. Install the following libraries:

pip install google-generativeai latex2unicode aiohttp asyncio requests

-------------------------------------------------------------------------------
3. COMMAND GUIDE
-------------------------------------------------------------------------------
!gemini <prompt>       - Standard chat with the bot.
!vision <url> <prompt> - Analyze an image or video (Max 10MB).
!url <url> <prompt>    - Analyze web content or a pastebin.
!image <prompt>        - Generate a high-quality image (Imagen 4).
!video <prompt>        - Generate a 4s cinematic video (Veo 3.1).
!music <prompt>        - Generate a 30s studio-quality track (Lyria 3).
!save <filename>       - Save current context to a permanent file.
!load <filename>       - Load a previously saved context.
!delete <filename>     - Delete a saved context file.
!list                  - List your saved conversation files.
!reset                 - Clear your current active context.

-------------------------------------------------------------------------------
4. CONFIGURATION
-------------------------------------------------------------------------------
* API KEY: Set your GOOGLE_API_KEY in the script or environment.
* STORAGE: Ensure the following directories exist and are writeable:
    - ./conversations/ (for JSON context files)
    - /var/www/html/images/ (or your public web path)
    - /var/www/html/videos/
    - /var/www/html/audio/

-------------------------------------------------------------------------------
5. TECHNICAL NOTES
-------------------------------------------------------------------------------
* IRC LIMITS: Messages are automatically chunked at ~380 characters 
    to comply with IRC protocol standards and avoid server truncation.
* FILE SAFETY: All filenames and inputs are sanitized to prevent 
    directory traversal or shell injection.
===============================================================================
