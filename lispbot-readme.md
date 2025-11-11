# LispBot — README (Lisp)

Bot IRC minimaliste, sûr et extensible, écrit en **Common Lisp (SBCL)**.

- Connexion IRC robuste (PING/PONG, reconnexion).
- **Commandes dynamiques** à chaud : `!addcmd !nom "message"`, `!cmds`, `!save`, `!load`.
- `!eval` **sécurisé** (filtre I/O, `run-program`, etc.).
- Persistance des commandes dans `commands.lisp`.

## 1) Lancer depuis Lisp (SBCL + Quicklisp)

### Prérequis
- SBCL (≥ 2.x)
- Quicklisp installé (par défaut sous `/root/quicklisp` ou `$HOME/quicklisp`)

```bash
sbcl --noinform --non-interactive      --load ~/quicklisp/setup.lisp      --eval '(ql:quickload :cl-ppcre)'      --eval '(ql:quickload :usocket)'      --eval '(ql:quickload :bordeaux-threads)'      --quit
```

### Démarrage
Assumant que ton fichier est `lisp-labynet.lisp` (remplace si besoin) :
```bash
sbcl --noinform --non-interactive      --load ~/quicklisp/setup.lisp      --load lisp-labynet.lisp      --quit
```

Le bot :
- se connecte à `*server*`:`*port*`,
- rejoint `*channels*`,
- recharge les commandes depuis `commands.lisp` si présent,
- boucle et se reconnecte automatiquement si la socket tombe.

## 2) Docker (optionnel mais pratique)

Fichier `Dockerfile` :

```dockerfile
FROM debian:bookworm-slim

ENV LANG=C.UTF-8
ENV LC_ALL=C.UTF-8
ENV HOME=/root

RUN apt-get update &&     apt-get install -y --no-install-recommends sbcl curl ca-certificates locales &&     sed -i '/fr_FR.UTF-8/s/^# //g' /etc/locale.gen &&     locale-gen &&     rm -rf /var/lib/apt/lists/*

# Quicklisp + libs
RUN curl -L https://beta.quicklisp.org/quicklisp.lisp -o /tmp/quicklisp.lisp &&     sbcl --noinform --non-interactive          --load /tmp/quicklisp.lisp          --eval '(quicklisp-quickstart:install :path "/root/quicklisp/")'          --eval '(load "/root/quicklisp/setup.lisp")'          --eval '(ql:quickload :cl-ppcre)'          --eval '(ql:quickload :usocket)'          --eval '(ql:quickload :bordeaux-threads)'          --quit &&     rm -f /tmp/quicklisp.lisp

WORKDIR /app
COPY . .

# Lance le bot (SBCL)
CMD ["sbcl","--noinform","--non-interactive","--load","/root/quicklisp/setup.lisp","--load","lisp-labynet.lisp","--quit"]
```

Build & run :
```bash
docker build -t lispbot:latest .
docker run -d --name lispbot   -v $PWD/commands.lisp:/app/commands.lisp   --restart unless-stopped   lispbot:latest
```

> Remplace `lisp-labynet.lisp` dans le `CMD` si ton fichier s’appelle autrement.

## 3) Configuration (dans le code)

Dans `lisp-labynet.lisp` :
- `*server*` (ex. `"labynet.fr"`)
- `*port*` (ex. `6667`)
- `*nick*` (ex. `"LispBot"`)
- `*channels*` (ex. `'("#labynet" "#lisp")`)
- `*admin-users*` (ex. `'("labynet" "patricia" "cleobuline")`)

> Astuce : tu peux exposer ces valeurs via variables d’environnement plus tard. Le bot fonctionne très bien **sans**.

## 4) Persistance

Les commandes dynamiques sont écrites dans `commands.lisp`.  
Monte ce fichier en volume si tu utilises Docker pour conserver l’état entre redéploiements.

## 5) Commandes IRC

- `!hello` — ping.
- `!addcmd !nom "message"` — ajoute une commande personnalisée.
- `!cmds` — liste les commandes personnalisées.
- `!save` / `!load` — persiste/charge depuis `commands.lisp` (admin).
- `!fork #canal` — rejoint un autre canal (admin).
- `!eval <form>` — évalue une forme Lisp **safe**.

Exemples :
```
!addcmd !bonjour "ceci est un test"
!bonjour
!cmds
!eval (+ 1 2 3)
!fork #lisp
```

## 6) Sécurité

- `!eval` filtre les formes dangereuses : `run-program`, I/O fichiers, `load`, etc.
- Les commandes sensibles (`!save`, `!load`, `!fork`) sont limitées aux `*admin-users*`.
- Les envois IRC sont verrouillés (`*write-lock*`) pour éviter les races.

## 7) Débogage / Ops

Voir les logs :
```bash
# source
tail -f lispbot.log   # si tu logs vers un fichier
# docker
docker logs -f lispbot
```

Le bot se reconnecte automatiquement si la connexion tombe.  
En cas de doute :
- vérifier DNS/réseau vers le serveur IRC,
- vérifier le nick déjà pris,
- s’assurer que `commands.lisp` est accessible en lecture/écriture.

## 8) Crédit

- Common Lisp (SBCL) + Quicklisp  
- Bibliothèques : `cl-ppcre`, `usocket`, `bordeaux-threads`  

