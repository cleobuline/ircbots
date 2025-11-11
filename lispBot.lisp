;;;; Bot IRC SBCL — Version FINALE ULTRA ROBUSTE
;;;; Auteurs: cleobuline with the help of Grok (xAI) — Novembre 2025
;;;; Fonctionne sur SBCL + Quicklisp

(unless (find-package :ql) (load "/root/quicklisp/setup.lisp"))
(ql:quickload :cl-ppcre)
(ql:quickload :usocket)
(ql:quickload :bordeaux-threads)

(defpackage :lispbot
  (:use :cl :cl-ppcre :usocket)
  (:import-from :bordeaux-threads
                #:make-thread #:make-lock #:with-lock-held)
  (:export #:start-bot))

(in-package :lispbot)

;;;; === Configuration ===
(defvar *server* "irc.libera.chat")
(defvar *port* 6667)
(defvar *nick* "lisp")
(defvar *channels* '("#lisp-experimental"))
(defvar *socket* nil)
(defvar *stream* nil)
(defvar *write-lock* (make-lock "irc-write-lock"))
(defvar *admin-users* '("labynet" "patricia" "cleobuline"))
(defvar *commands-file* "commands.lisp")

;; Sources persistées (nom -> code), et fonctions actives (nom -> fn)
(defvar *command-sources* (make-hash-table :test #'equal))
(defvar *commands* (make-hash-table :test #'equal))

(defparameter *current-target* nil)

;;;; === IRC I/O ===
(defun irc-send-line (fmt &rest args)
  (with-lock-held (*write-lock*)
    (apply #'format *stream* fmt args)
    (write-char #\Return *stream*)
    (write-char #\Linefeed *stream*)
    (force-output *stream*)))

(defun join-channel (chan) (irc-send-line "JOIN ~a" chan))
(defun privmsg (target text) (irc-send-line "PRIVMSG ~a :~a" target text))

(defun send-msg (text)
  (if *current-target*
      (privmsg *current-target* text)
      (dolist (c *channels*) (privmsg c text))))

(defun send-msg-to (target text) (privmsg target text))

;;;; === Connexion ===
(defun connect-bot ()
  (when *socket* (ignore-errors (close *socket*)))
  (setf *socket* (socket-connect *server* *port* :element-type 'character))
  (setf *stream* (socket-stream *socket*))
  (irc-send-line "NICK ~a" *nick*)
  (irc-send-line "USER ~a 0 * :LispBot ∞" *nick*)
  (format t "Connecté à ~a:~a~%" *server* *port*)
  (dolist (c *channels*)
    (join-channel c)
    (format t "Rejoint ~a~%" c)))

;;;; === Persistance ===
(defun save-commands ()
  (with-open-file (out *commands-file* :direction :output :if-exists :supersede)
    (format out ";;; Commandes du bot — généré automatiquement~%")
    (format out "(in-package :lispbot)~%~%")
    (maphash (lambda (name code)
               (format out "(add-command ~s ~s)~%" name code))
             *command-sources*))
  (send-msg "Commandes sauvegardées !"))

(defun load-commands ()
  ;; Réinitialise tables
  (setf *command-sources* (make-hash-table :test #'equal))
  (setf *commands* (make-hash-table :test #'equal))
  (when (probe-file *commands-file*)
    (let ((*package* (find-package :lispbot)))  ; IMPORTANT: exécuter add-command dans :lispbot
      (load *commands-file* :verbose nil))
    (send-msg (format nil "Chargé ~d commande(s)" (hash-table-count *command-sources*)))))

;;;; === Sécurité & Éval ===
(defun unsafe-form-p (form)
  (labels ((symbol-name-up (sym)
             (and (symbolp sym) (string-upcase (symbol-name sym))))
           (peek-symbols (f)
             (cond
               ((symbolp f) (list (symbol-name-up f)))
               ((consp f) (mapcan #'peek-symbols f))
               (t '()))))
    (let* ((bad-names '("RUN-PROGRAM" "SB-EXT:RUN-PROGRAM" "EXT:RUN-PROGRAM"
                        "LOAD" "WITH-OPEN-FILE" "OPEN" "READ-LINE" "READ-SEQUENCE"
                        "WRITE-SEQUENCE" "DELETE-FILE" "RENAME-FILE" "TRACE" "SYSTEM"
                        "USOCKET:SOCKET-CONNECT" "USOCKET:SOCKET-STREAM"
                        "INTERN" "EVAL" "READ-FROM-STRING" "SB-EXT:SH" "SB-EXT:EXECUTE"
                        "PROCESS" "FORK" "POPEN"))
           (symbols (peek-symbols form)))
      (some (lambda (s)
              (some (lambda (bad) (search bad s :test #'string=)) bad-names))
            symbols))))

(defun log-eval-attempt (sender code allowed)
  (with-open-file (out "eval-audit.log" :direction :output :if-exists :append :if-does-not-exist :create)
    (format out "~&~a ~a allowed=~a ~%~a~%---~%" (get-universal-time) sender allowed code)))

(defun safe-eval (code sender &optional target-chan)
  (handler-case
      (let* ((*package* (find-package :lispbot))
             (*current-target* target-chan)
             (form (read-from-string code))
             (dest (or target-chan (first *channels*)))
             (is-admin (member sender *admin-users* :test #'string=))
             (danger (unsafe-form-p form)))
        (log-eval-attempt sender code (not danger))
        (when (and danger (not is-admin))
          (send-msg-to dest "Forme interdite pour votre niveau d'utilisateur (I/O ou exécution détectée).")
          (return-from safe-eval :forbidden))
        (let ((result (eval form)))
          (cond
            ((stringp result) (send-msg-to dest result))
            ((listp result) (send-msg-to dest (format nil "~a → ~s" sender result)))
            (t (send-msg-to dest (format nil "~a → ~a" sender result))))))
    (error (e)
      (send-msg-to (or target-chan (first *channels*))
                   (format nil "Erreur ~a: ~a" sender e)))))

;;;; === Commandes dynamiques ===
(defun %normalize-name (s)
  (string-downcase (string-trim '(#\Space #\Tab #\Return #\Linefeed) s)))

(defun add-command (name code-str)
  (let* ((clean-name (%normalize-name name))
         ;; compile une fn fermée qui utilise send-msg dynamiquement
         (fn (compile nil `(lambda () (send-msg ,code-str)))))
    (setf (gethash clean-name *commands*) fn)
    (setf (gethash clean-name *command-sources*) code-str)
    (format t "AJOUT COMMANDE: !~a → ~s~%" clean-name code-str)))

(defun call-command (name)
  (let* ((clean (%normalize-name name))
         (fn (gethash clean *commands*)))
    (when fn
      (funcall fn)
      t)))

;;;; === Gestion des commandes (ULTRA ROBUSTE) ===
(defun handle-command (sender dest text)
  ;; Normalise la ligne (supprime CR/LF/espaces parasites)
  (let* ((*current-target* dest)
         (txt (string-trim '(#\Space #\Tab #\Return #\Linefeed) text)))
    (cond
      ((scan "^!hello\\b" txt)
       (send-msg (format nil "Salut ~a ! Lisp est vivant." sender)))

      ((scan "^!eval (.+)" txt)
       (safe-eval (subseq txt (length "!eval ")) sender dest))

 
((and (member sender *admin-users* :test #'string=)
      ;; 1er groupe = nom, 2e = message cité, 3e = message non-cité
      (scan "^!addcmd\\s+!([^\\s]+)\\s+(?:\"([^\"]*)\"|(.*))\\s*$" txt))
 (multiple-value-bind (_ regs)
     (scan-to-strings "^!addcmd\\s+!([^\\s]+)\\s+(?:\"([^\"]*)\"|(.*))\\s*$" txt)
   (let* ((cmd-name (%normalize-name (aref regs 0)))
          (raw2 (aref regs 1))
          (raw3 (aref regs 2))
          (msg (string-trim '(#\Space #\Tab #\Return #\Linefeed)
                            (or raw2 raw3 ""))))
     (if (and (plusp (length cmd-name)) (plusp (length msg)))
         (progn
           (add-command cmd-name msg)
           (send-msg (format nil "Commande !~a ajoutée !" cmd-name))
           (save-commands))
         (send-msg "Erreur : nom ou message vide.")))))


      ((and (member sender *admin-users* :test #'string=) (string= txt "!save"))
       (save-commands))

      ((and (member sender *admin-users* :test #'string=) (string= txt "!load"))
       (load-commands))

;; Remplace l'ancienne branche !fork par celle-ci (parse sans regexp)
((and (member sender *admin-users* :test #'string=)
      (scan "^!fork\\b" txt))
 (let* ((parts (ppcre:split "\\s+" txt))
        (chan  (when (>= (length parts) 2)
                 (string-trim '(#\Space #\Tab #\Return #\Linefeed)
                              (second parts)))))
   (if (and chan (> (length chan) 1) (char= (char chan 0) #\#))
       (progn
         ;; évite doublons, insensible à la casse
         (unless (member chan *channels* :test #'string-equal)
           (push chan *channels*)
           (join-channel chan))
         ;; confirmations explicites
         (send-msg (format nil "Rejoint ~a" chan))
         (send-msg-to chan (format nil "Je suis dans ~a !" chan)))
       (send-msg "Usage: !fork #canal"))))


      ((and (member sender *admin-users* :test #'string=) (string= txt "!cmds"))
       (let ((cmds (loop for k being the hash-keys of *command-sources* collect k)))
         (send-msg
          (if cmds
              (format nil "Commandes (~d): ~{~a~^, ~}" (length cmds) (sort cmds #'string<))
              "Aucune commande personnalisée."))))

      ;; Appel générique: !<nom>
      ((scan "^!([A-Za-z0-9_-]+)\\s*$" txt)
       (let* ((cmd-name-raw (subseq txt 1))
              (cmd-name (%normalize-name cmd-name-raw)))
         ;; 1) on tente la table de fonctions
         (or (call-command cmd-name)
             ;; 2) fallback béton: si on a la source, on envoie directement
             (let ((src (gethash cmd-name *command-sources*)))
               (if src
                   (progn (send-msg src) t)
                   (progn (send-msg (format nil "Commande inconnue : !~a" cmd-name-raw)) nil))))))

      (t nil))))

;;;; === Boucle IRC ===
(defun process-message (line)
  (format t "RECV: ~a~%" line)
  (let ((ln (string-trim '(#\Return #\Linefeed) line)))
    (cond
      ((scan "^PING :(.*)" ln)
       (irc-send-line "PONG :~a" (regex-replace "^PING :(.*)" ln "\\1")))
      ((scan "^:([^!]+)![^ ]+ PRIVMSG ([^ ]+) :(.*)" ln)
       (multiple-value-bind (_ regs)
           (scan-to-strings "^:([^!]+)![^ ]+ PRIVMSG ([^ ]+) :(.*)" ln)
         (let ((sender (aref regs 0)) (target (aref regs 1)) (text (aref regs 2)))
           (make-thread
            (lambda ()
              (handle-command sender (if (char= (char target 0) #\#) target sender) text))
            :name "worker")))))))

;;;; === Démarrage ===
(defun start-bot ()
  (format t "Démarrage du bot...~%")
  (connect-bot)
  (load-commands)
  (format t "Bot prêt !~%")
  (loop
    (handler-case
        (let ((line (read-line *stream* nil nil)))
          (when line (process-message line))
          (unless line
            (format t "Déconnexion. Reconnexion...~%")
            (sleep 5)
            (when *socket* (ignore-errors (close *socket*)))
            (setf *command-sources* (make-hash-table :test #'equal))
            (setf *commands* (make-hash-table :test #'equal))
            (connect-bot)
            (load-commands)))
      (error (e)
        (format t "Erreur: ~a~%" e)
        (sleep 5)
        (when *socket* (ignore-errors (close *socket*)))
        (setf *command-sources* (make-hash-table :test #'equal))
        (setf *commands* (make-hash-table :test #'equal))
        (connect-bot)
        (load-commands)))))

;; === Lancement automatique ===
(start-bot)
