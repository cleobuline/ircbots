;;;; Bot IRC SBCL + bordeaux-threads (écritures protégées, CRLF, worker threads)

;; Charger Quicklisp si nécessaire puis les libs requises
(unless (find-package :ql) (load "/root/quicklisp/setup.lisp"))
(ql:quickload :cl-ppcre)
(ql:quickload :usocket)
(ql:quickload :bordeaux-threads)

(defpackage :lispbot
  (:use :cl :cl-ppcre :usocket)
  (:import-from :bordeaux-threads
                #:make-thread #:make-lock #:with-lock-held))

(in-package :lispbot)

;;;; === Config ===
(defvar *server* "irc.libera.chat")
(defvar *port* 6667)
(defvar *nick* "LispBot")
(defvar *channels* (list "#lisp-experimental"))       ;; multi-canaux
(defvar *socket* nil)
(defvar *stream* nil)
(defvar *write-lock* (make-lock "irc-write-lock"))

(defvar *eval-users* nil)                    ;; tout le monde peut !eval (ajuste si besoin)
(defvar *admin-users* '("labynet" "patricia" "cleobuline"))
(defvar *commands-file* "commands.lisp")
(defvar *command-sources* (make-hash-table :test #'equal))

;; Contexte courant de réponse (canal / nick)
(defparameter *current-target* nil)

;;;; === IRC I/O (CRLF + lock) ===
(defun irc-send-line (fmt &rest args)
  (with-lock-held (*write-lock*)
    ;; écrire la ligne formatée
    (apply #'format *stream* fmt args)
    ;; ajouter CRLF correctement
    (write-char #\Return *stream*)
    (write-char #\Linefeed *stream*)
    (force-output *stream*)))

(defun join-channel (chan) (irc-send-line "JOIN ~a" chan))
(defun privmsg (target text) (irc-send-line "PRIVMSG ~a :~a" target text))

(defun send-msg (text)
  "Envoie vers le contexte courant si disponible, sinon broadcast sur *channels*."
  (if *current-target*
      (privmsg *current-target* text)
      (dolist (c *channels*) (privmsg c text))))

(defun send-msg-to (chan text) (privmsg chan text))

;;;; === Connexion ===
(defun connect-bot ()
  (setf *socket* (socket-connect *server* *port* :element-type 'character))
  (setf *stream* (socket-stream *socket*))
  (irc-send-line "NICK ~a" *nick*)
  (irc-send-line "USER ~a 0 * :LispBot ∞" *nick*)
  (format t "Connecté à ~a:~a~%" *server* *port*)
  ;; join tous les canaux configurés
  (dolist (c *channels*)
    (join-channel c)
    (format t "Rejoint ~a~%" c)))

;;;; === Persistance commandes ===
(defun save-commands ()
  (with-open-file (out *commands-file* :direction :output :if-exists :supersede)
    (format out ";; Commandes du bot - généré automatiquement~%")
    (maphash (lambda (name code)
               (format out "(add-command ~s ~s)~%" name code))
             *command-sources*))
  (send-msg "Commandes sauvegardées !"))

(defun add-command (name code-str)
  (let ((func (eval `(lambda () (send-msg ,code-str)))))
    (setf (symbol-function (intern (string-upcase name))) func)
    (setf (gethash name *command-sources*) code-str)))

(defun load-commands ()
  (when (probe-file *commands-file*)
    (load *commands-file*)
    (send-msg (format nil "Commandes chargées depuis ~a" *commands-file*))))

;;;; === Sécurité RUN-PROGRAM (portable SBCL/CLISP) ===
(defun unsafe-form-p (form)
  "Détecte les formes dangereuses (run-program, I/O, load, open, with-open-file, read-line, intern, eval).
   Retourne T si la forme doit être refusée pour les non-admins."
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

;;;; === Audit des !eval ===
(defun log-eval-attempt (sender code allowed)
  (with-open-file (out "eval-audit.log" :direction :output :if-exists :append :if-does-not-exist :create)
    (format out "~&~a ~a allowed=~a ~%~a~%---~%" (get-universal-time) sender allowed code)))

;;;; === Évaluation dans le bon package + contexte ===
(defun safe-eval (code sender &optional target-chan)
  (handler-case
      (let* ((*package* (find-package :lispbot))
             (*current-target* target-chan)
             (form (read-from-string code))
             (dest (or target-chan (first *channels*)))
             (is-admin (member sender *admin-users* :test #'string=))
             (danger (unsafe-form-p form)))
        ;; journalise la tentative
        (log-eval-attempt sender code (not danger))
        ;; refuse si dangereux et non-admin
        (when (and danger (not is-admin))
          (send-msg-to dest "Forme interdite pour votre niveau d'utilisateur (I/O ou exécution détectée).")
          (return-from safe-eval :forbidden))
        ;; exécution (ajouter timeout ici si nécessaire)
        (let ((result (eval form)))
          (cond
            ((stringp result) (send-msg-to dest result))
            ((listp result)   (send-msg-to dest (format nil "~a → ~s" sender result)))
            (t                (send-msg-to dest (format nil "~a → ~s" sender result))))))
    (error (e)
      (send-msg-to (or target-chan (first *channels*)) (format nil "Erreur ~a: ~a" sender e)))))

;;;; === Dispatch message → worker thread ===
(defun handle-command (sender dest text)
  (let ((*current-target* dest))
    (cond
      ;; !hello
      ((scan "^!hello" text)
       (send-msg (format nil "Salut ~a ! Lisp est vivant." sender)))

      ;; !eval CODE
      ((scan "^!eval (.+)" text)
       (let ((code (subseq text (length "!eval "))))
         (safe-eval code sender dest)))

      ;; !addcmd !nom "message" (admins)
      ((and (member sender *admin-users* :test #'string=)
            (scan "^!addcmd !([^ ]+) \"(.*)\"$" text))
       (let* ((cmd-name (subseq text (length "!addcmd !")
                                (position #\Space text :start (length "!addcmd !"))))
              (msg (subseq text (1+ (position #\" text))
                           (1- (position #\" text :from-end t)))))
         (setf (gethash cmd-name *command-sources*) msg)
         (add-command cmd-name msg)
         (send-msg (format nil "Commande !~a ajoutée !" cmd-name))))

      ;; !save / !load (admins)
      ((and (member sender *admin-users* :test #'string=)
            (scan "^!save$" text))
       (save-commands))

      ((and (member sender *admin-users* :test #'string=)
            (scan "^!load$" text))
       (load-commands))

      ;; !fork #canal → JOIN + ajout liste (lecture reste mono-thread; réponses contextuelles)
      ((and (member sender *admin-users* :test #'string=)
            (scan "^!fork (#.+)$" text))
       (let ((new-chan (aref (nth-value 1 (scan-to-strings "^!fork (#.+)$" text)) 0)))
         (unless (member new-chan *channels* :test #'string=)
           (push new-chan *channels*)
           (join-channel new-chan)
           (send-msg (format nil "Je me duplique vers ~a (worker threads)." new-chan))
           (send-msg-to new-chan (format nil "Je suis vivant dans ~a !" new-chan))))))))

(defun process-message (line)
  (format t "RECV: ~a~%" line)
  (cond
    ;; PING
    ((scan "^PING :(.*)" line)
     (multiple-value-bind (_ regs)
         (scan-to-strings "^PING :(.*)" line)
       (declare (ignore _))
       (irc-send-line "PONG :~a" (aref regs 0))))
    ;; PRIVMSG
    ((scan "^:([^!]+)![^ ]+ PRIVMSG ([^ ]+) :(.*)" line)
     (multiple-value-bind (match regs)
         (scan-to-strings "^:([^!]+)![^ ]+ PRIVMSG ([^ ]+) :(.*)" line)
       (when match
         (let* ((sender (aref regs 0))
                (target (aref regs 1))
                (text   (aref regs 2))
                (dest   (if (char= (char target 0) #\#) target sender)))
           ;; traiter chaque commande dans un worker thread
           (make-thread (lambda () (handle-command sender dest text))
                        :name (format nil "cmd-~a" dest))))))))

;;;; === Démarrage / boucle principale ===
(format t "Démarrage du bot...~%")
(connect-bot)
(load-commands)
(format t "Bot prêt. Ctrl+C pour arrêter.~%")

(loop
  (handler-case
      (let ((line (read-line *stream* nil nil)))
        (unless line
          (format t "Connexion perdue. Reconnexion...~%")
          (connect-bot)
          (load-commands))
        (when line
          (process-message line)))
    (error (e)
      (format t "Erreur: ~a. Reconnexion...~%" e)
      (sleep 5)
      (connect-bot)
      (load-commands))))
