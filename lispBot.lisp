;;;; Bot IRC SBCL — Ultra Robuste + DEFUN persistantes + !funcs/!delfunc + !connect
;;;; Version silencieuse (aucune sortie console) + !addcmd avec !eval
;;;; Fonctionne sur SBCL + Quicklisp

(unless (find-package :ql)
  (load "/root/quicklisp/setup.lisp"))
(ql:quickload :cl-ppcre)
(ql:quickload :usocket)
(ql:quickload :bordeaux-threads)

(defpackage :lispbot
  (:use :cl :cl-ppcre :usocket)
  (:import-from :bordeaux-threads
                #:make-thread #:make-lock #:with-lock-held)
  (:export #:start-bot))

(in-package :lispbot)

;;; === Prototypes (déclarations de type de fonction) ===
(declaim (ftype (function (t) t) process-message))
(declaim (ftype (function (t t t) t) handle-command))
(declaim (ftype (function (t &rest t) t) irc-send-line))

;;;; === Configuration ===
(defvar *server* "labynet.fr")
(defvar *port* 6667)
(defvar *nick* "LispBot")
(defvar *channels* '("#test"))
(defvar *socket* nil)
(defvar *stream* nil)
(defvar *write-lock* (make-lock "irc-write-lock"))
(defvar *persist-lock* (make-lock "persist-lock"))
(defvar *admin-users* '("labynet" "patricia" "cleobuline"))
(defvar *commands-file* "commands.lisp")
(defvar *user-funcs-file* "user-funcs.lisp")

;; Sources persistées (nom -> code), et fonctions actives (nom -> fn)
(defvar *command-sources* (make-hash-table :test #'equal))
(defvar *commands*        (make-hash-table :test #'equal))

(defparameter *current-target* nil)

;;;; === IRC I/O ===
(defun irc-send-line (fmt &rest args)
  (with-lock-held (*write-lock*)
    (apply #'format *stream* fmt args)
    (write-char #\Return *stream*)
    (write-char #\Linefeed *stream*)
    (force-output *stream*)))

(defun join-channel (chan)
  (irc-send-line "JOIN ~a" chan))

(defun privmsg (target text)
  (irc-send-line "PRIVMSG ~a :~a" target text))

(defun send-msg (text)
  (if *current-target*
      (privmsg *current-target* text)
      (dolist (c *channels*)
        (privmsg c text))))

(defun send-msg-to (target text)
  (privmsg target text))

;;;; === Boucle IRC (process-message) ===
(defun process-message (line)
  (let ((ln (string-trim '(#\Return #\Linefeed) line)))
    (cond
      ;; PING — compatible tags/prefix, ':' optionnel
      ((scan "^(?:@[^ ]+ )?(?::[^ ]+ )?PING ?:?(.*)$" ln)
       (let ((token (regex-replace
                     "^(?:@[^ ]+ )?(?::[^ ]+ )?PING ?:?(.*)$"
                     ln "\\1")))
         (irc-send-line "PONG ~a"
                        (if (plusp (length token))
                            (concatenate 'string ":" token)
                            ""))))

      ;; NUMÉRIQUES (404, 482, etc.) — ignorés silencieusement
      ((scan "^(?:@[^ ]+ )?:[^ ]+ ([0-9]{3}) (.*)" ln)
       (multiple-value-bind (_ regs)
           (scan-to-strings "^(?:@[^ ]+ )?:[^ ]+ ([0-9]{3}) (.*)" ln)
         (declare (ignore _ regs))
         nil))

      ;; PRIVMSG — accepte tags IRCv3
      ((scan "^(?:@[^ ]+ )?:([^!]+)![^ ]+ PRIVMSG ([^ ]+) :(.*)" ln)
       (multiple-value-bind (_ regs)
           (scan-to-strings
            "^(?:@[^ ]+ )?:([^!]+)![^ ]+ PRIVMSG ([^ ]+) :(.*)" ln)
         (declare (ignore _))
         (let ((sender (aref regs 0))
               (target (aref regs 1))
               (text   (aref regs 2)))
           (handle-command sender
                           (if (and (> (length target) 0)
                                    (char= (char target 0) #\#))
                               target
                               sender)
                           text))))

      (t
       nil))))

(defun wait-welcome ()
  "Attend le RPL_WELCOME (001) avant de continuer."
  (loop
    for line = (read-line *stream* nil nil)
    do
      (when (null line)
        (error "Déconnecté pendant l'enregistrement (avant 001)"))
      (process-message line)
      (when (cl-ppcre:scan "^(?:@[^ ]+ )?:\\S+ 001 " line)
        (return))))

;;;; === Connexion ===
(defun connect-bot ()
  (when *socket*
    (ignore-errors (close *socket*)))
  (setf *socket* (socket-connect *server* *port* :element-type 'character))
  (setf *stream* (socket-stream *socket*))
  (irc-send-line "NICK ~a" *nick*)
  (irc-send-line "USER ~a 0 * :LispBot" *nick*)
  ;; attendre le 001
  (wait-welcome)
  ;; Petit délai anti-flood
  (sleep 0.8)
  (dolist (c *channels*)
    (join-channel c)))

;;;; === Persistance des commandes ===
(defun save-commands ()
  (with-open-file (out *commands-file*
                       :direction :output
                       :if-exists :supersede)
    (format out ";;; Commandes du bot — généré automatiquement~%")
    (format out "(in-package :lispbot)~%~%")
    (maphash (lambda (name code)
               (format out "(add-command ~s ~s)~%" name code))
             *command-sources*))
  (send-msg "Commandes sauvegardées !"))

(defun load-commands ()
  (setf *command-sources* (make-hash-table :test #'equal))
  (setf *commands*        (make-hash-table :test #'equal))
  (when (probe-file *commands-file*)
    (let ((*package* (find-package :lispbot)))
      (load *commands-file* :verbose nil))
    (send-msg (format nil "Chargé ~d commande(s)"
                      (hash-table-count *command-sources*)))))

;;;; === Persistance des DEFUN via !eval (anti-doublon) ===
(defun persist-defuns-in-form (form)
  "Détecte toutes les (defun ...) dans FORM et les ajoute au fichier persistant
   si elles ne sont pas déjà présentes."
  (labels ((collect-defuns (f acc)
             (cond
               ((and (consp f) (eq (first f) 'defun))
                (cons f acc))
               ((consp f)
                (collect-defuns (rest f)
                                (collect-defuns (first f) acc)))
               (t acc))))
    (let ((defs (nreverse (collect-defuns form '()))))
      (when defs
        (with-lock-held (*persist-lock*)
          (let ((seen (make-hash-table :test #'equal)))
            ;; marquer celles déjà présentes
            (when (probe-file *user-funcs-file*)
              (with-open-file (in *user-funcs-file* :direction :input)
                (loop for ff = (read in nil :eof)
                      until (eq ff :eof)
                      when (and (consp ff) (eq (first ff) 'defun))
                      do (setf (gethash (string-downcase
                                         (symbol-name (second ff)))
                                        seen)
                               t))))
            ;; ajouter les nouvelles
            (with-open-file (out *user-funcs-file*
                                 :direction :output
                                 :if-exists :append
                                 :if-does-not-exist :create)
              (dolist (d defs)
                (let ((nm (string-downcase (symbol-name (second d)))))
                  (unless (gethash nm seen)
                    (format out "~s~%" d)
                    (setf (gethash nm seen) t)))))))))))

(defun list-user-funcs ()
  "Retourne la liste des fonctions persistées (symboles dans le package :lispbot)."
  (when (probe-file *user-funcs-file*)
    (let ((*package* (find-package :lispbot)))
      (with-open-file (in *user-funcs-file* :direction :input)
        (loop for f = (read in nil :eof)
              until (eq f :eof)
              when (and (consp f) (eq (first f) 'defun))
              collect (second f))))))

(defun delete-user-func (name)
  "Supprime une fonction du fichier et de la mémoire."
  (let* ((target-up (string-upcase (string-trim '(#\Space #\Tab #\Return #\Linefeed)
                                                name)))
         (sym       (intern target-up :lispbot))
         (fns       (list-user-funcs)))
    (if (not (some (lambda (s) (string-equal (symbol-name s) target-up))
                   fns))
        (send-msg (format nil "Aucune fonction nommée ~a." name))
        (progn
          (with-lock-held (*persist-lock*)
            (let ((*package* (find-package :lispbot)))
              (let ((tmp (make-string-output-stream)))
                (with-open-file (in *user-funcs-file* :direction :input)
                  (loop for f = (read in nil :eof)
                        until (eq f :eof)
                        unless (and (consp f)
                                    (eq (first f) 'defun)
                                    (string-equal (symbol-name (second f)) target-up))
                        do (format tmp "~s~%" f)))
                (with-open-file (out *user-funcs-file*
                                     :direction :output
                                     :if-exists :supersede)
                  (princ (get-output-stream-string tmp) out)))))
          (ignore-errors (fmakunbound sym))
          (send-msg (format nil "Fonction ~a supprimée." name))))))

(defun load-user-funcs ()
  (when (probe-file *user-funcs-file*)
    (let ((*package* (find-package :lispbot)))
      (load *user-funcs-file* :verbose nil))))

;;;; === Sécurité & Éval ===
(defun unsafe-form-p (form)
  (labels ((symbol-name-up (sym)
             (and (symbolp sym)
                  (string-upcase (symbol-name sym))))
           (peek-symbols (f)
             (cond
               ((symbolp f) (list (symbol-name-up f)))
               ((consp f) (mapcan #'peek-symbols f))
               (t '()))))
    (let* ((bad-names '("RUN-PROGRAM" "LOAD" "WITH-OPEN-FILE" "OPEN"
                        "READ-LINE" "READ-SEQUENCE"
                        "WRITE-SEQUENCE" "DELETE-FILE" "RENAME-FILE"
                        "TRACE" "SYSTEM"
                        "SOCKET-CONNECT" "SOCKET-STREAM"
                        "INTERN" "EVAL" "READ-FROM-STRING"
                        "SH" "EXECUTE" "PROCESS" "FORK" "POPEN"))
           (symbols (peek-symbols form)))
      (some (lambda (s)
              (some (lambda (bad)
                      (search bad s :test #'string=))
                    bad-names))
            symbols))))

(defun log-eval-attempt (sender code allowed)
  (with-open-file (out "eval-audit.log"
                       :direction :output
                       :if-exists :append
                       :if-does-not-exist :create)
    (format out "~&~a ~a allowed=~a ~%~a~%---~%"
            (get-universal-time) sender allowed code)))

(defun safe-eval (code sender &optional target-chan)
  (handler-case
      (let* ((*package* (find-package :lispbot))
             (*current-target* target-chan)
             (form     (read-from-string code))
             (dest     (or target-chan (first *channels*)))
             (is-admin (member sender *admin-users* :test #'string=))
             (danger   (unsafe-form-p form)))
        (log-eval-attempt sender code (not danger))
        (when (and danger (not is-admin))
          (send-msg-to dest
                       "Forme interdite pour votre niveau d'utilisateur (I/O ou exécution détectée).")
          (return-from safe-eval :forbidden))
        (persist-defuns-in-form form)
        (let ((result (eval form)))
          (cond
            ((stringp result)
             (send-msg-to dest result))
            ((listp result)
             (send-msg-to dest (format nil "~a → ~s" sender result)))
            (t
             (send-msg-to dest (format nil "~a → ~a" sender result))))))
    (error (e)
      (send-msg-to (or target-chan (first *channels*))
                   (format nil "Erreur ~a: ~a" sender e)))))

;;;; === Commandes dynamiques (hash-table) ===
(defun %normalize-name (s)
  (string-downcase (string-trim '(#\Space #\Tab #\Return #\Linefeed) s)))

;; Nouvelle version : supporte les commandes !eval
(defun add-command (name code-str &optional creator)
  (let* ((clean-name (%normalize-name name))
         (msg (string-trim '(#\Space #\Tab #\Return #\Linefeed) code-str))
         (creator-name (or creator "system"))
         (is-eval (and (> (length msg) 6)
                       (string= (subseq msg 0 6) "!eval ")))
         (eval-body (when is-eval
                      (string-trim '(#\Space #\Tab #\Return #\Linefeed)
                                   (subseq msg 6))))
         (fn (if is-eval
                 ;; Commande dynamique: exécute du code via safe-eval
                 (compile nil
                          `(lambda ()
                             (safe-eval ,eval-body ,creator-name *current-target*)))
                 ;; Commande simple: envoie juste le texte
                 (compile nil
                          `(lambda ()
                             (send-msg ,code-str))))))
    (setf (gethash clean-name *commands*) fn)
    (setf (gethash clean-name *command-sources*) code-str)))

(defun call-command (name)
  (let* ((clean (%normalize-name name))
         (fn    (gethash clean *commands*)))
    (when fn
      (funcall fn)
      t)))

;;;; === Instance éphémère multi-serveur (pour !connect) ===
(defun %random-nick (base)
  (format nil "~a~3,'0d" base (random 1000)))

(defun start-ephemeral-bot (srv chan &key (port 6667) (nick-base *nick*))
  "Démarre une instance indépendante connectée à SRV et rejoignant CHAN."
  (let ((*server* srv)
        (*port*   port)
        (*nick*   (%random-nick nick-base))
        (*channels* (list chan))
        (*socket* nil)
        (*stream* nil)
        (*current-target* nil)
        (*command-sources* (make-hash-table :test #'equal))
        (*commands*        (make-hash-table :test #'equal)))
    (labels ((run ()
               (connect-bot)
               (load-user-funcs)
               (load-commands)
               (send-msg-to chan
                            (format nil "Connecté à ~a dans ~a (nick ~a) !"
                                    srv chan *nick*))
               (loop
                 (handler-case
                     (let ((line (read-line *stream* nil nil)))
                       (when line
                         (process-message line))
                       (unless line
                         (sleep 5)
                         (when *socket*
                           (ignore-errors (close *socket*)))
                         (setf *command-sources*
                               (make-hash-table :test #'equal))
                         (setf *commands*
                               (make-hash-table :test #'equal))
                         (connect-bot)
                         (load-user-funcs)
                         (load-commands)))
                   (error (e)
                     (declare (ignore e))
                     (sleep 5)
                     (when *socket*
                       (ignore-errors (close *socket*)))
                     (setf *command-sources*
                           (make-hash-table :test #'equal))
                     (setf *commands*
                           (make-hash-table :test #'equal))
                     (connect-bot)
                     (load-user-funcs)
                     (load-commands))))))
      (run))))

;;;; === Gestion des commandes (ULTRA ROBUSTE) ===
(defun handle-command (sender dest text)
  (let* ((*current-target* dest)
         (txt (string-trim '(#\Space #\Tab #\Return #\Linefeed) text)))
    (cond
      ((scan "^!hello\\b" txt)
       (send-msg (format nil "Salut ~a ! Lisp est vivant." sender)))

      ((string= txt "!help")
       (send-msg
        "Commandes: !hello !eval !uptime !addcmd !save !load !fork !cmds !funcs !delfunc !connect !help"))

      ((string= txt "!uptime")
       ;; si tu as défini (defun pu () ...) via !eval, on peut s'en servir
       (handler-case
           (send-msg (pu))
         (error ()
           (send-msg "La fonction pu n'est pas définie (utilise !eval pour la créer)."))))

      ((scan "^!eval (.+)" txt)
       (safe-eval (subseq txt (length "!eval ")) sender dest))

      ;; !addcmd !nom message
      ((and (member sender *admin-users* :test #'string=)
            (scan "^!addcmd\\s+!([^\\s]+)\\s+(?:\"([^\"]*)\"|(.*))\\s*$" txt))
       (multiple-value-bind (_ regs)
           (scan-to-strings "^!addcmd\\s+!([^\\s]+)\\s+(?:\"([^\"]*)\"|(.*))\\s*$" txt)
         (declare (ignore _))
         (let* ((cmd-name (%normalize-name (aref regs 0)))
                (raw2     (aref regs 1))
                (raw3     (aref regs 2))
                (msg      (string-trim '(#\Space #\Tab #\Return #\Linefeed)
                                       (or raw2 raw3 ""))))
           (if (and (plusp (length cmd-name))
                    (plusp (length msg)))
               (progn
                 (add-command cmd-name msg sender)
                 (send-msg (format nil "Commande !~a ajoutée !" cmd-name))
                 (save-commands))
               (send-msg "Erreur : nom ou message vide.")))))

      ((and (member sender *admin-users* :test #'string=)
            (string= txt "!save"))
       (save-commands))

      ((and (member sender *admin-users* :test #'string=)
            (string= txt "!load"))
       (load-commands))

      ;; !fork #canal : rejoindre un canal supplémentaire
      ((and (member sender *admin-users* :test #'string=)
            (scan "^!fork\\b" txt))
       (let* ((parts (ppcre:split "\\s+" txt))
              (chan  (when (>= (length parts) 2)
                       (string-trim '(#\Space #\Tab #\Return #\Linefeed)
                                    (second parts)))))
         (if (and chan (> (length chan) 1)
                  (char= (char chan 0) #\#))
             (progn
               (unless (member chan *channels* :test #'string-equal)
                 (push chan *channels*)
                 (join-channel chan))
               (send-msg (format nil "Rejoint ~a" chan))
               (send-msg-to chan (format nil "Je suis dans ~a !" chan)))
             (send-msg "Usage: !fork #canal"))))

      ((string= txt "!cmds")
       (let ((cmds (loop for k being the hash-keys of *command-sources* collect k)))
         (send-msg
          (if cmds
              (format nil "Commandes (~d): ~{~a~^, ~}"
                      (length cmds)
                      (sort cmds #'string<))
              "Aucune commande personnalisée."))))

      ((string= txt "!funcs")
       (let ((fns (list-user-funcs)))
         (if fns
             (send-msg
              (format nil "Fonctions sauvegardées (~d): ~{~a~^, ~}"
                      (length fns) fns))
             (send-msg "Aucune fonction sauvegardée."))))

      ((scan "^!delfunc\\s+(.+)" txt)
       (multiple-value-bind (_ regs)
           (scan-to-strings "^!delfunc\\s+(.+)" txt)
         (declare (ignore _))
         (delete-user-func (aref regs 0))))

      ;; !connect srv #chan : lance une instance éphémère dans un thread
      ((and (member sender *admin-users* :test #'string=)
            (scan "^!connect\\s+([^\\s]+)\\s+(#\\S+)\\s*$" txt))
       (multiple-value-bind (_ regs)
           (scan-to-strings "^!connect\\s+([^\\s]+)\\s+(#\\S+)\\s*$" txt)
         (declare (ignore _))
         (let ((srv  (aref regs 0))
               (chan (aref regs 1)))
           (make-thread
            (lambda ()
              (start-ephemeral-bot srv chan :port 6667 :nick-base *nick*))
            :name (format nil "conn-~a" srv))
           (send-msg (format nil "Connexion lancée vers ~a (~a)..." srv chan)))))

      ;; appel de commande custom !xxx
      ((scan "^!([A-Za-z0-9_-]+)\\s*$" txt)
       (let* ((cmd-name-raw (subseq txt 1))
              (cmd-name     (%normalize-name cmd-name-raw)))
         (or (call-command cmd-name)
             (let ((src (gethash cmd-name *command-sources*)))
               (if src
                   (progn
                     (send-msg src)
                     t)
                   (progn
                     (send-msg
                      (format nil "Commande inconnue : !~a" cmd-name-raw))
                     nil))))))

      (t
       nil))))

;;;; === Démarrage ===
(defun start-bot ()
  (connect-bot)
  (load-user-funcs)
  (load-commands)
  (loop
    (handler-case
        (let ((line (read-line *stream* nil nil)))
          (when line
            (process-message line))
          (unless line
            (sleep 5)
            (when *socket*
              (ignore-errors (close *socket*)))
            (setf *command-sources* (make-hash-table :test #'equal))
            (setf *commands*        (make-hash-table :test #'equal))
            (connect-bot)
            (load-user-funcs)
            (load-commands)))
      (error (e)
        (declare (ignore e))
        (sleep 5)
        (when *socket*
          (ignore-errors (close *socket*)))
        (setf *command-sources* (make-hash-table :test #'equal))
        (setf *commands*        (make-hash-table :test #'equal))
        (connect-bot)
        (load-user-funcs)
        (load-commands)))))

;; === Lancement automatique ===
(start-bot)
