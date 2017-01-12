;;; niclein.el --- Nic's lein and clojure integration  -*- lexical-binding: t; -*-

;; Copyright (C) 2015  Nic Ferrier

;; Author: Nic Ferrier <nferrier@ferrier.me.uk>
;; Keywords: languages, lisp
;; Version: 0.1.4
;; Package-requires: ((shadchen "1.4")(smartparens "1.5")(s "1.9.0"))
;; Url: https://github.com/nicferrier/niclein

;; This program is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.

;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with this program.  If not, see <http://www.gnu.org/licenses/>.

;;; Commentary:

;; Simpler than cider Lein integration.

;; As a beginning Clojure user Nic found he wanted to run the whole
;; program. Not rely on some workflow to reload classes. Also it
;; seemed the right thing to do to him.

;; So this let's you run `lein' or run a `lein repl' in a process and
;; interact with it.

;; Nic has never liked ielm or any of the comint modes so this also
;; has a basic command line interaction implementation. The lein
;; process output is piped into a buffer and a fixed point at the end
;; of the buffer is used as the prompt. A simple keymap hanging off a
;; prefix key ("#" currently, "##" will insert a "#") is used to allow
;; scrolling up and down through the history.

;; The command line has smartparens support but no completion.

;;; Code:

(require 'shadchen)
(require 'smartparens)
(require 'url) ; for retrieving leiningen if we need it
(require 'clojure-mode)
(require 's)
(require 'kv)
(require 'base64)

(defconst lein-version "2.5.1"
  "The version of lein we will retrieve.")

(defconst lein-url
  (concat "https://github.com/technomancy/leiningen/releases/download/"
          lein-version
          "/leiningen-" lein-version "-standalone.zip"))

(defgroup niclein nil
  "Simple leiningen management for Clojure."
  :group 'clojure-mode)

(defcustom niclein-java "java"
  "What Java should we use?"
  :group 'niclein
  :type 'file)

(defvar niclein/prompt-marker nil
  "Where the prompt is in a repl buffer.")
(make-variable-buffer-local 'niclein/prompt-marker)

(defvar niclein/prompt-entry-marker nil
  "Where text entry for the prompt should be in a repl buffer.")
(make-variable-buffer-local 'niclein/prompt-entry-marker)

(defgroup clojure-custard-mode nil
  "Customize group for stuff not customizable in clojure-mode.")

(defcustom clojure-custard-mode-hook nil
  "Hook for clojure-custard-mode."
  :type 'hook
  :group 'clojure-custard-mode)

(defconst niclein-jvm-args
  '("-XX:+PrintGC"
    "-XX:+PrintGCDateStamps"
    "-Xmx128M"
    "-Xloggc:gc.log"))

(defcustom niclein-remote-box-config nil
  "Any java and lein configuration for a remote box.

A mapping of hostnames to important keys we need for remoting
leiningen, clojure and java.

For example:

  user@some.host.example.com
   :java /opt/jdk/jdk8/bin/java
   :lein /home/me/.lein/self-installs/leiningen-2.5.1-standalone.jar

Or in sexp form:

  ((\"user@some.host.example.com\"
   :java \"/opt/jdk/jdk8/bin/java\"
   :lein \"/home/me/.lein/self-installs/leiningen-2.5.1-standalone.jar\"))

When a buffer is visiting a file on host
\"some.host.example.com\" under \"user\" then we use the java we
find in \"/opt/jdk/jdk8/bin/java\" and the leiningen we find in
\"/home/me/.lein\" - the full path to the leiningen is needed."
  :group 'niclein
  :type '(alist
         :key-type string
         :value-type
          (plist
           :options (:java :lein)
           :value-type string)))

(defun niclein/file-name->tramp-host ()
  "Return the tramp host of the current buffer file-name

If we have a host name then we can try and use remote java."
  (let ((file-name
         (with-current-buffer (current-buffer)
           buffer-file-name)))
    (when (and file-name
               (string-match "^/[a-zA-Z]+:\\([a-zA-Z0-9-]+@[^:]+\\):.*" file-name))
      (match-string 1 file-name))))

(defconst niclein/ssh-jar
  (expand-file-name "~/work/nic-ssh/target/uberjar/nic-ssh.jar")
  "Where's my jar file that does an ssh client?")

(defun niclein/leiningen-version ()
  "Return the filename of the leiningen."
  "leiningen-2.6.1-standalone.zip")

(defun niclein/nicexec-bash-file ()
  "Just a record of what goes in the bash file for tramp lein"
  ;; This should be saved to a file somehow
  "PATH=$PATH:/opt/eFX/apps/evolve/java/jdk1.8.0_25/bin
cd $1
shift
exec $*")

(defun niclein/lein-process-command (&rest cmd)
  "Construct the Java leiningen command from CMD."
  ;; this needs the nicexec bash file, see niclein/nicexec-bash-file
  (let* ((tramp-host (niclein/file-name->tramp-host))
         (tramp-directory
          (when tramp-host
            (string-match ".*:[^/]+\\(.*\\)" default-directory)
            (match-string 1 default-directory)))
         (remote-config (kva tramp-host niclein-remote-box-config))
         (java
          (if tramp-host
              (list niclein-java "-jar" niclein/ssh-jar  tramp-host
                    "bash" "nicexec" tramp-directory
                    ;;(format "http_proxy=%s" (getenv "http_proxy"))
                    (plist-get remote-config :java))
              ;; Else
              (list niclein-java)))
         (lein-jar
          (if tramp-host
              (plist-get remote-config :lein)
              (expand-file-name (niclein/leiningen-version) "~/.lein/self-installs"))))
    (append
     java
     (list
      (concat "-Xbootclasspath/a:" lein-jar)
      "-XX:+TieredCompilation"
      "-XX:TieredStopAtLevel=1")
     niclein-jvm-args
     (list
      "-Djline.terminal=dumb" ;;"-Djline.internal.Log.debug=true"
      (concat
       "-Dleiningen.original.pwd="
       (or tramp-directory default-directory))
      "-classpath" lein-jar
      "clojure.main" "-m" "leiningen.core.main")
     cmd)))

(defconst niclein/clojure-projects
  (make-hash-table :test 'equal)
  "Hash of clojure project filenames that have repls.

We use this to associate repl processes with files in the same project.")

;;;###autoload
(define-derived-mode
  clojure-custard-mode clojure-mode "Clojure"
  "A customizable extension to `clojure-mode'."
  :group 'clojure-custard-mode
  ;; Body
  (let ((project
         (locate-dominating-file default-directory "project.clj")))
    ;; this is setting compile-command for all buffers... not sure why?
    (setq compile-command
          (s-join
           " "
           (append
            (list "cd" project ";")
            (niclein/lein-process-command "compile"))))
    (when (gethash project niclein/clojure-projects)
      (setq niclein-lein-proc (gethash project niclein/clojure-projects))
      (niclein-interaction))))


;;; Commands

(defconst niclein-keymap (make-sparse-keymap))

(defun niclein/buffer-cli-bol ()
  "Go to the beginning of the cli line."
  (interactive)
  (let ((current-line (line-number-at-pos (point)))
        (prompt-line (line-number-at-pos niclein/prompt-entry-marker)))
    (if (equal current-line prompt-line)
        (goto-char niclein/prompt-entry-marker)
        (beginning-of-line))))

(defun niclein/cli-hash ()
  "Insert a # in the command line."
  (insert "#"))


;;; CLI stuff

(defun niclein/cli ()
  "Make a command line history object."
  ;; This is probably generically useful for Emacs cli type things. It
  ;; let's you do a cli just in a buffer with not much else going
  ;; on. That's something I'm always seeming to want so perhaps other
  ;; people do.
  (let* (history len (cursor -1))
    (lambda (msg &optional arg)
      (cl-case msg
        (:new
         (push arg history)
         (setq cursor -1)
         (setq len (length history)))
        (:now (elt history cursor))
        (:prev
         (if (< (+ cursor 1) len)
             (progn
               (incf cursor)
               (elt history cursor))
             (list :error :at-the-bottom (elt history cursor))))
        (:next
         (if (> (- cursor 1) -1)
             (progn
               (decf cursor)
               (elt history cursor))
             (list :error :at-the-top (elt history cursor))))))))

(defun niclein/cli-test () ; should be a real test
  (let ((f (niclein/cli)))
    (funcall f :new "hello")
    (funcall f :new "hello world")
    (list ; all these values should be 't
     (equal (funcall f :prev) "hello world")
     (equal (funcall f :prev) "hello")
     (equal (funcall f :next) "hello world")
     (equal (funcall f :next) (list :error :at-the-top "hello world"))
     (equal (funcall f :prev) "hello")
     (equal (funcall f :prev) (list :error :at-the-bottom "hello")))))

(defconst niclein/hist (niclein/cli)
  "Variable holding the cli object.

This is actually made buffer local by `niclein-mode' so the
instance created here is not used.")

(defun niclein/cli-strip-cmd ()
  "Take the command off the command line."
  (let ((cmd 
         (buffer-substring-no-properties niclein/prompt-entry-marker (point-max))))
    (delete-region niclein/prompt-entry-marker (point-max))
    cmd))

(defun niclein/proc-insert (proc &rest args)
  (apply 'insert args)
  (set-marker (process-mark proc) (point)))

(defun niclein/send-command (process command)
  "Send COMMAND to the niclein PROCESS.

COMMAND is read from the prompt if we're in interactive mode."
  (interactive
   (list (get-buffer-process (current-buffer))
         (niclein/cli-strip-cmd)))
  (when (and command (not (equal command "")))
    (save-excursion
      (goto-char (line-beginning-position))
      (insert ";" command " >\n"))
    (funcall niclein/hist :new command)
    (goto-char (point-max))
    (niclein/proc-insert process "\n")
    (process-send-string process (concat command "\n"))))

(defun niclein/not-blank (str)
  (and str (not (equal str ""))))

(defun niclein/cli-insert-history (history)
  "Insert HISTORY at the prompt."
  (goto-char niclein/prompt-entry-marker)
  (insert history))

(defun niclein/cli-history-previous ()
  "Go to the previous item in the history."
  (niclein/cli-strip-cmd)
  (match (funcall niclein/hist :prev)
    ((list :error error value)
     (progn
       (message "%S" error)
       (niclein/cli-insert-history value)))
    ((? 'stringp prev-line)
     (niclein/cli-insert-history prev-line))))

(defun niclein/cli-history-next ()
  "Go to the next item in the history."
  (niclein/cli-strip-cmd)
  (match (funcall niclein/hist :next)
    ((list :error error value)
     (progn
       (message "%S" error)
       (niclein/cli-insert-history value)))
    ((? 'stringp next-line)
     (niclein/cli-insert-history next-line))))

(defun niclein-cli-read ()
  "Read commands for the CLI."
  (interactive)
  (let ((key (read-key-sequence "cli key: ")))
    (funcall
     (lookup-key niclein-keymap key))))

(defun niclein/init-map ()
  (define-key niclein-keymap (kbd "n") 'niclein/cli-history-next)
  (define-key niclein-keymap (kbd "p") 'niclein/cli-history-previous)
  (define-key niclein-keymap (kbd "#") 'niclein/cli-hash))

(define-generic-mode niclein-mode
  '(";")
  nil
  nil ;;'(("http://[^ \n	]+" 'link))
  nil
  (list
   (lambda ()
     (show-paren-mode)
     (smartparens-mode)
     (sp-local-pair 'niclein-mode "'" nil :actions nil)
     (goto-address-mode)
     (setq-local niclein/hist (niclein/cli)) ; appears to work even tho it's a const
     (niclein/init-map)
     (local-set-key (kbd "#") 'niclein-cli-read)
     (local-set-key (kbd "C-a") 'niclein/buffer-cli-bol)
     (local-set-key (kbd "C-o") 'goto-address-at-point)
     (local-set-key (kbd "RET") 'niclein/send-command)))
  "A mode for niclein.

\\{niclein-keymap}

Also initiates `show-paren-mode' and `smartparens-mode'.")

(defun niclein/kva (alist key)
  (kva key alist))

(defun niclein/format (object fmt-spec)
  (format fmt-spec object))

(defmacro niclein: (&rest args))

(defconst niclein/json-repl nil
  "Toggle for the json style repl.

Requires that clojure send you stuff wrapped in a JSON repl, like:

 (defmacro repl-thing [& expr]
   `(let [output (java.io.StringWriter.)
          result (binding [*out* output]
                   (print \"hello world\n\")
                   (+ 12 10))]
      (json/write-str
       {:output (.toString output)
        :result result})))

I think clojure already sends JSON maybe. Not sure.")

(defun niclein/proc-log (data)
  "A simple log for the filter, so we can see what's being filtered."
  (let ((lines (split-string data "[\r\n]")))
    (with-current-buffer (get-buffer-create "*niclein-proc-log*")
      (goto-char (point-max))
      (princ
       (format "%s %S\n" (buffer-name) lines)
       (current-buffer)))
    (when niclein/json-repl
      (with-current-buffer (get-buffer-create "*niclein-testout*")
        (goto-char (point-max))
        (let* ((outlines
                (->> lines
                  (--remove (string-match-p "^#_=>$" it))
                  (--map (let ((m (string-match "^#_=> " it)))
                           (if m
                               (replace-match "" nil nil it)
                               it)))))
               (json-key-type 'keyword))
          (-> (mapconcat 'identity outlines "")
            (json-read-from-string)
            (json-read-from-string)
            (niclein/kva :result)
            (niclein/format "%S")
            (insert)))
        (niclein:
         (switch-to-buffer-other-window "*niclein-testout*"))))))

(defun niclein/insert* (insert-func lines)
  (let ((line-list (--remove (string-match-p "\\(^$\\|^ +$\\)" it) lines)))
    (--each line-list (funcall insert-func it "\n"))))

(defun niclein/proc-filter-fn (buffer mark-func insert-func data)
  (let* ((line-list (split-string data "[\n\r]"))
         (last-line (car (reverse line-list)))
         (first-lines (reverse (cdr (reverse line-list)))))
    (with-current-buffer buffer
      (goto-char (funcall mark-func))
      (if (equal (point) (line-beginning-position))
          (cond
            ((not (and last-line (string-match-p "^[^=]+=> $" last-line)))
             (niclein/insert* insert-func line-list))
            ((not (and last-line (string-match-p "^.*[^\n]$" last-line)))
             (niclein/insert* insert-func first-lines)
             (funcall insert-func last-line))
            ;; else it's stuff where the last line is the prompt
            (t
             (niclein/insert* insert-func first-lines)
             (funcall insert-func "=> ")
             (set-marker niclein/prompt-marker (funcall mark-func))
             (set-marker niclein/prompt-entry-marker (funcall mark-func))))
          ;; Else it may be a long line or something
          (goto-char (line-beginning-position))
          (niclein/insert*
           insert-func
           (--keep (cond
                     ((string-match-p ".*#_=>" it)
                      (s-trim (car (reverse (split-string it "#_=>")))))
                     ((string-match-p "^[^=]+=> $" it) " ")
                     (t it))
                   line-list))
          ;; Why doesn't this work? we seem to need the additional: goto-char point-max
          (set-marker niclein/prompt-marker (point-max))
          (set-marker niclein/prompt-marker (point-max))
          (set-marker (funcall mark-func) (point-max))
          (goto-char (point-max))))))

(defun niclein/proc-filter (proc data)
  "Filter for the cli process."
  ;;(niclein/proc-log data) ; helps with logging
  (niclein/proc-filter-fn
   (process-buffer proc)
   (lambda () (process-mark proc))
   (lambda (&rest args) (apply 'niclein/proc-insert proc args))
   data))

(defun niclein/lein-process-sentinel (proc evt)
  (let ((msg (cond
               ((equal evt "finished\n") "*finished*")
               ((equal evt "killed\n") "*killed*")
               (t nil))))
    (if (not (stringp msg))
        (message "niclein process ended with %s" evt)
        ;; Else we know what it is - spit the message out
        (niclein-pop-lein (process-buffer proc))
        (with-current-buffer (process-buffer proc)
          (goto-char (point-max))
          (let (buffer-read-only)
            (insert msg)
            (newline))))))

;;; the window batch file for this is:
;;; @echo off
;;; java -classpath C:/users/43870810/.lein/self-installs/leiningen.jar clojure.main -m leiningen.core.main %*

(defun niclein/lein-process (name buffer &rest cmd)
  "Abstract lein process boot."
  (let ((shell-script nil))
    (if shell-script
        (apply
         'start-process
         (append (list name buffer "lein") cmd))
        ;; Else use java directly
        (let* ((default-directory
                (or
                 (locate-dominating-file
                  default-directory "project.clj")
                 default-directory))
               (tmpfile (make-temp-file "lein"))
               (lein-cmd (apply 'niclein/lein-process-command cmd))
               (args (append (list name buffer) lein-cmd)))
          (message "niclein running on %s with %S" buffer lein-cmd)
          ;;(setenv "TRAMPOLINE_FILE" tmpfile)
          ;;(setenv "LEIN_FAST_TRAMPOLINE" "y")
          ;;(message "running lein with %s" (append args cmd))
          (apply 'start-process args)))))

(defvar niclein-lein-proc nil
  "The inferiror lein process for a clojure buffer.

If a lein repl is started, this variable will point to the
process.")

(make-variable-buffer-local 'niclein-lein-proc)

(defun niclein-kill-me ()
  (interactive)
  (if (processp niclein-lein-proc)
      (delete-process niclein-lein-proc)
      ;; Else
      (message "niclein - could not fine a niclein lein process - niclein-lein-proc")))

(defun niclein-pop-lein (&optional lein-buffer)
  "Pop the lein buffer into view."
  (interactive)
  (let ((buf (or lein-buffer
                 (process-buffer niclein-lein-proc)
                 (process-buffer
                  (gethash
                   (locate-dominating-file default-directory "project.clj")
                   niclein/clojure-projects)))))
    (unless (equal (current-buffer) buf)
      (pop-to-buffer buf t))
    (with-current-buffer buf
      (goto-char (point-max)))))

;;;###autoload
(defun niclein-deps (project)
  "Run the deps command in the project."
  (interactive (list (or (expand-file-name
                          (locate-dominating-file
                           default-directory "project.clj"))
                         default-directory)))
  (let* ((proj (expand-file-name project)) ; so we can use it from eshell
         (buf (get-buffer-create (format "*niclein-deps-%s*" proj)))
         (start-pt (with-current-buffer buf (point)))
         (proc
          (niclein/lein-process
           (format "*niclein-deps-%s*" proj)
           buf "deps")))
    (set-process-sentinel proc 'niclein/lein-process-sentinel)
    (niclein-pop-lein (process-buffer proc))
    (with-current-buffer buf
      (niclein-clojure-output-mode)
      (buffer-substring start-pt (point)))))


(defconst niclein/lein-commands
  '(change    check    classpath    clean    compile
    deploy    deps    do
    help    install   jar    javac
    new    plugin    pom    release
    repl    retest    run    search
    set-version    show-profiles    test    trampoline
    uberjar    update-in    upgrade    vcs    version    with-profile)
  "List of lein commands.") ; we should really generate this from lein

(defvar niclein/command-hist nil)

(defun niclein-command (project command &rest args)
  "Run any lein COMMAND in PROJECT."
  (interactive
   (let ((dom-file (or (expand-file-name
                        (locate-dominating-file
                         default-directory "project.clj"))
                       default-directory))
         (commands (--map (cons it it) niclein/lein-commands)))
     (if current-prefix-arg
         (append
          (list dom-file)
          (split-string 
           (read-from-minibuffer
            "lein command with args: "
            nil nil nil
            'niclein/command-hist) " "))
         ;; Else do a completing read
         (list dom-file
               (completing-read
                "lein command: " commands
                nil nil nil
                'niclein/command-hist)))))
  (let* ((proj (expand-file-name project)) ; so we can use from eshell
         (buf (get-buffer-create (format "*niclein-%s-%s*" command proj)))
         (proc
          (apply
           'niclein/lein-process
           (append
            (list
             (format "*niclein-%s-%s*" command proj)
             (with-current-buffer buf
               (niclein-clojure-output-mode)
               (current-buffer)))
            (cons command args))))
         (start-pt (with-current-buffer buf (point))))
    (set-process-sentinel proc 'niclein/lein-process-sentinel)
    (niclein-pop-lein (process-buffer proc))
    (with-current-buffer buf
      (buffer-substring start-pt (point)))))

(defun niclein/output-mode-clear ()
  "Clear the output."
  (interactive)
  (let (buffer-read-only)
    (erase-buffer)))

(defconst niclein-clojure-output-mode-map (make-sparse-keymap))

(defun niclein/clojure-output-mode-map-init ()
  "Initialize the keymap."
  (define-key niclein-clojure-output-mode-map (kbd "c") 'niclein/output-mode-clear)
  (use-local-map niclein-clojure-output-mode-map))

(define-generic-mode niclein-clojure-output-mode
    nil nil nil nil
    (list
     'niclein/clojure-output-mode-map-init
     (lambda ()
       (setq buffer-read-only t)))
    "A mode for clojure output

\\{niclein-clojure-output-mode-map}
")

(defun niclein/filter (proc data)
  (with-current-buffer (process-buffer proc)
    (save-excursion
      (goto-char (point-max))
      (let (buffer-read-only)
        (insert data)))))

;;;###autoload
(defun niclein-run (&optional cmd-args)
  "Run leiningen for the current working directory."
  (interactive
   (when current-prefix-arg
     (list
      (read-from-minibuffer "program arguments: "))))
  (let* ((out-buf (format "*niclein-%s*" (buffer-name)))
         (arg-words (when cmd-args (split-string cmd-args " ")))
         (proc (apply 'niclein/lein-process "*niclein*" out-buf "run" arg-words)))
    (with-current-buffer (process-buffer proc)
      (niclein-clojure-output-mode))
    (niclein-pop-lein (process-buffer proc))
    (set-process-sentinel
     proc (lambda (proc evt)
            (let ((msg (cond
                         ((equal evt "finished\n") "*finished*")
                         ((equal evt "killed\n") "*killed*")
                         (t nil))))
              (if (not (stringp msg))
                  (message "niclein process ended with %s" evt)
                  ;; Else we know what it is - spit the message out
                (niclein-pop-lein (process-buffer proc))
                (with-current-buffer (process-buffer proc)
                  (goto-char (point-max))
                  (let (buffer-read-only)
                    (insert msg)
                    (newline)))))))))

(defmacro niclein/with-lein-buffer (&rest code)
  `(if (process-live-p ; check for liveness of the actual proc being used
        (gethash
         (locate-dominating-file default-directory "project.clj")
         niclein/clojure-projects))
       (progn
         (setq niclein-lein-proc
               (gethash
                (locate-dominating-file default-directory "project.clj")
                niclein/clojure-projects))
         (progn ,@code)
         (niclein-pop-lein (process-buffer niclein-lein-proc)))
       (message "%s has no lein process - use M-x niclein-start" (buffer-name))))

(defun niclein/munge-clj-ns (start end)
  (let* ((ns-decl
          (save-excursion
            (goto-char (point-min))
            (read (current-buffer))))
         (namespace (cadr ns-decl))
         (form
          (format
           ;;"(let [cns *ns*] (try (in-ns '%s)  %s (finally (in-ns cns))))\n"
           "(binding [*ns* (find-ns '%s)](eval '%s))\n"
           ;;"(let [cns '%s]) %s\n"
           namespace
           (buffer-substring-no-properties start end))))
    form))

(defun niclein/send-log (line)
  "Keep a log of what we're sending to various processes."
  (with-current-buffer (get-buffer-create "*niclein-send-proc-log*")
    (goto-char (point-max))
    (princ
     (format "%s %s\n" (buffer-name) line)
     (current-buffer))))

(defun niclein-eval-region (start end)
  (interactive "r")
  (let ((munged (niclein/munge-clj-ns start end)))
    (niclein/send-log munged)
    (niclein/with-lein-buffer
     (process-send-string niclein-lein-proc munged))))

(defun niclein-eval-defun (pt)
  "Eval the current clojure defn or def."
  (interactive "d")
  (save-excursion
    (goto-char pt)
    (let ((end (save-excursion (end-of-defun)(point))))
      (niclein-eval-region
       (progn
         (beginning-of-defun)
         (point))
       end))))

(defun niclein-eval-last-sexp ()
  (interactive)
  (niclein-eval-region
   (save-excursion (backward-sexp) (point)) (point)))

(defconst niclein-interaction-keymap (make-sparse-keymap))

(defun niclein/init-interaction-keymap ()
  (define-key niclein-interaction-keymap (kbd "C-x C-e") 'niclein-eval-last-sexp)
  (define-key niclein-interaction-keymap (kbd "C-c C-e") 'niclein-eval-last-sexp)
  (define-key niclein-interaction-keymap (kbd "M-C-x") 'niclein-eval-defun)
  (define-key niclein-interaction-keymap (kbd "C-c C-z") 'niclein-pop-lein))

(define-minor-mode niclein-interaction "Interact with a leiningen child process."
  nil
  "*niclein*"
  niclein-interaction-keymap
  ;; And now the body...
  (niclein/init-interaction-keymap))

;;;###autoload
(defun niclein-start ()
  "Start a leiningen repl in a process.

The process will have a rudimentary cli for entering leiningen
repl commands.  A simple history system is implemented using # as
a prefix key:

\\{niclein-keymap}

Each repl buffer has it's own history.

The repl is run in `niclein-mode'.

Once the repl is started `niclein-lein-buffer' will contain a
reference to it."
  (interactive)
  (if (process-live-p niclein-lein-proc)
      (progn
        (message "%s already has a lein process" (buffer-name))
        (niclein-pop-lein (process-buffer niclein-lein-proc)))
      ;; Else
      (let* ((source-buffer (current-buffer))
             (project (locate-dominating-file default-directory "project.clj"))
             (repl-buf (format "*niclein-repl-%s*" (buffer-name)))
             (proc (niclein/lein-process
                    "*niclein*" repl-buf
                    "repl")))
        (puthash project proc niclein/clojure-projects)
        (setq niclein-lein-proc proc)
        (niclein-pop-lein (process-buffer proc))
        (niclein-mode)
        ;; Set up the repl buffer with a bottom prompt
        (with-current-buffer (process-buffer proc)
          (setq niclein/prompt-marker (point-marker))
          ;;(set-marker-insertion-type niclein/prompt-marker nil)
          (goto-char niclein/prompt-marker)
          ;;(insert "=> ")
          (set-marker-insertion-type niclein/prompt-marker t)
          (setq niclein/prompt-entry-marker (point-marker))
          (set-marker-insertion-type niclein/prompt-entry-marker nil))
        (with-current-buffer source-buffer
          (niclein-interaction))
        (set-process-filter proc 'niclein/proc-filter))))

(defun niclein-fast-start ()
  "Start a leiningen repl in a process.

The process will have a rudimentary cli for entering leiningen
repl commands.  A simple history system is implemented using # as
a prefix key:

\\{niclein-keymap}

Each repl buffer has it's own history.

The repl is run in `niclein-mode'.

Once the repl is started `niclein-lein-buffer' will contain a
reference to it."
  (interactive)
  (if (process-live-p niclein-lein-proc)
      (progn
        (message "%s already has a lein process" (buffer-name))
        (niclein-pop-lein (process-buffer niclein-lein-proc)))
      ;; Else
      (let* ((source-buffer (current-buffer))
             (repl-buf (format "*niclein-repl-%s*" (buffer-name)))
             (proc (niclein/lein-process
                    "*niclein*" repl-buf "run" "-m" "clojure.main")))
        (setq niclein-lein-proc proc)
        (niclein-pop-lein (process-buffer proc))
        (niclein-mode)
        ;; Set up the repl buffer with a bottom prompt
        (with-current-buffer (process-buffer proc)
          (setq niclein/prompt-marker (point-marker))
          (set-marker-insertion-type niclein/prompt-marker nil)
          (goto-char niclein/prompt-marker)
          (insert "=> ")
          (set-marker-insertion-type niclein/prompt-marker t)
          (setq niclein/prompt-entry-marker (point-marker))
          (set-marker-insertion-type niclein/prompt-entry-marker nil))
        (with-current-buffer source-buffer
          (niclein-interaction))
        (set-process-filter proc 'niclein/proc-filter))))

;;;###autoload
(defalias 'niclein-repl 'niclein-start)


;;;###autoload
(defun niclein-new (project &optional template)
  "Make a new leiningen project in PROJECT directory."
  (interactive
   (if current-prefix-arg
       (list
        (read-from-minibuffer "Project name: ")
        (completing-read "Lein template: " '("app" . "app")))
     (list (read-from-minibuffer "Project name: "))))
  ;;(interactive "MProject name: ")
  (let ((proc
         (apply
          'niclein/lein-process
          (append
           (list 
            (format "*niclein-new-%s*" project)
            (get-buffer-create (format "*niclein-new-%s*" project))
            "new")
           (if template
               (list template project)
             (list project))))))
    (niclein-pop-lein (process-buffer proc))))

;;;###autoload
(defun niclein-help (sub-command)
  "Show lein help"
  (interactive
   (list
    (when current-prefix-arg
      (read-from-minibuffer "sub command: "))))
  (let ((proc
         (apply 'niclein/lein-process
                (append (list
                         "*niclein-help*"
                         (get-buffer-create "*niclein-help*")
                         "help") (when sub-command
                                   (list sub-command))))))
    (niclein-pop-lein (process-buffer proc))))

(defun niclein/tree (src-dir)
  (let* ((dir (expand-file-name src-dir))
         (content (directory-files dir t "^[^.].*")))
    (append
     content
     (->> content
       (--filter (file-directory-p it))
       (--map (niclein/tree it))))))

(defun niclein/all-files (src-dir &optional regex-filter)
  (let ((all (-flatten (niclein/tree src-dir))))
    (if regex-filter
        (--filter (string-match-p regex-filter it) all)
        all)))

(defun niclein/index (project-file-location)
  "Makes a list of base file names across the project, for completion.

Also introduces a hyphenated form in addition to any underscored
name because it's more natural to type somehow."
  (let* ((project-dir (file-name-directory project-file-location))
         (files (niclein/all-files project-dir "\\(.*\\.clj$\\|/resources/.*\\)")))
    (->> files
      (--map (cons (file-name-nondirectory it) it))
      (--map (let ((bare-name (car it))
                   (full-path (cdr it)))
               (if (string-match-p "_" bare-name)
                   (list
                    it
                    (cons (replace-regexp-in-string "_" "-" bare-name)
                          full-path))
                   it)))
      (-flatten))))

(defun niclein/index-cache ()
  (message "niclein building index cache...")
  (let* ((dir (locate-dominating-file (buffer-file-name) "project.clj"))
         (sym-name (concat "niclein/index-result_" (base64-encode-string dir)))
         (symbol (intern sym-name)))
    (when (equal current-prefix-arg -2)
      (makunbound symbol))
    (unless (boundp symbol)
      (set symbol
           `((:updated . ,(current-time))
             (:result . ,(niclein/index dir)))))
    ;; Now return it
    (kva :result (symbol-value symbol))))

(defun niclein/find-file-prompt ()
  (let ((index (niclein/index-cache)))
    (list
     (completing-read "File in project: " index nil t)
     index)))

(defun niclein-find-file (filename project-index)
  "Find FILENAME with completion via the PROJECT-INDEX.

PROJECT-INDEX is constructed with `niclein/index'."
  (interactive (niclein/find-file-prompt))
  (let ((file (kva filename project-index)))
    (find-file file)))

(defun niclein-find-file-other-window (filename project-index)
  (interactive (niclein/find-file-prompt))
  (let ((file (kva filename project-index)))
    (find-file-other-window file)))

(define-key clojure-mode-map (kbd "C-c f") 'niclein-find-file)
(define-key clojure-mode-map (kbd "C-c 4 f") 'niclein-find-file-other-window)

(provide 'niclein)

;; TODO

;; completion - http://stackoverflow.com/questions/2747294/how-to-list-the-functions-of-a-namespace
;; basically it's this:
;;
;;  (keys (ns-publics 'foo))

;;; niclein.el ends here
