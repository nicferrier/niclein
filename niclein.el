;;; niclein.el --- Nic's lein and clojure integration  -*- lexical-binding: t; -*-

;; Copyright (C) 2015  Nic Ferrier

;; Author: Nic Ferrier <nferrier@ferrier.me.uk>
;; Keywords: languages, lisp
;; Version: 0.0.10
;; Package-requires: ((shadchen "1.4")(smartparens "1.5"))
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

(defgroup clojure-custard-mode nil
  "Customize group for stuff not customizable in clojure-mode.")

(defcustom clojure-custard-mode-hook nil
  "Hook for clojure-custard-mode."
  :type 'hook
  :group 'clojure-custard-mode)

;;;###autoload
(define-derived-mode
  clojure-custard-mode clojure-mode "Clojure"
  "A customizable extension to `clojure-mode'."
  :group 'clojure-custard-mode)


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

(defvar niclein/prompt-entry-marker nil
  "Where text entry for the prompt should be in a repl buffer.")


;;; Commands

(defconst niclein-keymap (make-sparse-keymap))

(defun niclein/buffer-cli-bol ()
  "Go to the beginning of the cli line."
  (interactive)
  (goto-char niclein/prompt-entry-marker))

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
    (process-send-string process (format "%s\n" command))))

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
  nil
  nil
  (list
   (lambda ()
     (show-paren-mode)
     (smartparens-mode)
     (setq-local niclein/hist (niclein/cli)) ; appears to work even tho it's a const
     (niclein/init-map)
     (local-set-key (kbd "#") 'niclein-cli-read)
     (local-set-key (kbd "C-a") 'niclein/buffer-cli-bol)
     (local-set-key (kbd "RET") 'niclein/send-command)))
  "A mode for niclein.

\\{niclein-keymap}

Also initiates `show-paren-mode' and `smartparens-mode'.")

(defun niclein/proc-filter (proc data)
  (with-current-buffer (process-buffer proc)
    (save-excursion
      (let ((lines
             (->> (split-string data "\n")
               (--take-while (not (string-match "^\\([a-zA-Z.-]+\\)=> " it))))))
        (mapc (lambda (l)
                (goto-char niclein/prompt-marker)
                (insert (concat l "\n")))
              lines)))
    (goto-char niclein/prompt-entry-marker)))

(defun niclein/lein-process (name buffer &rest cmd)
  "Abstract lein process boot."
  (let ((shell-script nil)
        (lein-jar
         (expand-file-name
          "leiningen-2.5.1-standalone.jar"
          "~/.lein/self-installs")))
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
             (args
              (list name buffer 
                    "java" ; where's your java at?
                    (concat "-Xbootclasspath/a:" lein-jar)
                    "-XX:+TieredCompilation"
                    "-XX:TieredStopAtLevel=1"
                    (concat "-Dleiningen.original.pwd=" default-directory)
                    "-classpath" lein-jar
                    "clojure.main" "-m" "leiningen.core.main")))
        ;;(setenv "TRAMPOLINE_FILE" tmpfile)
        ;;(setenv "LEIN_FAST_TRAMPOLINE" "y")
        (message "running lein with %s" (append args cmd))
        (apply 'start-process (append args cmd))))))


(defvar niclein-lein-proc nil
  "The inferiror lein process for a clojure buffer.

If a lein repl is started, this variable will point to the
process.")

(make-variable-buffer-local 'niclein-lein-proc)

(defun niclein-pop-lein (&optional lein-buffer)
  "Pop the lein buffer into view."
  (interactive)
  (let ((buf (or lein-buffer (process-buffer niclein-lein-proc))))
    (pop-to-buffer buf)
    (with-current-buffer buf
      (goto-char (point-max)))))

;;;###autoload
(defun niclein-deps (project)
  "Run the deps command in the project."
  (interactive (list (or (locate-dominating-file
                          default-directory "project.clj")
                         default-directory)))
  (let ((proc
         (niclein/lein-process
          (format "*niclein-deps-%s*" project)
          (get-buffer-create (format "*niclein-new-%s*" project))
          "deps")))
    (niclein/pop-lein (process-buffer proc))))

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
                (niclein/pop-lein (process-buffer proc))
                (with-current-buffer (process-buffer proc)
                  (goto-char (point-max))
                  (let (buffer-read-only)
                    (insert msg)
                    (newline)))))))))

(defmacro niclein/with-lein-buffer (&rest code)
  `(if (process-live-p niclein-lein-proc)
       (progn
         (progn ,@code)
         (niclein-pop-lein (process-buffer niclein-lein-proc)))
       (message "%s has no lein process - use M-x niclein-start" (buffer-name))))

(defun niclein-eval-region (start end)
  (interactive "r")
  (niclein/with-lein-buffer
   (process-send-string
    niclein-lein-proc
    (concat
     (buffer-substring-no-properties start end)
     "\n"))))

(defun niclein-eval-last-sexp ()
  (interactive)
  (niclein-eval-region
   (save-excursion (backward-sexp) (point)) (point)))

(defconst niclein-interaction-keymap (make-sparse-keymap))

(defun niclein/init-interaction-keymap ()
  (define-key niclein-interaction-keymap (kbd "C-x C-e") 'niclein-eval-last-sexp)
  (define-key niclein-interaction-keymap (kbd "C-c C-e") 'niclein-eval-last-sexp)
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
        (niclein/pop-lein (process-buffer niclein-lein-proc)))
      ;; Else
      (let* ((repl-buf (format "*niclein-repl-%s*" (buffer-name)))
             (proc (niclein/lein-process
                    "*niclein*" repl-buf "repl")))
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
        (set-process-filter proc 'niclein/proc-filter))))

;;;###autoload
(defalias 'niclein-repl 'niclein-start)

;;;###autoload
(defun niclein-new (project)
  "Make a new leiningen project in PROJECT directory."
  (interactive "MProject name: ")
  (let ((proc
         (niclein/lein-process
          (format "*niclein-new-%s*" project)
          (get-buffer-create (format "*niclein-new-%s*" project))
          "new" project)))
    (niclein/pop-lein (process-buffer proc))))

;;;###autoload
(defun niclein-app-new (project)
  "Make a new leiningen app project in PROJECT directory."
  (interactive "MProject name: ")
  (let ((proc
         (niclein/lein-process
          (format "*niclein-new-%s*" project)
          (get-buffer-create (format "*niclein-new-%s*" project))
          "new" "app" project)))
    (niclein/pop-lein (process-buffer proc))))

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
    (niclein/pop-lein (process-buffer proc))))

(provide 'niclein)

;;; niclein.el ends here
