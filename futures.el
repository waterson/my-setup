;;; -*- lexical-binding: t; -*-
(require 'dash)
(require 'fns)
(require 'subr-x)

(defun fu-make-argv (&rest args)
  "Creates a command that can be run with `fu-run' etc.

ARGS should just be the argv for the command. Lists will be
flattened, and any `nil' value will be removed."
  (let ((args0 (-flatten args)))
    (assert (-all-p #'stringp args0))
    (record 'fu-argv args0)))

(defun fu-argv-p (x)
  "Returns `t' if the argument is a command that can be run with `fu-run'."
  (and (recordp x)
       (eq (aref x 0) 'fu-argv)))

(defun fu-argv-args (argv)
  "Returns the command-line argument list from ARGV."
  (assert (fu-argv-p argv))
  (aref argv 1))

(defun fu--maybe-quote (arg)
  "Returns ARG with (hopefully) appropriate quoting for use as a shell command.

Backquote-escapes whitespace and quotes. Even so, this will
probably not work well for nested quoted things."
  (if (string-match-p "[ \t\n\r'\"]" arg)
      (->> arg
           (replace-regexp-in-string "\n" (regexp-quote "\\n"))
           (replace-regexp-in-string "\t" (regexp-quote "\\t"))
           (replace-regexp-in-string "\"" (regexp-quote "\""))
           (replace-regexp-in-string "'" (regexp-quote "\\'"))
           (replace-regexp-in-string " " (regexp-quote "\\ ")))
    arg))

(defvar fu-log-buffer-name "*fu-log*" "The default buffer for logging output.")

(defun fu--get-log-buffer (opts)
  "Returns the buffer to use for output logging."
  (get-buffer-create (or (plist-get opts :buffer) fu-log-buffer-name)))

(defun fu--done (opts)
  (lambda () (append-string-to-buffer
              (fu--get-log-buffer opts)
              (format "[%s] *** Done.\n"
                      (format-time-string "%Y-%m-%d %H:%M:%S")))))

(defun fu--run-r (commands opts)
  (let ((buf (fu--get-log-buffer opts))
        (process-dir default-directory)
        (process-env process-environment))
    (lambda ()
      (let ((default-directory process-dir)
            (process-environment process-env))
        (if commands
            (let ((command (fu-argv-args (car commands))))
              (append-string-to-buffer
               buf
               (format "[%s] *** CWD=%s # Running:\n%s\n"
                       (format-time-string "%Y-%m-%d %H:%M:%S")
                       process-dir
                       (string-join command " ")))
              (make-process
               :name "futures-run"
               :buffer buf
               :command command
               :sentinel (lambda (process event)
                           (let ((default-directory process-dir)
                                 (process-environment process-env))
                             (if (string-prefix-p "finished" event)
                                 (funcall (fu--run-r (cdr commands) opts))
                               (funcall (or (plist-get opts :or-else)
                                            (plist-get opts :then)
                                            (fu--done opts))))))))
          (funcall (or (plist-get opts :and-then)
                       (plist-get opts :then)
                       (fu--done opts))))))))

(defun fu-run (commands &rest opts)
  "Runs a list of commands.

COMMANDS should be a list of `argvs' created using
`fu-make-argv'; COMMANDS may be an arbitrarily nested list: it
will be flattened before use."
  (let ((flattened-commands (-flatten commands)))
    (assert (seq-every-p #'fu-argv-p flattened-commands))
    (fu--run-r flattened-commands opts)))

;; In your local buffer, do:
;;
;;   (setq-local fu-environment (fu-make-environment "/my/process/dir"))
;;
;; And then you can:
;;
;;   (fu-do (fu-run "pwd"))
;;
(defvar-local fu-environment nil
  "The buffer-specific environemnt for running commands.")

(defun fu-make-environment (process-directory &rest args)
  "Creates an environemnt for running commands.

PROCESS-DIRECTORY is the directory in which processes will be run."
  (append (list :process-directory process-directory) args))

(defmacro fu-do-in (process-directory target)
  "Runs TARGET in with a `default-directory' of PROCESS-DIRECTORY."
  `(let ((default-directory ,process-directory))
     (funcall ,target)))

(defmacro fu-do (target)
  "Runs TARGET with the `default-directory' specified by `fu-environment'."
  `(fu-do-in
    (or (plist-get fu-environment :process-directory)
        default-directory)
    ,target))

(defmacro fu-do-run-in (process-directory &rest argvs)
  "Runs ARGVS with a `default-directory' of PROCESS-DIRECTORY."
  `(fu-do-in ,process-directory (fu-run ,argvs)))

(defmacro fu-do-run (argvs)
  "Runs ARGVS with the `default-directory' specified by `fu-environment'."
  `(let ((default-directory (or (plist-get fu-environment :process-directory)
                                default-directory)))
     (funcall (fu-run ,argvs))))

(defun fu! (argvs)
  (fu-do-run argvs))

(defun fu-show-command (argv)
  "Returns the command string for ARGV.

ARGV may either be an object that is `fu-argv-p' or a (possibly
nested) list of `fu-argv-p' objects."
  (cond ((fu-argv-p argv)
         (replace-regexp-in-string
          " --" (regexp-quote " \\\n--")
          (string-join (mapcar #'fu--maybe-quote (fu-argv-args argv)) " ")))
        ((listp argv)
         (string-join (mapcar #'fu-show-command argv) "\n"))
        (t
         (assert nil nil "Expected ARGV to either be `fu-argv-p' or `listp'"))))

(defalias 'fu? 'fu-show-command)

(provide 'futures)
