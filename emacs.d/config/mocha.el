(require 'ansi-color)

;; VARIABLES

(defvar mocha--last-spec-file)
(defvar mocha--last-command)
(defvar mocha--last-nearest-spec)
(defconst mocha--base-command "mocha -c")

(defconst mocha--test-block-regex
  (rx
   (or "it" "describe" "context")
   (0+ whitespace)
   (opt "(")
   (0+ whitespace)
   (or "\"" "'")
   (group (* any))
   (or "\"" "'")))

;; Private functions

(defun mocha--is-spec-file ()
  "Returns value if current file is a spec file"
  (let ((is-spec-regex  "describe\s*(?\s*[\"']\\(.*\\)[\"']"))
    (string-match is-spec-regex (buffer-string))))

(defun mocha--get-nearest-spec-description ()
  (interactive)
  (let ((old-point (point)))
    (search-backward-regexp mocha--test-block-regex)
    (goto-char old-point)
    (match-string 1 (buffer-substring-no-properties (point-min) (point-max)))))

(defun mocha--run-command (command)
  "Runs the given command in a new colorized buffer"
  (cd project-root-dir)
  (shell-command command "*mocha-output*")
  (pop-to-buffer "*mocha-output*")
  (mocha-colorize-buffer)
  (compilation-mode))

(defun mocha-colorize-buffer ()
  "Transforms the ansi color codes in the compilation buffer"
  (ansi-color-apply-on-region (point-min) (point-max)))

(defun mocha--store-last-spec-file (file)
  "Stores the given file as last spec file"
  (set-variable 'mocha--last-spec-file file))

(defun mocha--store-last-nearest-spec (spec)
  "Stores the given file as last spec file")

(defun mocha--store-last-command (cmd)
  "Stores the last command"
  (set-variable 'mocha--last-command cmd))

(defun mocha--command-from-filename (filename)
  (format "%s %s" mocha--base-command filename))

(defun mocha--command-from-spec-description (description)
  "Returns the mocha command to be run from a spec description"
  (print "Got %s" description)
  (format "%s -g %s" mocha--base-command description))

(defun mocha--run-last-spec-file ()
  "Attempts to run last ran spec file"
  (unless (boundp 'mocha--last-spec-file)
    (error "No last spec file was registered."))
  (mocha--run-command (mocha--command-from-filename mocha--last-spec-file)))

;; PUBLIC FUNCTIONS

(defun mocha-run-current-file ()
  "Runs the current spec file or last ran spec file"
  (interactive)
  (if (mocha--is-spec-file)
      (let ((command (mocha--command-from-filename (buffer-file-name))))
	(mocha--store-last-spec-file (buffer-file-name))
	(mocha--store-last-command command)
	(mocha--run-command command))
    (mocha--run-last-spec-file)))

(defun mocha-run-nearest-spec ()
  "Runs the nearest spec"
  (interactive)
  (let ((nearest-spec (mocha--get-nearest-spec-description)))
    (print (format "Nearest spec: %s " nearest-spec))
    (print "Will run %s" (mocha--command-from-spec-description nearest-spec))
    ))

(defun mocha-run-last-spec ()
  "Runs last called spec command"
  (interactive)
  (unless (boundp 'mocha--last-command)
    (error "No last command registered"))
  (mocha--run-command mocha--last-command))

(defun mocha-run-all-specs ()
  "Runs all specs"
  (interactive)
  (mocha--run-command mocha--base-command))

(provide 'mocha)
