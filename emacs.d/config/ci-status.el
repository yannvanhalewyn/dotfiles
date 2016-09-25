(require 'util)

(defface cis/failed '((t :foreground "Red" :weight bold)) "")
(defface cis/pending '((t :foreground "Orange" :weight bold)) "")
(defface cis/success '((t :foreground "Green" :weight bold)) "")

(defvar cis/latest-ci-status "no-status")
(defvar cis/no-status-char "-")
(defvar cis/failed-status-char "✘")
(defvar cis/success-status-char "✓")
(defvar cis/pending-status-char "●")

(defun cis/current-branch ()
  "Asks git what the current branch is"
  (shell-command-to-string "git branch 2> /dev/null | awk '{ if ( $1 == \"*\" ) { print $2 } }'"))

(defun cis/origin (branch)
  "Returns the reference to origin for given branch"
  (format "origin/%s" branch))

(defun cis/status (&optional ref)
  "Fetches the ci status for the given reference"
  (shell-command-to-string (format "hub ci-status %s" ref)))

(defun cis/propertized-status (status)
  "transforms a CI status string into a representative colorized char"
  (cond
   ((string-equal status "success")
    (propertize cis/success-status-char 'face 'cis/success))
   ((string-equal status "failed")
    (propertize cis/failed-status-char 'face 'cis/failed))
   ((string-equal status "pending")
    (propertize cis/pending-status-char 'face 'cis/pending))
   (t cis/no-status-char)))

(defun cis/modeline-status ()
  (format "CI:%s" (cis/propertized-status cis/latest-ci-status)))

(defun cis/update ()
  "Updates the cis/latest-ci-status variable"
  (interactive)
  (setq cis/latest-ci-status
        (string-trim
         (cis/status (cis/origin (cis/current-branch))))))

(provide 'ci-status)
