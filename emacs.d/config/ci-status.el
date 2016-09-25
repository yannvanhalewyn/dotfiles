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
  (string-trim
   (shell-command-to-string "git branch 2> /dev/null | awk '{ if ( $1 == \"*\" ) { print $2 } }'")))

(defun cis/origin (branch)
  "Returns the reference to origin for given branch"
  (format "origin/%s" branch))

(defun cis/status (ref callback)
  "Fetches the ci status for the given REF and calls CALLBACK with status text"
  (message "Fetching CI status for %s" ref)
  (shell-command-to-string-async
   (format "hub ci-status %s" ref)
   callback))

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

(defun cis/latest-build-url (ref callback)
  (message "Fetching CI build url for %s" ref)
  (shell-command-to-string-async
   (format "hub ci-status %s -v | awk '{ print $2}'" ref)
   callback))

;; Public API
;; ==========
(defun cis/modeline-status ()
  (format "CI:%s  " (cis/propertized-status cis/latest-ci-status)))

(defun cis/update ()
  "Updates the cis/latest-ci-status variable asynchronously"
  (interactive)
  (cis/status (cis/origin (cis/current-branch))
              (lambda (status)
                (setq cis/latest-ci-status (string-trim status)))))

(defun cis/open-ci-build ()
  (interactive)
  (cis/latest-build-url (cis/origin (cis/current-branch))
                        (lambda (url) (browse-url url))))

(provide 'ci-status)
