;;; ~/dotfiles/doom.d/flycheck-test.el -*- lexical-binding: t; -*-

;; (defun mychecker/start (checker callback)
;;   (message "My checker START")
;;   (callback 'finished '()))

(defun mychecker--start (checker callback)
  "Clean up errors when done.

CHECKER is the checker (eglot).
CALLBACK is the function that we need to call when we are done, on all the errors."
  (message "START")
  (message "WHHYYYYY")
  (cl-labels
      ((flymake-diag->flycheck-err
        (diag)
        (with-current-buffer (flymake--diag-buffer diag)
          (flycheck-error-new-at-pos
           (flymake--diag-beg diag)
           (pcase (flymake--diag-type diag)
             ('eglot-note 'info)
             ('eglot-warning 'warning)
             ('eglot-error 'error)
             (_ (error "Unknown diagnostic type, %S" diag)))
           (flymake--diag-text diag)
           :end-pos (flymake--diag-end diag)
           :checker checker
           :buffer (current-buffer)
           :filename (buffer-file-name)))))
    ;; NOTE: Setting up eglot to automatically create flycheck errors for the buffer.
    (eglot-flymake-backend
     (lambda (flymake-diags &rest _)
       (message "CALLBACK?")
       (funcall callback
                'finished
                (mapcar #'flymake-diag->flycheck-err flymake-diags))))
    ;; NOTE: Forcefully trigger a check in the buffer (function name is confusing)
    ;; (flycheck-buffer)
    ))

(flycheck-define-generic-checker 'my-eglot
  ""
  :start #'mychecker--start
  :modes '(prog-mode text-mode))

(push 'my-eglot flycheck-checkers)

;; https://github.com/flycheck/flycheck/issues/1592
;; Hypothesis: the callback is not called when there are no unreported (new)
;; errors. In the issue, flycheck author said to always call the callback with
;; whatever is there, and retrigger flycheck whenever lsp errors come in.
(defun my-checker-enable ()
  (interactive)
  ;; (when-let ((current-checker (flycheck-get-checker-for-buffer)))
  ;;   (unless (equal current-checker 'eglot)
  ;;     (flycheck-add-next-checker 'eglot current-checker)))
  (flycheck-add-mode 'my-eglot major-mode)
  (flycheck-mode -1)
  (flycheck-mode 1)
  (flymake-mode -1))

;; (setq flycheck-checkers (delete 'my-eglot flycheck-checkers))
