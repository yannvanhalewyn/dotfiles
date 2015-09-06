(require 'f)
(require 'ansi-color)

(defun mocha-command ()
  ;;; Code:
  (format "cd %s && mocha -c" (mocha-root-path)))

(defun mocha-root-path ()
  (or
    (f-traverse-upwards
      (lambda (path)
	(message "Visiting %s\n\texpanded - %s" path (f-expand "test" path))
        (f-exists? (f-expand "test" path))))
    (user-error "NOT FOUND")))

(defun mocha-compile ()
  (compile (mocha-command) 'mocha-compilation-mode))
  ;; (let ((default-directory (f-join (mocha-root-path) "test")))

;; Coloring the output
(defun mocha-colorize-compilation-buffer ()
  (ansi-color-apply-on-region compilation-filter-start (point)))

(define-derived-mode mocha-compilation-mode compilation-mode "Mocha compilation"
  (add-hook 'compilation-filter-hook 'mocha-colorize-compilation-buffer))

(provide 'mocha)
