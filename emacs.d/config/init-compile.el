(require-package 'swift-mode)

(require 'cl)
(defun* get-closest-pathname (&optional (file "makefile"))
  (let ((root (expand-file-name "/")))
    (expand-file-name file
                      (loop
                       for d = default-directory then (expand-file-name ".." d)
                       if (file-exists-p (expand-file-name file d))
                       return d
                       if (equal d root)
                       return nil))))

(require 'compile)
(add-hook 'c-mode-hook (lambda ()
                         (let ((nearest-makefile (get-closest-pathname)))
                           (set (make-local-variable 'compile-command)
                                (format "cd %s && make"
                                        (file-name-directory nearest-makefile))))))

(provide 'init-compile)