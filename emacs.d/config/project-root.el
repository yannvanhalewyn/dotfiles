(defvar project-root-dir
  "The root dir of the project")

(set-variable 'project-root-dir default-directory)

(defun project-cd (dir)
  (interactive (list (read-file-name "Dir: ")))
  (cd dir)
  (set-variable 'project-root-dir dir))

(provide 'project-root)
