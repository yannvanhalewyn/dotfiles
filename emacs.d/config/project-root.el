(defvar project-root-dir
  "The root dir of the project")

(set-variable 'project-root-dir default-directory)

(defadvice cd (before cd (dir))
  (set-variable 'project-root-dir dir))

(ad-activate 'cd)

(provide 'project-root)
