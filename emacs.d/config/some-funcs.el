(defun call-ag-with (query)
  (interactive (list (read-string "Search for: ")))
  (shell-command (format "Ag --ignore-dir node_modules %s" query) "*ag-results*")
  (pop-to-buffer "*ag-results*")
  (compilation-mode))

(provide 'some-funcs)
