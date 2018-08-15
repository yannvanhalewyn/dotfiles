(defun re-match (r s)
  "Returns the string matched by R"
  (if (string-match r s)
      (let ((beg (car (match-data)))
            (end (cadr (match-data))))
        (if (and beg end) (substring s beg end)))))

(defun add-hooks (hook modes)
  "Applies hook to all mode (hooks)"
  (dolist (mode modes)
    (add-hook mode hook)))

(defun build-keymap (&rest key-commands)
  "Builds a new sparse keymap containing given commands"
  (let ((new-map (make-sparse-keymap)))
    (while (not (cl-endp key-commands))
      (define-key new-map (kbd (pop key-commands)) (pop key-commands)))
    new-map))

(defun shell-command-to-string-async (command callback)
  "Execute shell command COMMAND asynchronously in the
  background. Return the temporary output buffer which command is
  writing to during execution. When the command is finished, call
  CALLBACK with the resulting output as a string."
  (lexical-let ((output-buffer (generate-new-buffer " *tmp-async*"))
                (callback callback))
    (set-process-sentinel
     (start-process "Shell" output-buffer shell-file-name shell-command-switch command)
     (lambda (process signal)
       (when (memq (process-status process) '(exit signal))
         (with-current-buffer output-buffer
           (let ((output-string
                  (buffer-substring-no-properties
                   (point-min)
                   (point-max))))
             (funcall callback output-string)))
         (kill-buffer output-buffer))))
    output-buffer))

(defun operate-current-buffer-file (fn)
  (let ((name (buffer-name))
        (filename (buffer-file-name)))
    (if (not (and filename (file-exists-p filename)))
        (error "Buffer '%s' is not visiting a file!" name)
      (funcall fn filename))))

(defun rename-current-buffer-file ()
  "Renames current buffer and file it is visiting."
  (interactive)
  (operate-current-buffer-file
   (lambda (filename)
     (let ((new-name (read-file-name "New name: " filename)))
       (if (get-buffer new-name)
           (error "A buffer named '%s' already exists!" new-name)
         (rename-file filename new-name 1)
         (rename-buffer new-name)
         (set-visited-file-name new-name)
         (set-buffer-modified-p nil)
         (message "File '%s' successfully renamed to '%s'"
                  filename (file-name-nondirectory new-name)))))))

(defun copy-current-buffer-file ()
  "Copies the current buffer file and visits the copy"
  (interactive)
  (operate-current-buffer-file
   (lambda (filename)
     (let ((new-name (read-file-name "Copy to: " filename)))
       (if (get-buffer new-name)
           (error "A buffer named '%s' already exists!" new-name)
         (copy-file filename new-name)
         (find-file new-name)
         (message "File '%s' successfully copied to '%s'"
                  filename (file-name-nondirectory new-name)))))))

(defun delete-current-buffer-file ()
  "Copies the current buffer file and visits the copy"
  (interactive)
  (operate-current-buffer-file
   (lambda (filename)
     (when (y-or-n-p (format "Permanently delete %s?" filename))
       (delete-file filename)
       (kill-this-buffer)))))

(require 'cl)
(defun find-file-i (file)
  (lexical-let ((f file))
    (lambda () (interactive) (find-file (eval f)))))

(defun dired-current-dir ()
  (interactive)
  (dired ""))

(defun cycle-theme (&optional reverse)
  "Load the next (or previous if REVERSE is true) available theme."
  (interactive)
  (let* ((current-theme (car custom-enabled-themes))
         (all-themes (if reverse
                         (reverse (custom-available-themes))
                       (custom-available-themes)))
         (first-theme (car all-themes))
         (go (lambda (theme)
               (message "Loading %s." (symbol-name theme))
               (disable-theme current-theme)
               (load-theme theme)))
         theme)
    (if (catch 'done
          (while (setq theme (pop all-themes))
            (if (and (eq theme current-theme)
                     (setq theme (pop all-themes)))
                (progn
                  (funcall go theme)
                  (throw 'done nil))))
          t)
        (funcall go first-theme))))

(defun file->str (path)
  "Return path's file content."
  (with-temp-buffer
    (insert-file-contents path)
    (buffer-string)))

(defun cider-connect-local ()
  "Directly connects to an nrepl server by repl started in project
root, or asks for a repl port to connect form anywhere."
  (interactive)
  (let ((port (or (ignore-errors (file->str (projectile-expand-root ".nrepl-port")))
                  (read-number "Cider connect to port: "))))
    (cider-connect `(:host "localhost" :port ,port))))

(defun chrome-reload (&optional focus)
  "Use osascript to tell Google Chrome to reload. If optional argument
  FOCUS is non-nil, give Chrome the focus as well."
  (interactive "P")
  (let ((cmd (concat "osascript -e 'tell application \"Google Chrome\" "
                     "to (reload (active tab of (window 1)))"
                     (if focus " & activate" "")
                     "'")))
    (with-temp-buffer (shell-command cmd t))))

(defun -vc-current-project-remote-url ()
  "The base remote url for current git remote"
  (string-trim (shell-command-to-string "hub browse -u")))

(defun -vc-current-branch ()
  "Asks git what the current branch is"
  (string-trim
   (shell-command-to-string "git branch 2> /dev/null | awk '{ if ( $1 == \"*\" ) { print $2 } }'")))

(defun -vc-url-for-file (repo filepath)
  "The url for a FILEPATH on REPO (url). Will point to optional BRANCH-REF"
  (format "%s/%s" repo filepath))

(defun browse-url (url)
  "Open URL in browser"
  (call-process "open" nil nil nil url))

(defun browse-current-line-github ()
  "Go to the current file's current line on the codebase site."
  (interactive)
  (let* ((line-num (number-to-string (line-number-at-pos)))
         (file-path (replace-regexp-in-string
                     (expand-file-name (vc-find-root (buffer-file-name) ".git"))
                     ""
                     (buffer-file-name)))
         (url (format "%s/%s#L%s" (-vc-current-project-remote-url) file-path line-num)))
    (browse-url url)))

(defun neotree-project-root ()
  "Open NeoTree using the git root."
  (interactive)
  (let ((project-dir (projectile-project-root))
        (file-name (buffer-file-name)))
    (neotree-toggle)
    (when project-dir
      (neotree-dir project-dir)
      (neotree-find file-name))))

(defun buff-swap ()
  "Swap current buffer with buffer to the left or right."
  (interactive)
  (let* ((other-win (or (windmove-find-other-window 'right)
                        (windmove-find-other-window 'left)))
         (buf-this-buf (window-buffer (selected-window))))
    (if (null other-win)
        (error "No other split")
      ;; swap top with this one
      (set-window-buffer (selected-window) (window-buffer other-win))
      ;; move this one to top
      (set-window-buffer other-win buf-this-buf)
      (select-window other-win))))

;; Custom file finders
(defun filer--choices (dirs)
  "Uses `projectile-dir-files' function to find files in directories.
The DIRS is list of lists consisting of a directory path and regexp to filter files from that directory.
Returns a hash table with keys being short names and values being relative paths to the files."
  (let ((hash (make-hash-table :test 'equal)))
    (loop for (dir re) in dirs do
          (loop for file in (projectile-dir-files (projectile-expand-root dir)) do
                (when (string-match re file)
                  (puthash (match-string 1 file) file hash))))
    hash))

(defun my-create-newline-and-enter-sexp (&rest _ignored)
  "Open a new brace or bracket expression, with relevant newlines and indent. "
  (newline)
  (indent-according-to-mode)
  (forward-line -1)
  (indent-according-to-mode))

(defvar junk-file/directory "~/.emacs.d/cache/junk/")

(defun junk-file/find ()
  (interactive)
  (counsel-find-file junk-file/directory))

(defun comment-as-title()
  (interactive)
  (move-beginning-of-line 1)
  (kill-line)
  (yank)
  (open-line 1)
  (next-line 1)
  (yank)
  (move-beginning-of-line 1)
  (forward-word)
  (backward-word)
  (replace-regexp "." "=" nil (point) (line-end-position)))

(defun comment-as-title--bm()
  (interactive)
  (forward-line -1)
  (newline)
  (insert-char ?\; 81)
  (move-beginning-of-line nil))

(defun yank-from-revision-buffer ()
  "Revision buffers are full of \+ and \- characters at the beginning
of lines. This function will yank the current region from a revision
buffer and sanitize it."
  (interactive)
  (let ((s (buffer-substring (region-beginning) (region-end))))
    (kill-new (replace-regexp-in-string "^." "" s))
    (deactivate-mark)
    (move-beginning-of-line 1)))

(defun force-push-with-lease ()
  (interactive)
  (magit-push-current-to-pushremote "--force-with-lease"))

(defun git-unwhip ()
  (interactive)
  (magit-reset-soft "HEAD~1"))

(defun my-ivy-completing-read-with-symbol-def
    (prompt collection
            &optional predicate require-match initial-input
            history def inherit-input-method)
  "Same as `ivy-completing-read' but with different handling of DEF.

Specifically, if DEF is a symbol, it is converted to a string. Useful for cljr's completing read usage when adding missing libspec."
  (ivy-completing-read
   prompt collection predicate require-match initial-input
   history (if (symbolp def) (symbol-name def) def) inherit-input-method))

(defun org-current-is-todo ()
  (string= "TODO" (org-get-todo-state)))

(defun my-org-agenda-skip-all-siblings-but-first ()
  "Skip all but the first non-done entry. Useful for capturing project
next-actions in GTD"
  (let (should-skip-entry)
    (unless (org-current-is-todo)
      (setq should-skip-entry t))
    (save-excursion
      (while (and (not should-skip-entry) (org-goto-sibling t))
        (when (and (org-current-is-todo) (member "@work" (org-get-tags)))
          (setq should-skip-entry t))))
    (when should-skip-entry
      (or (outline-next-heading)
          (goto-char (point-max))))))

(defun view-test-file-in-other-window ()
  (interactive)
  (delete-other-windows)
  (view-file-other-window
   (projectile-find-implementation-or-test (buffer-file-name)))
  (read-only-mode -1))

(defun yvh/jump-to-repl ()
  (interactive)
  (when-let ((repl-buffer (first (cider-repl-buffers))))
    (split-window-below)
    (evil-window-down 1)
    (switch-to-buffer (buffer-name repl-buffer))
    (end-of-buffer)
    (enlarge-window (- 12 (window-height)))))

(provide 'init-functions)
