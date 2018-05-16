(defun re-match (r s)
  "Returns the string matched by R"
  (if (string-match r s)
      (let ((beg (car (match-data)))
            (end (cadr (match-data))))
        (if (and beg end) (substring s beg end)))))

(defun edit-packages ()
  "Goes to the main emacs config file (init-plugins.el)"
  (interactive) (find-file "~/.emacs.d/config/init-packages.el"))

(defun edit-general-behavior ()
  "Goes to the main emacs config file (init-plugins.el)"
  (interactive) (find-file "~/.emacs.d/config/general-behavior.el"))

(defun edit-evil ()
  "Goes to the general emacs config file (general-behavior.el)"
  (interactive) (find-file "~/.emacs.d/config/init-evil.el"))

(defun edit-layout ()
  "Goes to the layout emacs config file (layout.el)"
  (interactive) (find-file "~/.emacs.d/config/layout.el"))

(defun edit-todo ()
  "Goes to the todo org file (todo.org)"
  (interactive) (find-file "~/.org/todo.org"))

(defun edit-functions ()
  "Goes to the todo org file (todo.org)"
  (interactive) (find-file "~/.emacs.d/config/init-functions.el"))

(require 'cl)
(defun find-file-i (file)
  (lexical-let ((f file))
    (lambda () (interactive) (find-file (eval f)))))

(defun code-buffer? (name)
  "Returns wether or the NAME is a name for a code buffer"
  (not (string-match-p "^\*" name)))

(defun circle-code-buffers (circle-fn)
  (let ((bread-crumb (buffer-name)))
    (funcall circle-fn)
    (while
        (and
         (not (code-buffer? (buffer-name)))
         (not (equal bread-crumb (buffer-name))) )
      (funcall circle-fn))))

(defun next-code-buffer ()
  "Open next active buffer, ignoring non-code related buffers."
  (interactive)
  (circle-code-buffers 'next-buffer))

(defun previous-code-buffer ()
  "Open next active buffer, ignoring non-code related buffers."
  (interactive)
  (circle-code-buffers 'previous-buffer))

;; Save buffer when exiting insert mode
(defun save-if-code-buffer ()
  "Saves the current buffer if code buffer"
  (if (code-buffer? (buffer-name))
      (save-buffer)))

;; Clear emacs shell
(defun shell-clear-buffer ()
  (interactive)
  ;; Size one because of my 2-line prompt
  (let ((comint-buffer-maximum-size 1))
    (comint-truncate-buffer)))

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
    (cider-connect "localhost" port)))

(defun cider-make-cljs-repl ()
  "Changes the connection type of repl the buffer at point to CLJS."
  (interactive)
  (setq-local cider-repl-type "cljs"))

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

(defun ticket-number (branchname)
  "returns the ticket number (eg 4+ digits number) from branchname"
  (re-match "[0-9][0-9][0-9][0-9]" branchname))

(defun redmine-url (issue)
  "The url for the redmine ISSUE"
  (format "http://redmine.publiekeomroep.nl/issues/%s" issue))

(defun open-current-ticket-in-redmine ()
  "Opens the current ticket (fetched from the branchname) in redmine"
  (interactive)
  (browse-url (redmine-url (ticket-number (-vc-current-branch)))))

(defun neotree-project-root ()
  "Open NeoTree using the git root."
  (interactive)
  (let ((project-dir (projectile-project-root))
        (file-name (buffer-file-name)))
    (neotree-toggle)
    (if project-dir
        (progn
          (neotree-dir project-dir)
          (neotree-find file-name)))))

(defun split-window-with-rspec-alternate-file ()
  (interactive)
  (delete-other-windows)
  (split-window-horizontally)
  (windmove-right)
  (rspec-toggle-spec-and-target))

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

(defmacro filer--find-resource (prompt dirs)
  "Presents files from DIRS to the user using `projectile-completing-read'.
If users chooses a non existant file and NEWFILE-TEMPLATE is not nil
it will use that variable to interpolate the name for the new file.
NEWFILE-TEMPLATE will be the argument for `s-lex-format'.
The bound variable is \"filename\"."
  `(let* ((choices (filer--choices ,dirs))
          (filename (projectile-completing-read ,prompt (projectile-rails-hash-keys choices))))
     (find-file (projectile-expand-root (gethash filename choices)))))

(defun coffee-find-model ()
  (interactive)
  (filer--find-resource "model: " '(("frontend/src/models" "/models/\\(.+\\).coffee"))))

(defun coffee-find-component ()
  (interactive)
  (filer--find-resource "component: " '(("frontend/src/components" "/components/\\(.+\\).coffee"))))

(defun coffee-find-redux ()
  (interactive)
  (filer--find-resource "redux: " '(("frontend/src/redux" "/redux/\\(.+\\).coffee"))))

(defun coffee-find-test ()
  (interactive)
  (filer--find-resource "test: " '(("frontend/test" "/test/\\(.+\\)_spec.coffee"))))

(defun cider-find-clj ()
  (interactive)
  (filer--find-resource "src: " '(("src" "/clj/\\(.+\\).clj"))))

(defun cider-find-cljs ()
  (interactive)
  (filer--find-resource "src: " '(("src" "/cljs/\\(.+\\).cljs"))))

(defun cljs-find-card ()
  (interactive)
  (filer--find-resource "card: " '(("src/cards" "/cards/\\(.+\\).cljs"))))

(defun cljs-find-component ()
  (interactive)
  (filer--find-resource "component: " '(("src/cljs/frontend/" "/views/\\(.+\\).cljs"))))

(defun cljs-find-model ()
  (interactive)
  (filer--find-resource "model: " '(("src/clj/brightmotive/" "/models/\\(.+\\).clj"))))

(defun my-create-newline-and-enter-sexp (&rest _ignored)
  "Open a new brace or bracket expression, with relevant newlines and indent. "
  (newline)
  (indent-according-to-mode)
  (forward-line -1)
  (indent-according-to-mode))

(defvar junk-file/directory "~/.emacs.d/cache/junk/")

(defun junk-file/new ()
  "Opens a new junk file, useful for testing purpouses"
  (interactive)
  (let ((file (read-file-name "Junk Code (Enter extension): "
                              (format-time-string
                               (concat junk-file/directory "%Y-%m-%d-%H%M%S.")
                               (current-time)))))
    (make-directory (file-name-directory file) t)
    (find-file file)))

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
  (magit-push-current-to-upstream "--force-with-lease"))

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

(provide 'init-functions)
