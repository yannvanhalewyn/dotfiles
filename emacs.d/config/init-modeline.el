;;; -*- lexical-binding: t; -*-
;; Most code stolen from the beautiful doom-emacs modeline:
;; https://github.com/hlissner/doom-emacs/blob/master/modules/ui/doom-modeline/config.el
;; Credits to https://github.com/hlissner

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Helpers for defining modelines / segments

(defmacro def-modeline-segment! (name &rest forms)
  "Defines a modeline segment and byte compiles it."
  (declare (indent defun) (doc-string 2))
  (let ((sym (intern (format "yvh/modeline-segment--%s" name))))
    `(progn (defun ,sym () ,@forms))))

(defsubst yvh/-prepare-modeline-segments (segments)
  (cl-loop for seg in segments
           if (stringp seg)
           collect seg
           else
           collect (list (intern (format "yvh/modeline-segment--%s" (symbol-name seg))))))

(defmacro def-modeline! (name lhs &optional rhs)
  "Defines a modeline format. NAME is a symbol to identify
it (used by `yvh/modeline' for retrieval). LHS and RHS are lists of symbols of
modeline segments defined with `def-modeline-segment!'.
Example:
  (def-modeline! minimal
    (bar macro-recording \" \" buffer-info)
    (media-info major-mode))
  (yvh/set-modeline 'minimal t)"
  (let ((sym (intern (format "yvh/modeline-format--%s" name)))
        (lhs-forms (yvh/-prepare-modeline-segments lhs))
        (rhs-forms (yvh/-prepare-modeline-segments rhs)))
    `(progn
       (defun ,sym ()
         (let ((lhs (list ,@lhs-forms))
               (rhs (list ,@rhs-forms)))
           (let ((rhs-str (format-mode-line rhs)))
             (list lhs
                   (propertize
                    " " 'display
                    `((space :align-to (- (+ right right-fringe right-margin)
                                          ,(+ 1 (string-width rhs-str))))))
                   rhs-str)))))))

(defun yvh/modeline (key)
  "Returns a mode-line configuration associated with KEY (a symbol). Throws an
error if it doesn't exist."
  (let ((fn (intern (format "yvh/modeline-format--%s" key))))
    (when (functionp fn)
      `(:eval (,fn)))))

(defun yvh/set-modeline (key &optional default)
  "Set the modeline format. Does nothing if the modeline KEY doesn't exist. If
DEFAULT is non-nil, set the default mode-line for all buffers."
  (let ((modeline (yvh/modeline key)))
    (when modeline
      (setf (if default
                (default-value 'mode-line-format)
              (buffer-local-value 'mode-line-format (current-buffer)))
            modeline))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Keep track of active window

(defvar yvh/modeline-current-window (frame-selected-window))
(defun yvh/modeline|set-selected-window (&rest _)
  "Sets `yvh/modeline-current-window' appropriately"
  (let ((win (frame-selected-window)))
    (when win
      (unless (minibuffer-window-active-p win)
        (setq yvh/modeline-current-window win)))))

(defsubst active ()
  (eq (selected-window) yvh/modeline-current-window))

(add-hook 'window-configuration-change-hook #'yvh/modeline|set-selected-window)
(add-hook 'focus-in-hook #'yvh/modeline|set-selected-window)
(advice-add #'handle-switch-frame :after #'yvh/modeline|set-selected-window)
(advice-add #'select-window :after #'yvh/modeline|set-selected-window)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Vars and layout

;; fish-style modeline
(use-package shrink-path
  :commands (shrink-path-prompt shrink-path-file-mixed))

(defvar yvh/modeline-height 29
  "How tall the mode-line should be (only respected in GUI emacs).")

(defvar yvh/modeline-bar-width 3
  "How wide the mode-line bar should be (only respected in GUI emacs).")

(defvar yvh/modeline-vspc
  (propertize " " 'face 'variable-pitch)
  "TODO")

(defgroup yvh/modeline nil
  ""
  :group 'yvh)

(defface yvh/modeline-buffer-path
  '((t (:inherit (mode-line-emphasis bold))))
  "Face used for the dirname part of the buffer path."
  :group 'yvh/modeline)

(defface yvh/modeline-buffer-file
  '((t (:inherit (mode-line-buffer-id bold))))
  "Face used for the filename part of the mode-line buffer path."
  :group 'yvh/modeline)

(defface yvh/modeline-buffer-modified
  '((t (:inherit (error bold) :background nil)))
  "Face used for the 'unsaved' symbol in the mode-line."
  :group 'yvh/modeline)

(defface yvh/modeline-buffer-major-mode
  '((t (:inherit (mode-line-emphasis bold))))
  "Face used for the major-mode segment in the mode-line."
  :group 'yvh/modeline)

(defface yvh/modeline-highlight
  '((t (:inherit mode-line-emphasis)))
  "Face for bright segments of the mode-line."
  :group 'yvh/modeline)

(defface yvh/modeline-info
  `((t (:inherit (success bold))))
  "Face for info-level messages in the modeline. Used by `*vc'."
  :group 'yvh/modeline)

(defface yvh/modeline-warning
  `((t (:inherit (warning bold))))
  "Face for warnings in the modeline. Used by `*flycheck'"
  :group 'yvh/modeline)

(defface yvh/modeline-urgent
  `((t (:inherit (error bold))))
  "Face for errors in the modeline. Used by `*flycheck'"
  :group 'yvh/modeline)

(defface yvh/modeline-bar '((t (:inherit mode-line)))
  "The face used for the left-most bar on the mode-line of an active window."
  :group 'yvh/modeline)

(defface yvh/modeline-inactive-bar '((t (:inherit warning :inverse-video t)))
  "The face used for the left-most bar on the mode-line of an inactive window."
  :group 'yvh/modeline)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Helpers

;; Inspired from `powerline's `pl/make-xpm'.
(defun yvh/modeline--make-xpm (color height width)
  "Create an XPM bitmap."
  (propertize
   " " 'display
   (let ((data (make-list height (make-list width 1)))
         (color (or color "None")))
     (create-image
      (concat
       (format "/* XPM */\nstatic char * percent[] = {\n\"%i %i 2 1\",\n\". c %s\",\n\"  c %s\","
               (length (car data))
               (length data)
               color
               color)
       (apply #'concat
              (cl-loop with idx = 0
                       with len = (length data)
                       for dl in data
                       do (cl-incf idx)
                       collect
                       (concat "\""
                               (cl-loop for d in dl
                                        if (= d 0) collect (string-to-char " ")
                                        else collect (string-to-char "."))
                               (if (eq idx len) "\"};" "\",\n")))))
      'xpm t :ascent 'center))))

(defun yvh/modeline-buffer-file-name ()
  (let* ((project-root (projectile-project-root))
         (file-name-split (shrink-path-file-mixed project-root
                                                  (file-name-directory buffer-file-truename)
                                                  buffer-file-truename))
         (active (active)))
    (if (null file-name-split)
        (propertize "%b" 'face (if active 'yvh/modeline-buffer-file))
      (pcase-let ((`(,root-path-parent ,project ,relative-path ,filename) file-name-split))
        (let ((modified-faces (if (buffer-modified-p) 'yvh/modeline-buffer-modified)))
          (let ((sp-faces       (or modified-faces (if active 'font-lock-comment-face)))
                (project-faces  (or modified-faces (if active 'font-lock-string-face)))
                (relative-faces (or modified-faces (if active 'yvh/modeline-buffer-path)))
                (file-faces     (or modified-faces (if active 'yvh/modeline-buffer-file))))
            (let ((sp-props       `(,@(if sp-faces       `(:inherit ,sp-faces))      ,@(if active '(:weight bold))))
                  (project-props  `(,@(if project-faces  `(:inherit ,project-faces)) ,@(if active '(:weight bold))))
                  (relative-props `(,@(if relative-faces `(:inherit ,relative-faces))))
                  (file-props     `(,@(if file-faces     `(:inherit ,file-faces)))))
              (concat (propertize root-path-parent 'face sp-props)
                      (propertize (concat project "/") 'face project-props)
                      (if relative-path (propertize relative-path 'face relative-props))
                      (propertize filename 'face file-props)))))))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Modeline segments

(def-modeline-segment! buffer-default-directory
  "Displays `default-directory'. This is for special buffers like the scratch
buffer where knowing the current project directory is important."
  (let ((face (if (active) 'yvh/modeline-buffer-path)))
    (concat (if (display-graphic-p) " ")
            (all-the-icons-octicon
             "file-directory"
             :face face
             :v-adjust -0.05
             :height 1.25)
            (propertize (concat " " (abbreviate-file-name default-directory))
                        'face face))))

(def-modeline-segment! buffer-info
  "Combined information about the current buffer, including the current working
directory, the file name, and its state (modified, read-only or non-existent)."
  (concat (cond (buffer-read-only
                 (concat (all-the-icons-octicon
                          "lock"
                          :face 'yvh/modeline-warning
                          :v-adjust -0.05)
                         " "))
                ((buffer-modified-p)
                 (concat (all-the-icons-faicon
                          "floppy-o"
                          :face 'yvh/modeline-buffer-modified
                          :v-adjust -0.0575)
                         " "))
                ((and buffer-file-name
                      (not (file-exists-p buffer-file-name)))
                 (concat (all-the-icons-octicon
                          "circle-slash"
                          :face 'yvh/modeline-urgent
                          :v-adjust -0.05)
                         " "))
                ((buffer-narrowed-p)
                 (concat (all-the-icons-octicon
                          "fold"
                          :face 'yvh/modeline-warning
                          :v-adjust -0.05)
                         " ")))
          (if buffer-file-name
              (yvh/modeline-buffer-file-name)
            "%b")))

(def-modeline-segment! major-mode
  "The major mode, including process, environment and text-scale info."
  (propertize
   (format-mode-line mode-name)
   'face (if (active) 'yvh/modeline-buffer-major-mode)))

(def-modeline-segment! vcs
  "Displays the current branch, colored based on its state."
  (when (and vc-mode buffer-file-name)
    (let* ((backend (vc-backend buffer-file-name))
           (state   (vc-state buffer-file-name backend)))
      (let ((face   'mode-line-inactive)
            (active  (active))
            (all-the-icons-default-adjust -0.1))
        (concat "  "
                (cond ((memq state '(edited added))
                       (if active (setq face 'yvh/modeline-warning))
                       (all-the-icons-octicon
                        "git-branch"
                        :face face
                        :v-adjust -0.05))
                      ((eq state 'needs-merge)
                       (if active (setq face 'yvh/modeline-info))
                       (all-the-icons-octicon "git-merge" :face face))
                      ((eq state 'needs-update)
                       (if active (setq face 'yvh/modeline-warning))
                       (all-the-icons-octicon "arrow-down" :face face))
                      ((memq state '(removed conflict unregistered))
                       (if active (setq face 'yvh/modeline-urgent))
                       (all-the-icons-octicon "alert" :face face))
                      (t
                       (if active (setq face 'yvh/modeline-info))
                       (all-the-icons-octicon "git-branch" :face face)))
                " "
                (propertize (substring vc-mode 5) 'face (if active face))
                " ")))))

(defun yvh/ml-icon (icon &optional text face voffset)
  "Displays an octicon ICON with FACE, followed by TEXT. Uses
`all-the-icons-octicon' to fetch the icon."
  (concat (when icon
            (concat
             (all-the-icons-material icon :face face :height 1.1 :v-adjust (or voffset -0.2))
             (if text yvh/modeline-vspc)))
          (when text
            (propertize text 'face face))))

(def-modeline-segment! flycheck
  "Displays color-coded flycheck error status in the current buffer with pretty
icons."
  (when (boundp 'flycheck-last-status-change)
    (pcase flycheck-last-status-change
      ('finished (if flycheck-current-errors
                     (let-alist (flycheck-count-errors flycheck-current-errors)
                       (let ((sum (+ (or .error 0) (or .warning 0))))
                         (yvh/ml-icon "do_not_disturb_alt"
                                      (number-to-string sum)
                                      (if .error 'yvh/modeline-urgent 'yvh/modeline-warning))))
                   (yvh/ml-icon "check" nil 'yvh/modeline-info)))
      ('running     (yvh/ml-icon "access_time" nil 'font-lock-doc-face -0.25))
      ('no-checker  "")
      ('errored     (yvh/ml-icon "sim_card_alert" "Error" 'yvh/modeline-urgent))
      ('interrupted (yvh/ml-icon "pause" "Interrupted" 'font-lock-doc-face)))))

(defun yvh/modeline--macro-recording ()
  "Display current Emacs or evil macro being recorded."
  (when (and (active) (or defining-kbd-macro executing-kbd-macro))
    (concat " "
            (propertize (if (bound-and-true-p evil-this-macro)
                            (char-to-string evil-this-macro)
                          "Macro")
                        'face 'yvh/modeline-warning)
            " "
            (all-the-icons-octicon "triangle-right"
                                   :face 'yvh/modeline-warning
                                   :v-adjust -0.05)
            " ")))

(def-modeline-segment! macro-recording
  "Displays the currently recording macro"
  (let ((meta (yvh/modeline--macro-recording)))
    (or (and (not (equal meta "")) meta)
        (if buffer-file-name " %I "))))

(def-modeline-segment! bar
  "The bar regulates the height of the mode-line in GUI Emacs.
Returns \"\" to not break --no-window-system."
  (if (display-graphic-p)
      (yvh/modeline--make-xpm
       (face-background (if (active)
                            'yvh/modeline-bar
                          'yvh/modeline-inactive-bar)
                        nil t)
       yvh/modeline-height
       yvh/modeline-bar-width)
    ""))

(def-modeline-segment! point-info
  "The current position and line length of point"
  (let ((line-length (number-to-string (- (line-end-position) (line-beginning-position))))
        (buffer-length (int-to-string (count-lines (point-min) (point-max)))))
    (propertize
     (concat " L %l:" buffer-length
             " C %c:" line-length)
     'face 'mode-line-emphasis)))

(def-modeline-segment! ci-status
  (let ((status (format " CI %s  " (cis/propertized-status cis/latest-ci-status))))
    (if (active) status (propertize status 'face 'mode-line-inactive))))

(def-modeline! main
  (bar macro-recording buffer-info point-info)
  (major-mode vcs flycheck ci-status))

(yvh/set-modeline 'main t)

(provide 'init-modeline)
