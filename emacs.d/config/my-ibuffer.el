(eval-after-load 'ibuffer
  '(progn
    (evil-set-initial-state 'ibuffer-mode 'normal)
    (evil-define-key 'normal ibuffer-mode-map
      (kbd "j") 'ibuffer-forward-line
      (kbd "k") 'ibuffer-backward-line
      (kbd "o") 'ibuffer-visit-buffer
    )
  )
)
