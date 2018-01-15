;global evil-mode for now
(evil-mode t)

;default state for modes
(evil-set-initial-state 'dired-mode 'emacs)
(evil-set-initial-state 'erc-mode 'emacs)
(evil-set-initial-state 'help-mode 'emacs)
(evil-set-initial-state 'Custom-mode 'emacs)

;making it behave more like vim
(define-key
  evil-normal-state-map
  (kbd "Y")
  'evil-yank-line)
(define-key evil-normal-state-map (kbd "j") 'evil-next-visual-line)
(define-key evil-normal-state-map (kbd "k") 'evil-previous-visual-line)
