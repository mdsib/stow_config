;global evil-mode for now
(evil-mode t)

;making it behave more like vim
(define-key
  evil-normal-state-map
  (kbd "Y")
  'evil-yank-line)
(define-key evil-normal-state-map (kbd "j") 'evil-next-visual-line)
(define-key evil-normal-state-map (kbd "k") 'evil-previous-visual-line)
