;;; parinfer
;(setq parinfer-extensions '(defaults smart-tab pretty-parens evil smart-yank))

;for evil lispy too
(add-hook 'lispy-mode-hook (lambda ()
                             (setq cursor-type 'box)
                             (evil-set-cursor-color "pink")
                             (define-key lispy-mode-map "i" 'special-lispy-flow)))
