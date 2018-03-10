;; https://melpa.org/#/js2-mode told me to do this

;this is how it was, then i changed it. hmm.
;(add-to-list 'auto-mode-alist '("\\.jsx?\\'" . web-mode))

(add-to-list 'auto-mode-alist '("\\.jsx[?]\\'" . web-mode))
(add-to-list 'auto-mode-alist '("\\.css\\'" . web-mode))
(add-to-list 'interpreter-mode-alist '("node" . web-mode))

(setq web-mode-content-types-alist
      '(("jsx"  . "\\.js[x]?\\'")))


;; from http://company-mode.github.io/

;; from https://github.com/iquiw/company-ghc/issues/12
(with-eval-after-load 'company
  (add-to-list 'company-backends 'company-tern))

(add-hook 'js2-jsx-mode-hook (lambda ()
                               (tern-mode)
                               (js2-refactor-mode)))
(add-hook 'js2-jsx-mode-hook #'tern-mode)
(add-hook 'js2-jsx-mode-hook #'js2-refactor-mode)

;(js2r-add-keybindings-with-prefix "C-@")

(add-hook 'web-mode-hook #'tern-mode)

