;; https://melpa.org/#/js2-mode told me to do this
(add-to-list 'auto-mode-alist '("\\.jsx?\\'" . js2-jsx-mode))
(add-to-list 'interpreter-mode-alist '("node" . js2-jsx-mode))


;; from http://company-mode.github.io/

;; from https://github.com/iquiw/company-ghc/issues/12
(with-eval-after-load 'company
  (add-to-list 'company-backends 'company-tern))

(add-hook 'js2-jsx-mode-hook (lambda ()
                               (tern-mode)
                               (js2-refactor-mode)))
(add-hook 'js2-jsx-mode-hook #'tern-mode)
(add-hook 'js2-jsx-mode-hook #'js2-refactor-mode)
(js2r-add-keybindings-with-prefix "C-@")
