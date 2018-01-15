;; https://melpa.org/#/js2-mode told me to do this

;this is how it was, then i changed it. hmm.
;(add-to-list 'auto-mode-alist '("\\.jsx?\\'" . web-mode))

(add-to-list 'auto-mode-alist '("\\.jsx?" . web-mode))
(add-to-list 'interpreter-mode-alist '("node" . web-mode))


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

;; (defadvice web-mode-highlight-part (around tweak-jsx activate)
;;   (if (or (equal web-mode-content-type "javascript") (equal web-mode-content-type "jsx"))
;;       (let ((web-mode-enable-part-face nil))
;;         ad-do-it)
;;     ad-do-it))

(setq web-mode-extra-snippets
      '(("js" . (("preact-base" .
                  "import { h, Component} from 'preact';\n\nclass | extends Component {\n}")))))
