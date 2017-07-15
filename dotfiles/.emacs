;; Added by Package.el.  This must come before configurations of
;; installed packages.  Don't delete this line.  If you don't want it,
;; just comment it out by adding a semicolon to the start of the line.
;; You may delete these explanatory comments.
(package-initialize)

(setq evil-want-C-u-scroll t)

(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(cider-auto-jump-to-error nil)
 '(cider-cljs-lein-repl
   "(do (require 'cljs.repl.node) (cemerick.piggieback/cljs-repl (cljs.repl.node/repl-env)))")
 '(custom-safe-themes
   (quote
    ("251348dcb797a6ea63bbfe3be4951728e085ac08eee83def071e4d2e3211acc3" default)))
 '(evil-lispy-cursor (quote (bar . 2)))
 '(evil-show-paren-range 1)
 '(geiser-active-implementations (quote (guile)))
 '(geiser-debug-jump-to-debug-p nil)
 '(help-window-select t)
 '(indent-tabs-mode nil)
 '(ivy-mode t)
 '(ivy-use-virtual-buffers nil)
 '(js2-bounce-indent-p t)
 '(js2r-prefered-quote-type 1)
 '(org-agenda-files nil)
 '(org-hide-emphasis-markers t)
 '(org-modules
   (quote
    (org-habit org-w3m org-bbdb org-bibtex org-docview org-gnus org-info org-irc org-mhe org-rmail)))
 '(org-src-window-setup (quote current-window))
 '(org-startup-truncated nil)
 '(package-selected-packages
   (quote
    (git-gutter+ dired+ rainbow-blocks highlight-symbol geiser markdown-mode quack guile-scheme emmet-mode simple-httpd cider evil-lispy scheme-complete nodejs-repl org-pomodoro js-comint project-root project-persist find-file-in-project magithub sudo-edit evil-smartparens org-bullets which-key org-journal smart-tabs-mode auctex airline-themes web-mode color-theme json-mode js2-mode company-tern powerline-evil evil counsel ivy)))
 '(project-persist-keymap-prefix "P")
 '(project-persist-mode t)
 '(show-paren-mode t)
 '(tab-width 4)
 '(tool-bar-mode nil)
 '(whitespace-action (quote (auto-cleanup)) t)
 '(xterm-mouse-mode t))
;;(custom-set-faces
;; ;; custom-set-faces was added by Custom.
;; ;; If you edit it by hand, you could mess it up, so be careful.
;; ;; Your init file should contain only one such instance.
;; ;; If there is more than one, they won't work right.
;; '(linum ((t (:inherit (shadow default) :background "black" :foreground "brightblack" :weight bold))))
;; '(lispy-cursor-face ((t (:background "white" :foreground "black"))))
;; '(whitespace-empty ((t (:background "yellow" :foreground "firebrick"))))
;; '(whitespace-indentation ((t (:background "grey40" :foreground "grey24"))))
;; '(whitespace-space-after-tab ((t (:background "grey40" :foreground "grey24"))))
;; '(whitespace-space-before-tab ((t (:background "grey40" :foreground "grey24")))))

(load-file "~/.emacs-init.el")

(defvar local-el "~/.emacs-local.el")
(if (file-exists-p local-el)
    (load-file local-el))
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 )
