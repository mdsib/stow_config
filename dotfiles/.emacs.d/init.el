;;; init.el --- just a little init script

;;; Commentary:
;;; I'm putting this here so my linter doesn't complain.

;;; Code:
(electric-indent-mode t)
(electric-pair-mode t)
(show-paren-mode t)
(global-display-line-numbers-mode t)

(load-theme 'deeper-blue)

(setq scroll-margin 10
      scroll-conservatively 1
      display-line-numbers-width 3
      standard-indent 2)
()

(define-prefix-command 'spacer )
(global-set-key (kbd "M-SPC") 'spacer)

(setq backup-directory-alist '((".*" . "~/.emacs.d/private/backup"))
      auto-save-file-name-transforms '((".*" "~/.emacs.d/private/backup" t))
      backup-by-copying t    ; Don't delink hardlinks
      version-control t      ; Use version numbers on backups
      delete-old-versions t  ; Automatically delete excess backups
      kept-new-versions 10   ; how many of the newest versions to keep
      kept-old-versions 5)   ; and how many of the old
 
(require 'package)

(eval-and-compile
  (add-to-list 'package-archives '("org" . "http://orgmode.org/elpa/"))
  (add-to-list 'package-archives '("melpa" . "http://melpa.org/packages/"))
  (add-to-list 'package-archives '("melpa-stable" . "http://stable.melpa.org/packages/")))

(setq package-enable-at-startup nil)
(package-initialize)

(unless (package-installed-p 'use-package)
  (package-refresh-contents)
  (package-install 'use-package))

(eval-when-compile
  (require 'use-package))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;; PACKAGES ;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(use-package ag :ensure t)

(use-package auto-highlight-symbol
  :ensure t)

;; (use-package cask :ensure t) ;; how do i do this

(use-package cider
  :ensure t)

(use-package color-theme-buffer-local
  :ensure t)

(use-package company
  :ensure t
  :config
  (global-company-mode t))

(use-package counsel-projectile
  :ensure t
  :after (projectile)
  :config
  (define-key projectile-command-map "A" 'counsel-projectile-rg))

(use-package image-dired+ :ensure t)

(use-package evil
  :ensure t
  :config
  (evil-mode t)
  (add-to-list 'evil-emacs-state-modes 'racket-describe-mode))

(use-package evil-lispy
  :ensure t
  :hook (racket-mode lisp-mode emacs-lisp-mode))

(use-package exec-path-from-shell
  :ensure t
  :config
  (when (memq window-system '(mac ns x))
    (exec-path-from-shell-initialize)))

(use-package flycheck
  :ensure t
  :after (web-mode lsp)
  :init
  (global-flycheck-mode)
  (defun my/use-flow-from-node-modules ()
    "TODO: de-hardcode version somehow"
    (let* ((root (locate-dominating-file
  		  (or (buffer-file-name) default-directory)
  		  "node_modules"))
	   (flow (and root
		      (expand-file-name "node_modules/flow-bin/flow-osx-v0.91.0/flow"
					root))))
      (when (and flow (file-executable-p flow))
  	(setq-local flycheck-javascript-flow-executable flow))))
  (defun my/use-eslint-from-node-modules ()
    (let* ((root (locate-dominating-file
  		  (or (buffer-file-name) default-directory)
  		  "node_modules"))
  	   (eslint (and root
  			(expand-file-name "node_modules/eslint/bin/eslint.js"
  					  root))))
      (when (and eslint (file-executable-p eslint))
  	(setq-local flycheck-javascript-eslint-executable eslint))))
  :config
  (add-hook 'flycheck-mode-hook #'my/use-flow-from-node-modules)
  (add-hook 'flycheck-mode-hook #'my/use-eslint-from-node-modules)
  (setq-default flycheck-disabled-checkers
		(append flycheck-disabled-checkers
			'(javascript-jshint)))
  (flycheck-add-mode 'javascript-eslint 'web-mode)
  (flycheck-add-mode 'javascript-flow 'web-mode)
  (flycheck-add-next-checker 'javascript-flow 'javascript-flow-coverage))

(use-package flycheck-flow :ensure t)

(use-package git-gutter
  :ensure t
  :config
  (global-git-gutter-mode t))

(use-package ivy
  :ensure t
  :config
  (ivy-mode t))

(use-package import-js :ensure t)

(use-package lsp-mode
  :ensure t
  :commands lsp
  :config
  (add-hook 'web-mode-hook #'lsp))

(use-package lsp-ui :ensure t :commands lsp-ui-mode
  ; :config (add-hook 'lsp-mode-hook 'lsp-ui-mode) ;; lsp-ui-mode is evil, but has some useful commands
  )

(use-package company-lsp
  :ensure t
  :after (company)
  :commands company-lsp
  :config
  (push 'company-lsp company-backends))

(use-package magit :ensure t)

;;(use-package nvm :ensure t) ;; this is wrong too

(use-package org
  :config
  (visual-line-mode t)
  (org-indent-mode t))

(use-package org-pomodoro :ensure t)

(use-package prettier-js
  :ensure t
  :after (web-mode)
  :config
  (add-hook 'web-mode-hook 'prettier-js-mode))

(use-package projectile
  :ensure t
  :bind-keymap
  ("M-SPC p" . projectile-command-map)
  :config
  (projectile-mode +1)
  (setq projectile-enable-caching t
	projectile-completion-system 'ivy
	projectile-require-project-root nil)
        projectile-switch-project-action 'projectile-dired)

(use-package racket-mode
  :ensure t)

(use-package graphql-mode :ensure t)

(use-package smooth-scrolling
  :ensure t
  :config
  (smooth-scrolling-mode 1)
  (setq smooth-scroll-margin 5))

;(use-package evil-smartparens :ensure t
;  :after (smartparens)
;  :config
;  (add-hook 'smartparens-enabled-hook #'evil-smartparens-mode))

(use-package smartparens :ensure t)

(defun mdsib/setup-tide-mode ()
  (interactive)
  (tide-setup)
  (flycheck-mode +1)
  (setq flycheck-check-syntax-automatically '(save mode-enabled))
  (eldoc-mode +1)
  (tide-hl-identifier-mode +1)
  ;; company is an optional dependency. You have to
  ;; install it separately via package-install
  ;; `M-x package-install [ret] company`
  (company-mode +1))

(use-package tide
  :ensure t
  :after (web-mode flycheck company)
  :config
  (add-hook 'web-mode-hook
	    (lambda ()
	      (when (string-equal "ts" (file-name-extension buffer-file-name))
		(mdsib/setup-tide-mode))))
  (flycheck-add-next-checker 'javascript-eslint 'jsx-tide 'append))

(setq-local mds/web-mode-reg "\\.[tj]sx?\\(\\.flow\\)?\\'")
(eval `(use-package web-mode
  :ensure t
  :mode ,mds/web-mode-reg
  :after (smartparens)
  :config
  (setq web-mode-content-types-alist '(("jsx" . ,mds/web-mode-reg))
	web-mode-enable-auto-quoting nil)
  (defun my-web-mode-hook ()
    (setq web-mode-enable-auto-pairing nil))
  (add-hook 'web-mode-hook  'my-web-mode-hook)
  ;(defun sp-web-mode-is-code-context (id action context)
  ;  (and (eq action 'insert)
  ;	 (not (or (get-text-property (point) 'part-side)
  ;		  (get-text-property (point) 'block-side)))))
  ;(sp-local-pair 'web-mode "<" nil :when '(sp-web-mode-is-code-context))
  ))

(use-package which-key
  :ensure t
  :config (which-key-mode t))

(use-package yasnippet
  :ensure t)

;; custom stuff

(defun me/load-buffer-local-theme ()
  (if (boundp 'me/colortheme) (load-theme-buffer-local 'me/colortheme (current-buffer))))

(defun say-message ()
  "Say message on osx after time."
  (interactive)
  (let* ((msg (read-from-minibuffer "say this message: "))
	(relative-time (read-from-minibuffer "when: ")))
    (run-at-time relative-time nil (lambda (mesg) (shell-command (concat "say -r 150 " mesg))) msg)))

;;(add-hook 'fundamental-mode )

;; from https://www.emacswiki.org/emacs/ToggleWindowSplit
(defun toggle-window-split ()
  "Toggle between horizontal and vertical split when there are exactly 2 windows."
  (interactive)
  (if (= (count-windows) 2)
      (let* ((this-win-buffer (window-buffer))
	     (next-win-buffer (window-buffer (next-window)))
	     (this-win-edges (window-edges (selected-window)))
	     (next-win-edges (window-edges (next-window)))
	     (this-win-2nd (not (and (<= (car this-win-edges)
					 (car next-win-edges))
				     (<= (cadr this-win-edges)
					 (cadr next-win-edges)))))
	     (splitter
	      (if (= (car this-win-edges)
		     (car (window-edges (next-window))))
		  'split-window-horizontally
		'split-window-vertically)))
	(delete-other-windows)
	(let ((first-win (selected-window)))
	  (funcall splitter)
	  (if this-win-2nd (other-window 1))
	  (set-window-buffer (selected-window) this-win-buffer)
	  (set-window-buffer (next-window) next-win-buffer)
	  (select-window first-win)
	  (if this-win-2nd (other-window 1))))))

(define-key 'spacer "t" 'toggle-window-split)
(define-key 'spacer "f" 'magit-blob-next)
(define-key 'spacer "b" 'magit-blob-previous)
(define-key 'spacer "g" 'say-message)

(define-prefix-command 'lsp-prefix)
(define-key 'spacer "l" 'lsp-prefix)
(define-key 'lsp-prefix "d" 'lsp-ui-peek-find-definitions)
(define-key 'lsp-prefix "r" 'lsp-ui-peek-find-references)
(define-key 'lsp-prefix "b" 'lsp-ui-peek-jump-backward)
(define-key 'lsp-prefix "f" 'lsp-ui-peek-jump-forward)
(define-key 'lsp-prefix "t" 'lsp-ui-sideline-toggle-symbols-info)

;; move around sanely
(define-prefix-command 'windmover)
(define-key 'spacer "w" 'windmover)

(define-key 'windmover "h" 'windmove-left)
(define-key 'windmover "j" 'windmove-down)
(define-key 'windmover "k" 'windmove-up)
(define-key 'windmover "l" 'windmove-right)

;; org mode custom commands
(define-prefix-command 'org-keys)
(define-key 'spacer "o" 'org-keys)

(define-key 'org-keys "t" 'org-todo)


(defun find-file-here (here)
  "Start 'find-file' somewhere.
HERE: a file path to find"
  (interactive)
  (save-excursion
    (cd here)
    (call-interactively 'counsel-find-file)))

;; open my notes
(defun find-note ()
  "Find file in notes."
  (interactive)
  (find-file-here "~/notes"))

(define-key 'spacer "n" 'find-note)

;; generated
(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(magit-diff-highlight-indentation nil)
 '(magit-diff-refine-hunk (quote all))
 '(package-selected-packages
   (quote
    (graphql-mode import-js company-lsp lsp-ui lsp-mode-ui lsp-mode image-dired+ diredp dired-p org-pomodoro evil-smartparens smartparens-javascript smart-parens smartparens nvm magit-popup flow-minor-mode color-theme-buffer-local yasnippet cider flycheck-flow exec-path-from-shell flycheck flymake-eslint git-gutter magit-gutter evil-lispy racket-mode auto-highlight-symbol highlight-symbol smooth-scrolling prettier-js prettier prettierjs ag counsel-projectile company-tern web-mode company tern which-key projectile magit ivy evil use-package)))
 '(safe-local-variable-values (quote ((standard-indent . 2))))
 '(split-height-threshold 95)
 '(split-window-preferred-function (quote split-window-sensibly)))
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 )
(provide 'init)
;;; init.el ends here
