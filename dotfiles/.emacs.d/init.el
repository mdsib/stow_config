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

(use-package cider
  :ensure t)

;;(use-package color-theme-buffer-local
;;  :ensure t)

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
  (add-to-list 'evil-emacs-state-modes 'racket-describe-mode)
  (add-to-list 'evil-emacs-state-modes 'tide-project-errors-mode)
  (add-to-list 'evil-emacs-state-modes 'tide-references-mode))

(use-package exec-path-from-shell
  :ensure t
  :config
  (when (memq window-system '(mac ns x))
    (exec-path-from-shell-initialize)))

(use-package flycheck
  :ensure t
  :after (web-mode)
  :init
  (global-flycheck-mode)
  :config
  (setq-default flycheck-disabled-checkers
		(append flycheck-disabled-checkers
			'(javascript-jshint)))
  (flycheck-add-mode 'javascript-eslint 'web-mode)
  (flycheck-add-mode 'typescript-tslint 'web-mode)
  (flycheck-add-next-checker 'typescript-tslint '(t . jsx-tide) 'append))

(use-package git-gutter
  :ensure t
  :config
  (global-git-gutter-mode t))

(use-package ivy
  :ensure t
  :config
  (ivy-mode t))

(use-package magit :ensure t)

(use-package org
  :config
  (visual-line-mode t)
  (org-indent-mode t))

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

(defun me/setup-tide-mode ()
  "Set up tide mode."
  (interactive)
  (tide-setup)
  (flycheck-mode +1)
  (setq flycheck-check-syntax-automatically '(save mode-enabled))
  (eldoc-mode +1)
  (tide-hl-identifier-mode +1)
  (company-mode +1))

(use-package tide
  :ensure t
  :after (web-mode flycheck company)
  :config
  (add-hook 'web-mode-hook
	    (lambda ()
	      (when (string-match "tsx?" (file-name-extension buffer-file-name))
		(me/setup-tide-mode)))))

(setq-local mds/web-mode-reg "\\.[tj]sx?\\'")
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
  ))

(use-package which-key
  :ensure t
  :config (which-key-mode t))

(use-package yasnippet
  :ensure t
  :config (yas-global-mode +1))

;; custom stuff

(defun say-message ()
  "Say message on osx after time."
  (interactive)
  (let ((msg (read-from-minibuffer "say this message: "))
	(relative-time (read-from-minibuffer "when: ")))
    (run-at-time relative-time nil (lambda (mesg) (shell-command (concat "say -r 150 " mesg))) msg)))

;;; note: you have to make your own notepad for now
;;; C-x 5 2
;;; alt-shift-ret
;;; (set-frame-name "***NOTES***")
(defun me/toggle-notepad ()
  "Show the notes frame.  If you're on the notes frame, show the last frame you were on."
  (interactive)
  (if (equal (frame-parameter (selected-frame) 'name) "***NOTES***")
      (other-frame -1)
    (condition-case nil
	(select-frame-by-name "***NOTES***")
      (error (progn (make-frame `((name . "***NOTES***")))
		    (select-frame-by-name "***NOTES***")))))
  )

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

;; Toggle window dedication
(defun toggle-window-dedicated ()
  "Toggle whether the current active window is dedicated or not."
  (interactive)
  (message
   (if (let (window (get-buffer-window (current-buffer)))
	 (set-window-dedicated-p window
				 (not (window-dedicated-p window))))
       "Window '%s' is dedicated"
     "Window '%s' is normal")
   (current-buffer)))


(define-key 'spacer "b" 'magit-blob-previous)
(define-key 'spacer "d" 'toggle-window-dedicated)
(define-key 'spacer "f" 'magit-blob-next)
(define-key 'spacer "g" 'say-message)
(define-key 'spacer "t" 'toggle-window-split)
(define-key 'spacer "v" 'me/toggle-notepad)

(define-prefix-command 'ts-prefix)
(define-key 'spacer "l" 'ts-prefix)
(define-key 'ts-prefix "d" 'tide-jump-to-definition)
(define-key 'ts-prefix "r" 'tide-references)
(define-key 'ts-prefix "b" 'tide-find-previous-reference)
(define-key 'ts-prefix "f" 'tide-find-next-reference)
(define-key 'ts-prefix "h" 'tide-documentation-at-point)
(define-key 'ts-prefix "u" 'tide-jump-back)
(define-key 'ts-prefix "e" 'tide-project-errors)
(define-key 'ts-prefix "x" 'tide-restart-server)

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
    (magit-stash golden-ratio graphql-mode import-js company-lsp lsp-ui lsp-mode-ui lsp-mode image-dired+ diredp dired-p org-pomodoro evil-smartparens smartparens-javascript smart-parens smartparens nvm color-theme-buffer-local yasnippet cider exec-path-from-shell flycheck flymake-eslint git-gutter magit-gutter evil-lispy racket-mode auto-highlight-symbol highlight-symbol smooth-scrolling prettier-js prettier prettierjs ag counsel-projectile company-tern web-mode company tern which-key projectile ivy evil use-package)))
 '(safe-local-variable-values (quote ((standard-indent . 2))))
 '(split-height-threshold 95)
 '(split-window-preferred-function (quote split-window-sensibly))
 '(tide-disable-suggestions t)
 '(tide-server-max-response-length 999999)
 '(tide-tscompiler-executable "./node_modules/.bin/tsc")
 '(tide-tsserver-executable nil))
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 )
(provide 'init)
;;; init.el ends here
