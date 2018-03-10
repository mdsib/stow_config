(load-file "~/.emacs-cfg/packages.el")
(load-file "~/.emacs-cfg/global.el")
(load-file "~/.emacs-cfg/theme.el")

;package-specifics
(load-file "~/.emacs-cfg/evil.el")
(load-file "~/.emacs-cfg/js.el")
(load-file "~/.emacs-cfg/counsel-ivy.el")
(load-file "~/.emacs-cfg/lisplike.el")
(require 'magit)
(load-file "~/.emacs-cfg/magit.el")


;my code
(load-file "~/.emacs-cfg/custom-funs.el")
(load-file "~/.emacs-cfg/newest-file-in-dir.el")
(load-file "~/.emacs-cfg/org-terms.el")

(load-file "~/.emacs-cfg/org.el")

;(load-file "~/.emacs-cfg/playground.el")

;maybe this shouldCustom go somewhere
(defun add-hook-to-many-modes (fun hooks)
  (mapc (lambda (hook)
          (add-hook hook fun))
        hooks))

(defvar lisplike-hooks
  '(lisp-mode-hook
    scheme-mode-hook
    elisp-mode-hook))
(add-hook-to-many-modes 'evil-lispy-mode lisplike-hooks)

(defvar textlike-hooks
  '(org-mode-hook
    json-mode-hook))
(add-hook-to-many-modes 'visual-line-mode textlike-hooks)

(add-to-list 'auto-mode-alist '("\\.html?\\'" . web-mode))

(setq backup-directory-alist '(("." . "~/.emacs.d/backup"))
  backup-by-copying t    ; Don't delink hardlinks
  version-control t      ; Use version numbers on backups
  delete-old-versions t  ; Automatically delete excess backups
  kept-new-versions 3    ; how many of the newest versions to keep
  kept-old-versions 2    ; and how many of the old
  )

; yasnippet

(yas-global-mode)
(setq yas-snippet-dirs '("/home/mds/.emacs-cfg/snippets"))
(global-set-key (kbd "C-@ n") 'yas-new-snippet)
(global-set-key (kbd "C-@ s") 'yas-insert-snippet)
(global-set-key (kbd "C-@ v") 'yas-visit-snippet-file)

(defun rgb-to-hex (r g b)
  (format "#%02X%02X%02X" r g b))

(defun hex-to-rgb (hexcode)
  (format "rgb(%d, %d, %d)"
          (string-to-int (substring hexcode 1 3) 16)
          (string-to-int (substring hexcode 3 5) 16)
          (string-to-int (substring hexcode 5 7) 16)))

(add-hook 'before-save-hook 'delete-trailing-whitespace)

(defun ms/ivy-switch-buffer-with-ignore (regex)
  (interactive)
  (let ((old-ignore-list ivy-ignore-buffers))
    (progn
      (setq ivy-ignore-buffers `(,regex))
      (ivy-switch-buffer)
      (setq ivy-ignore-buffers old-ignore-list))))

(global-set-key
 (kbd "C-c e b")
 (lambda ()
   (interactive)
   (ms/ivy-switch-buffer-with-ignore "^[^#]")))

(global-set-key
 (kbd "C-x b")
 (lambda ()
   (interactive)
   (ms/ivy-switch-buffer-with-ignore "#")))
