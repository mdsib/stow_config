;kind of global settings
(toggle-diredp-find-file-reuse-dir 1)
(setq linum-format "%-2d ")

;keybindings
(global-set-key (kbd "C-x g") 'magit-status)
(global-set-key (kbd "C-x C-g") 'magit-dispatch-popup)
(global-set-key (kbd "C-x C-b") 'ibuffer)

(define-prefix-command 'ol-space)
;spc = @ in iterm vs standalone
(global-set-key (kbd "C-SPC") 'ol-space)
(global-set-key (kbd "C-@") 'ol-space)
(global-set-key (kbd "C-@ C-@") 'set-mark-command)
(global-set-key (kbd "C-@ C-SPC") 'set-mark-command)

;exposing org to the world
(global-set-key (kbd "C-x c") 'org-capture)
(global-set-key (kbd "C-x a") 'org-agenda)

;emacs functionality settings
(setq split-height-threshold nil)
(setq split-width-threshold 0)
(setq whitespace-action '(auto-cleanup))

(define-key minibuffer-local-map [f3]
  (lambda () (interactive) 
     (insert (buffer-name (window-buffer (minibuffer-selected-window))))))

(setq browse-url-browser-function 'browse-url-generic
      browse-url-generic-program "firefox-developer")

;; Save all tempfiles in $TMPDIR/emacs$UID/
;; ty to https://www.emacswiki.org/emacs/AutoSave

(defconst emacs-tmp-dir (expand-file-name (format "emacs-%d" (user-uid))
                                          temporary-file-directory))
(if (not (file-exists-p emacs-tmp-dir))
    (progn
      (make-directory emacs-tmp-dir t)
      (set-file-modes emacs-tmp-dir (string-to-number "0755" 8))))
(setq backup-directory-aist
      `((".*" . ,emacs-tmp-dir)))
(setq auto-save-file-name-transforms
      `((".*" ,emacs-tmp-dir t)))
(setq auto-save-list-file-prefix
      emacs-tmp-dir)

