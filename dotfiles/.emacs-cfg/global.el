;kind of global settings
(toggle-diredp-find-file-reuse-dir 1)
(setq linum-format "%-2d ")

;keybindings
(global-set-key (kbd "C-x g") 'magit-status)
(global-set-key (kbd "C-x C-g") 'magit-dispatch-popup)
(global-set-key (kbd "C-x C-b") 'ibuffer)

(define-prefix-command 'my-magit)
(global-set-key (kbd "C-c g") 'my-magit)
(global-set-key (kbd "C-c g n") 'magit-blob-next)
(global-set-key (kbd "C-c g p") 'magit-blob-previous)
(global-set-key (kbd "C-c g i") 'magit-init)

;exposing org to the world
(global-set-key (kbd "C-x c") 'org-capture)
(global-set-key (kbd "C-x a") 'org-agenda)
(global-set-key (kbd "C-c j") 'org-journal-new-entry)

;etc
(global-set-key (kbd "C-c f") 'find-grep-dired)

(define-prefix-command 'my-erc)
(global-set-key (kbd "C-c e") 'my-erc)
(define-prefix-command 'my-znc)
(global-set-key (kbd "C-c e z") 'my-znc)
(global-set-key (kbd "C-c e z l") (lambda () (interactive)
                                    (erc-tls :server "luv2serve-local"
                                             :port "5000"
                                             :nick "mduggie"
                                             :password (read-passwd "znc passphrase: "))))

(global-set-key (kbd "C-c e z r") (lambda () (interactive)
                                    (erc-tls :server "luv2serve-remote"
                                             :port "5000"
                                             :nick "mduggie"
                                             :password (read-passwd "znc passphrase: "))))

(setq erc-lurker-hide-list '("JOIN" "PART" "QUIT"))
(setq erc-lurker-threshold-time 3600)

;emacs functionality settings
;(setq split-height-threshold nil)
;(setq split-width-threshold 0)
(setq whitespace-action '(auto-cleanup))

(define-key minibuffer-local-map [f3]
  (lambda () (interactive)
     (insert (buffer-name (window-buffer (minibuffer-selected-window))))))

(setq browse-url-browser-function 'browse-url-generic
      browse-url-generic-program "xdg-open")
