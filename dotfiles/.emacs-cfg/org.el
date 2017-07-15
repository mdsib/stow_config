;;; org, my lovely love

;refiling
(setq org-default-notes-file "~/Dropbox/org/refile.org")
(setq org-refile-targets '((nil :maxlevel . 9)
                          (org-agenda-files :maxlevel . 9)))
;capturing
(setq org-capture-templates
      '(("T" "todo at point" entry (file+headline "" "Todo")
         "* TODO %?\n  %u\n  %a")
        ("t"
          "todo"
          entry
          (file+headline org-default-notes-file "Todo")
          "* TODO %?")))
(add-hook 'org-capture-mode-hook 'evil-insert-state)

;journal
(setq org-journal-file-format "%Y-%m-%d.org")

;agenda
(setq org-stuck-projects
      '("TODO={.+}/-DONE" "DONE" nil "SCHEDULED:\\|DEADLINE:"))
(setq org-agenda-use-tag-inheritance '(todo search timeline agenda))
(setq org-agenda-files '("~/Dropbox/org/" "~/Documents/journal"))
(setq org-agenda-custom-commands
      '(("z" "today"
         ((agenda "" ((org-agenda-ndays 7)))
          (tags-todo "today")
          (tags-todo "daily")))))

;(setq org-src-fontify-natively t) ;this seems to not be right, from docs

;code blocks

; https://github.com/ternjs/tern/issues/701
; how to correctly enable flycheck in babel source blocks
(defadvice org-edit-src-code (around set-buffer-file-name activate compile)
  (let ((file-name (buffer-file-name)))
    ad-do-it
    (setq buffer-file-name file-name)))

;da hook
(add-hook 'org-mode-hook
          (lambda ()
            ;minor modes
            (org-bullets-mode t)
            (org-indent-mode t)
            (org-term/jank-activate-modeish-thing)
            ;local bindings
            (local-set-key (kbd "C-x C-,") 'org-timestamp-down-day)
            (local-set-key (kbd "C-x C-.") 'org-timestamp-up-day)
            (local-set-key (kbd "C-c t") 'org-toggle-heading)
            (local-set-key (kbd "C-c p") 'org-pomodoro)
            ;code blocks
            (add-to-list 'org-src-lang-modes '("js" . js2-jsx))))
