(defun magit-status-mode-funs ()
  (local-set-key (kbd "M-n")
                 (lambda() (interactive)
                   (magit-section-forward)
                   (recenter-top-bottom 0)))
  (local-set-key (kbd "M-p")
                 (lambda() (interactive)
                   (magit-section-backward)
                   (recenter-top-bottom 0))))

(add-to-list 'magit-status-mode-hook 'magit-status-mode-funs)
