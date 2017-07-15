(setq lexical-binding t)
;;some weird scheme buffer stuff that doesn't work most of the time
;(defun ms/scheme-in-other ()
;  (interactive)
;  (delete-other-windows)
;  (split-window-right)
;  (other-window 1)
;  (run-scheme nil)
;  (other-window 1))
;(defun ms/run-scheme-def ()
;  (interactive)
;  (ms/scheme-in-other)
;  (scheme-send-definition))
;(defun ms/run-scheme-buffer ()
;  (interactive)
;  (ms/scheme-in-other)
;  (save-excursion
;    (scheme-send-region 1 (point-max))))
;(add-hook 'scheme-mode-hook (lambda ()
;                              (message "enter scheme mode")
;                              (local-set-key (kbd "C-M-x") 'ms/run-scheme-def)
;                              (local-set-key (kbd "C-M-r") 'ms/run-scheme-buffer)))

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
  '(org-mode-hook))
(add-hook-to-many-modes 'visual-line-mode textlike-hooks)

;;;;;;;;;;;;



