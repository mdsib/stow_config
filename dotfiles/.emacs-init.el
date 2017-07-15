(load-file "~/.emacs-cfg/packages.el")
(load-file "~/.emacs-cfg/global.el")
(load-file "~/.emacs-cfg/theme.el")

;package-specifics
(load-file "~/.emacs-cfg/org.el")
(load-file "~/.emacs-cfg/evil.el")
(load-file "~/.emacs-cfg/js.el")
(load-file "~/.emacs-cfg/counsel-ivy.el")
(load-file "~/.emacs-cfg/lisplike.el")

;my code
(load-file "~/.emacs-cfg/custom-funs.el")
(load-file "~/.emacs-cfg/newest-file-in-dir.el")
(load-file "~/.emacs-cfg/org-terms.el")

;(load-file "~/.emacs-cfg/playground.el")

;maybe this should go somewhere
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
