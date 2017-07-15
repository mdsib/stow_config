(setq package-archives '(("gnu" . "https://elpa.gnu.org/packages/")
                         ("marmalade" . "https://marmalade-repo.org/packages/")
                         ("melpa" . "https://melpa.org/packages/")))

; fetch the list of packages available
(unless package-archive-contents
  (package-refresh-contents))

; Install the missing packages
(dolist (package package-selected-packages)
  (unless (package-installed-p package)
    (package-install package)))

;activation of some hardly-configured packages
;other activations are in their cfg files
;TODO: maybe I could activate all here, and check if the mode is active before loading the other code on boot. could speed things up
(which-key-mode t)
(setq which-key-sort-order nil)

(desktop-save-mode t)
(global-company-mode t)
(show-paren-mode t)
(electric-pair-mode t)
(linum-mode t)
