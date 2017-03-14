(setq package-archives '(("gnu" . "https://elpa.gnu.org/packages/")
                         ("marmalade" . "https://marmalade-repo.org/packages/")
                         ("melpa" . "https://melpa.org/packages/")))

;; git repos
(add-to-list 'load-path "~/.emacs.d/git/mk-project/")
(require 'mk-project)

(project-def "org" '((basedir "~/org")))
(project-def "code" '((basedir "~/code")))

;; Activating stuff
(which-key-mode 1)
(ivy-mode 1)
(evil-mode 1)
(desktop-save-mode 1)
(global-company-mode 1)
(show-paren-mode t)
(electric-pair-mode)

;; setting stuff
(setq linum-format "%-2d ")

;; evil stuff
(define-key
  evil-normal-state-map
  (kbd "Y")
  'evil-yank-line)

;; global bindings for sanity
(define-prefix-command 'c-dub)
(define-prefix-command 'ol-space)

(global-set-key (kbd "C-w") 'c-dub)

(global-set-key (kbd "C-w h") 'evil-window-left)
(global-set-key (kbd "C-w j") 'evil-window-down)
(global-set-key (kbd "C-w k") 'evil-window-up)
(global-set-key (kbd "C-w l") 'evil-window-right)
(global-set-key (kbd "C-w s") 'evil-window-split)
(global-set-key (kbd "C-w v") 'evil-window-vsplit)
(global-set-key (kbd "C-w q") 'evil-window-delete)

(global-set-key (kbd "C-@") 'ol-space)
(global-set-key (kbd "C-@ C-@") 'set-mark-command)

(global-set-key (kbd "C-x c") 'org-capture)
(setq org-default-notes-file "~/org/agenda/todo.org")
(global-set-key (kbd "C-x a") 'org-agenda)
(setq org-capture-templates
      '(("T" "todo at point" entry (file+headline "" "Todo")
         "* TODO %?\n  %u\n  %a")
        ("t"
          "todo"
          entry
          (file+headline org-default-notes-file "Todo")
          "* TODO %?")))

(add-hook 'org-capture-mode-hook 'evil-insert-state)


;; paredit
(add-hook 'elisp-mode-hook 'smartparens-mode)
(add-hook
 'smartparens-mode-hook
 (lambda ()
   (sp-local-pair 'emacs-lisp-mode "'" nil :actions nil)
   (local-set-key (kbd "C-@ C-d") 'sp-kill-sexp)
   (local-set-key (kbd "C-@ C-h") 'sp-previous-sexp)
   (local-set-key (kbd "C-@ C-k") 'sp-backward-up-sexp)
   (local-set-key (kbd "C-@ C-j") 'sp-down-sexp)
   (local-set-key (kbd "C-@ C-l") 'sp-next-sexp)
   (local-set-key (kbd "C-@ C-n") 'sp-beginning-of-next-sexp)
   (local-set-key (kbd "C-@ C-e") 'sp-end-of-sexp)
   (local-set-key (kbd "C-@ C-a") 'sp-beginning-of-sexp)
   (local-set-key (kbd "C-@ C-y") 'sp-copy-sexp)
   (local-set-key (kbd "C-@ l") 'sp-forward-slurp-sexp)
   (local-set-key (kbd "C-@ h") 'sp-backward-slurp-sexp)
   (local-set-key (kbd "C-@ M-l") 'sp-forward-barf-sexp)
   (local-set-key (kbd "C-@ M-h") 'sp-backward-barf-sexp)
   (local-set-key (kbd "C-@ C-p") 'sp-beginning-of-previous-sexp)))

;; parinfer
(setq parinfer-extensions '(defaults smart-tab pretty-parens evil smart-yank))

;; org, my love
(setq org-agenda-use-tag-inheritance '(todo search timeline agenda))
(setq org-journal-file-format "%Y-%m-%d.org")
(setq org-src-fontify-natively t)
(add-hook 'org-mode-hook 'org-bullets-mode)
(add-hook 'org-mode-hook
          (lambda ()
            (org-indent-mode)
            (add-to-list 'org-src-lang-modes '("js" . js2-jsx))
            (local-set-key (kbd "C-c t") 'org-toggle-heading)
            (local-set-key (kbd "C-c p") 'org-pomodoro)
            (setq org-agenda-files '("~/org" "~/Documents/journal"))))


;; https://github.com/ternjs/tern/issues/701
;; how to correctly enable flycheck in babel source blocks
(defadvice org-edit-src-code (around set-buffer-file-name activate compile)
  (let ((file-name (buffer-file-name)))
    ad-do-it
    (setq buffer-file-name file-name)))

;; which-key https://github.com/justbur/emacs-which-key
(setq which-key-sort-order nil)

;; MY FUNCS
;;;;;;;;;;;;;;;;;;

(defun ms/kill-virtual-buffer ()
  "Deletes an element from ivy's virtual buffer list"
  (interactive)
  (delete (assoc (completing-read
                   "VBuff to destroy: "
                   (mapcar 'car ivy--virtual-buffers))
                  ivy--virtual-buffers)
          ivy--virtual-buffers))

(defun ms/ivy-kill-buffer-and-virtual ()
  (interactive)
  (let* ((buffer (completing-read
                "Buffer to kill: "
                (append (mapcar 'car ivy--virtual-buffers)
                        (remove nil (mapcar 'buffer-name (buffer-list))))))
        (vbuff (assoc buffer ivy--virtual-buffers)))

    (if (get-buffer buffer)
        (kill-buffer buffer))
    (if vbuff
        (delete vbuff ivy--virtual-buffers))))

(defvar ms/pr-str "__PROJECT_ROOT__")
(defvar ms/dar-str "__DIR_AS_ROOT__")
(defun ms/ffib ()
  (interactive)
  (let ((loc (ivy-completing-read
               "Pick dir to fuzzy find from: "
               (append
                `(,ms/pr-str)
                (remove-if-not
                 (lambda (item) (directory-name-p (bookmark-get-filename item)))
                 (bookmark-all-names)))))
        (prev-ffip-project-root ffip-project-root))
    (cond
     ((eq nil loc) (message "no project, sry"))
     ((equal loc ms/pr-str) (ffip))
     (t (setq ffip-project-root (bookmark-get-filename loc))
        (ffip)
        (setq ffip-project-root prev-ffip-project-root)))))

;;;;;;;;;;
;; and now we put this code to use

(setq ivy-use-virtual-buffers t)
(global-set-key (kbd "C-x M-k") 'ms/ivy-kill-buffer-and-virtual)
(global-set-key (kbd "C-x b") 'ivy-switch-buffer)
(global-set-key (kbd "C-x M-f") 'ms/ffib)
(global-set-key (kbd "C-x f") 'ffip)
(global-set-key (kbd "C-x p r") (lambda () (interactive) (setq ffip-project-root nil)))
(global-set-key (kbd "C-x p s") (lambda () (interactive) (setq ffip-project-root (read-file-name "yes"))))



;; shamelessly stolen from ivy's example config: https://github.com/abo-abo/swiper
;; wow how ugly
(setq counsel-find-file-ignore-regexp "\\(^[\\.\\#]\\)\\|\\(~$\\)")
(global-set-key "\C-s" 'swiper)
(global-set-key (kbd "C-c C-r") 'ivy-resume)
(global-set-key (kbd "<f6>") 'ivy-resume)
(global-set-key (kbd "C-x C-f") 'counsel-find-file)
(global-set-key (kbd "M-x") 'counsel-M-x)
(global-set-key (kbd "<f1> f") 'counsel-describe-function)
(global-set-key (kbd "<f1> v") 'counsel-describe-variable)
(global-set-key (kbd "<f1> l") 'counsel-find-library)
(global-set-key (kbd "<f2> i") 'counsel-info-lookup-symbol)
(global-set-key (kbd "<f2> u") 'counsel-unicode-char)
(global-set-key (kbd "C-c g") 'counsel-git)
(global-set-key (kbd "C-c j") 'counsel-git-grep)
(global-set-key (kbd "C-c k") 'counsel-ag)
(global-set-key (kbd "C-x l") 'counsel-locate)
(global-set-key (kbd "C-S-o") 'counsel-rhythmbox)
(define-key read-expression-map (kbd "C-r") 'counsel-expression-history)

;; http://oremacs.com/swiper/#ivy--regex-fuzzy
;; (setq ivy-re-builders-alist
;;       '((t . ivy--regex-fuzzy)))

;; from https://github.com/milkypostman/powerline
(powerline-default-theme)

;; https://melpa.org/#/js2-mode told me to do this
(add-to-list 'auto-mode-alist '("\\.jsx?\\'" . js2-jsx-mode))
(add-to-list 'interpreter-mode-alist '("node" . js2-jsx-mode))


;; from http://company-mode.github.io/

;; from https://github.com/iquiw/company-ghc/issues/12
(with-eval-after-load 'company
  (add-to-list 'company-backends 'company-tern))

(add-hook 'js2-jsx-mode-hook (lambda ()
                               (tern-mode)
                               (js2-refactor-mode)
                               (linum-mode)))
(add-hook 'js2-jsx-mode-hook #'tern-mode)
(add-hook 'js2-jsx-mode-hook #'js2-refactor-mode)
(js2r-add-keybindings-with-prefix "C-@")

(add-hook 'neotree-mode-hook
          (lambda ()
            (define-key evil-normal-state-local-map (kbd "TAB") 'neotree-enter)
            (define-key evil-normal-state-local-map (kbd "SPC") 'neotree-enter)
            (define-key evil-normal-state-local-map (kbd "q") 'neotree-hide)
            (define-key evil-normal-state-local-map (kbd "RET") 'neotree-enter)))

(color-theme-initialize)
(color-theme-charcoal-black)

(require 'airline-themes)
(load-theme 'airline-cool)

