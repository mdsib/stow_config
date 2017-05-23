(defvar org-term/def-regex "\\<\\(def\\*\\)\\([^\\*]+\\)\\(\\*\\)")
(defvar org-term/def-list '())
(defvar org-term/last-keyword '())

(defface org-term/definition-face '((t . (:background "#888" :foreground "black" :bold t)))
  "Face for the definitions of terminology.")
(defface org-term/usage-face '((t . (:background "#666" :foreground "#333" :bold t)))
  "Face for the usage of terminology.")

(defun org-term/configure-font-lock ()
  "Sets up the font lock, dude. What else my main man?"
  (font-lock-add-keywords
   nil
   `((,org-term/def-regex (1 '(face org-term/definition-face invisible t))
                          (2 '(face org-term/definition-face))
                          (3 '(face org-term/definition-face invisible t)))))
  (setq font-lock-extra-managed-props '(invisible))
  (font-lock-fontify-buffer))

(defun org-term/formatted-occur (str)
  "Apply our beautiful font lock stuff to the occur buffer."
  (interactive)
  (save-excursion
    (occur str)
    (switch-to-buffer "*Occur*")
    (org-term/configure-font-lock)))

;; TODO: rework with new representation for usage
(defun org-term/def-or-term-from-line (&optional point)
  "Gets definition namie from line that the point or current point is in."
  (interactive)
  (save-excursion
    (beginning-of-line)
    (if (re-search-forward org-term/defortermregex (line-end-position) t)
        (match-string 2)
      "")))

;; TODO: rework with new representation for usage
(defun org-term/usage-occurances ()
  "Shows all usage of a term."
  (interactive)
  (org-term/formatted-occur (concat "term\\*"
                 (regexp-quote (org-term/def-or-term-from-line))
                 "\\*")))

(defun org-term/show-defs ()
  "Shows all defs in document"
  (interactive)
  (org-term/formatted-occur org-term/defregex))

;; TODO: rework with new representation for usage
(defun org-term/all-occurances ()
  "Shows def and all usage of a term"
  (interactive)
  (org-term/formatted-occur (concat "\\(term\\|def\\)\\*"
                 (regexp-quote (org-term/def-or-term-from-line))
                 "\\*")))

(defun org-term/jank-activate-modeish-thing ()
  "A probably temporary way to get this org-term into mode hooks"
  (org-term/configure-font-lock)
  (add-hook 'post-command-hook 'org-term/apply-terms))

(defun org-term/change-usage-keywords (exps)
  (let ((kword `((,(concat "\\(^\\|[[:blank:]]\\)\\*?"
                           "\\<\\("
                           (string-join (mapcar 'regexp-quote exps) "\\|")
                           "\\)\\>")
                  (2 'org-term/usage-face)))))
    (font-lock-add-keywords nil kword)
    (if (not (equalp org-term/last-keyword kword))
        (org-term/unset-keywords))
    (setq org-term/last-keyword kword)))

(defun org-term/unset-keywords ()
  (if (not (equalp 'org-term/last-keyword nil))
      (progn
        (font-lock-remove-keywords nil org-term/last-keyword)
        (setq org-term/last-keyword nil))))

(defun org-term/apply-terms ()
  (interactive)
  (let ((new-defs '()))
    (save-excursion
      (beginning-of-buffer)
      (while (re-search-forward org-term/def-regex nil t)
        (add-to-list 'new-defs (match-string-no-properties 2))))
    (if (null new-defs)
        (org-term/unset-keywords)
      (org-term/change-usage-keywords new-defs)))
  (font-lock-fontify-buffer))
