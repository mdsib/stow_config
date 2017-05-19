(defvar org-term/def-regex "\\<\\(def\\*\\)\\([^\\*]+\\)\\(\\*\\)")
(defvar org-term/term-regex "\\<\\(term\\*\\)\\([^\\*]+\\)\\(\\*\\)")
(defvar org-term/def-or-term-regex "\\<\\(term\\*\\|def\\*\\)\\([^\\*]+\\)\\(\\*\\)")
(defface org-term-definition-face '((t . (:background "#888" :foreground "black" :bold t))))
(defface org-term-usage-face '((t . (:background "#666" :foreground "#333" :bold t)))
  "Face for the usage of terminology.")
(defun org-term/configure-font-lock ()
  "Sets up the font lock, dude. What else my main man?"
  (font-lock-add-keywords
   nil
   `((,org-term/def-regex (1 '(face nil invisible t)))
     (,org-term/def-regex (2 '(face org-term-definition-face)))
     (,org-term/def-regex (3 '(face nil invisible t)))
     (,org-term/term-regex (1 '(face nil invisible t)))
     (,org-term/term-regex (2 '(face org-term-usage-face)))
     (,org-term/term-regex (3 '(face nil invisible t)))))
  (setq font-lock-extra-managed-props '(invisible))
  (font-lock-fontify-buffer))

(defun org-term/formatted-occur (str)
  "Apply our beautiful font lock stuff to occur."
  (interactive)
  (save-excursion
    (occur str)
    (switch-to-buffer "*Occur*")
    (org-term/configure-font-lock)))

(defun org-term/def-or-term-from-line (&optional point)
  "Gets definition name from line that the point or current point is in."
  (interactive)
  (save-excursion
    (beginning-of-line)
    (if (re-search-forward org-term/defortermregex (line-end-position) t)
        (match-string 2)
      "")))

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

(defun org-term/all-occurances ()
  "Shows def and all usage of a term"
  (interactive)
  (org-term/formatted-occur (concat "\\(term\\|def\\)\\*"
                 (regexp-quote (org-term/def-or-term-from-line))
                 "\\*")))

(defun org-term/jank-activate-modeish-thing ()
  "A probably temporary way to get this org-term into mode hooks"
  (org-term/configure-font-lock)
  (local-set-key (kbd "C-c C-M-c") 'org-term/all-occurances)
  (local-set-key (kbd "C-c C-M-v") 'org-term/usage-occurances)
  (local-set-key (kbd "C-c C-M-t") 'org-term/show-defs))
