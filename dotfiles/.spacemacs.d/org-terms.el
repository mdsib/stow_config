;; TODO: fontify region or page only if defs have changed.
(defvar org-term/def-regex "\\<\\(def\\*\\)\\([^\\*]+\\)\\(\\*\\)")
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
  (org-term/apply-terms)
  (font-lock-fontify-buffer))

(defun org-term/usage-re-builder (terms)
  "Build usage regex out of 1 or more terms."
  (let ((term-regex (if (listp terms)
                        (string-join (mapcar 'regexp-quote terms) "\\|")
                      (regexp-quote terms))))
    (concat "\\(^\\|[^[:alpha:]\\*]\\)\\*?\\<\\("
            term-regex
            "\\)\\>")))

(defun org-term/formatted-occur (str)
  "Apply our beautiful font lock stuff to the occur buffer."
  (interactive)
  (save-excursion
    (occur str)
    (switch-to-buffer "*Occur*")
    (org-term/configure-font-lock)))

(defun org-term/formatted-occur (str)
  (org-occur str))

(defun org-term/def-or-term-from-line (&optional point)
  "Gets definition name from line that the point or current point is in."
  (interactive)
  (save-excursion
    (save-match-data
      (beginning-of-line)
      (cond ((re-search-forward)))
      (if (re-search-forward org-term/defortermregex (line-end-position) t)
          (match-string 2)
        ""))))

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
  (org-term/formatted-occur org-term/def-regex))

;; TODO: rework with new representation for usage
(defun org-term/all-occurances ()
  "Shows def and all usage of a term"
  (interactive)
  (org-term/formatted-occur (concat "def\\*"
                                    (regexp-quote (org-term/def-or-term-from-line))
                                    "\\*")))

(defun org-term/change-usage-keywords (exps)
  (if (null exps)
      (org-term/unset-keywords)
    (let ((kword `(( ,(org-term/usage-re-builder exps)
                     (2 'org-term/usage-face)))))
      (if (not (equal org-term/last-keyword kword))
          (progn
            (org-term/unset-keywords)
            (font-lock-add-keywords nil kword)
            (setq org-term/last-keyword kword))))))

(defun org-term/unset-keywords ()
  (if (not (cl-equalp 'org-term/last-keyword nil))
      (progn
        (font-lock-remove-keywords nil org-term/last-keyword)
        (setq org-term/last-keyword nil))))

;throw away args from before change functions call
(defun org-term/apply-terms (&rest args)
  (interactive)
  (setq case-fold-search t)
  (setq new-defs '())
  (save-excursion
    (save-match-data
      ;; TODO: new-defs might not be doing the right thing, cuz last-keyword is always nil. investigate this shit. because of this, old definitions won't go away.
      (goto-char (point-min))
      (while (re-search-forward org-term/def-regex nil t)
        (add-to-list 'new-defs (match-string-no-properties 2)))
      (org-term/change-usage-keywords new-defs))))

(defun org-term/jank-activate-modeish-thing ()
  "A probably temporary way to get this org-term into mode hooks"
  (interactive)
  (org-term/configure-font-lock)
  (add-hook 'before-change-functions 'org-term/apply-terms))
