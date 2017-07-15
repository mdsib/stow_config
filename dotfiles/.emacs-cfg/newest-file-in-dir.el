;thing I made that inserts newest filename from a directory
(defvar ms/key (kbd "C-c C-S-l"))
(defvar ms/dir "~/Downloads")

(defun ms/filter-dirs (filelist)
  (remove-if 'file-directory-p filelist))
(defun ms/newest-file (dir)
  (reduce (lambda (accum file)
            (if (file-newer-than-file-p file accum)
                file
              accum))
          (ms-filter-dirs (directory-files dir t))))

;general case
(defun ms/newest-file-in-buffer ()
  (interactive)
  (insert (ms/newest-file ms/dir)))
(global-set-key ms/key 'ms/newest-file-in-buffer)

;Org specific
(defun ms/insert-newest-file-org ()
      (interactive)
      (org-insert-link :link-location (concat "file:"
                                              (ms/newest-file ms/dir))))
(defun ms/newest-file-org-hook ()
  (local-set-key ms/key
                 ms/insert-newest-file))

(add-to-list 'org-mode-hook 'ms-org/newest-file-org-hook)
