(defun position-to-kill-ring ()
  "Copy to the kill ring a string in the format \"file-name:line-number\"
for the current buffer's file name, and the line number at point."
  (interactive)
  (kill-new
   (format "%s::%d" (buffer-file-name) (save-restriction
                                        (widen) (line-number-at-pos)))))
;;; MY FUNCS
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

(require 'bookmark)
(defvar ms/pr-str "__PROJECT_ROOT__")
(defvar ms/dar-str "__DIR_AS_ROOT__")
(defun ms/ffib ()
  (interactive)
  (let ((loc (ivy-completing-read
               "Pick dir to fuzzy find from: "
               (append
                `(,ms/pr-str)
                (cl-remove-if-not
                 (lambda (item) (directory-name-p (bookmark-get-filename item)))
                 (bookmark-all-names)))))
        (prev-ffip-project-root (if (boundp 'ffip-project-root) ffip-project-root "")))
    (cond
     ((eq nil loc) (message "no project, sry"))
     ((equal loc ms/pr-str) (ffip))
     (t (setq ffip-project-root (bookmark-get-filename loc))
        (ffip)
        (setq ffip-project-root prev-ffip-project-root)))))

;;;;;;;;;;
;; and now we put this code to use

(global-set-key (kbd "C-x M-k") 'ms/ivy-kill-buffer-and-virtual)
(global-set-key (kbd "C-x M-f") 'ms/ffib)
(global-set-key (kbd "C-x f") 'ffip)
(global-set-key (kbd "C-x p r") (lambda () (interactive) (setq ffip-project-root nil)))
(global-set-key (kbd "C-x p s") (lambda () (interactive) (setq ffip-project-root (read-file-name "yes"))))
