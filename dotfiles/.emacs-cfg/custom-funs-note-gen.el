;; This buffer is for text that is not saved, and for Lisp evaluation.
;; To create a file, visit it with <open> and enter text in its buffer.

;generate a note sequence of 3 from 7 
(defun thing (notes length)
  (print-3 (randomize-list notes)))

(defun randomize-list (lst)
  (if (null lst)
      lst
    (let ((r-el (nth (random (length lst)) lst)))
      (cons r-el (randomize-list (remove r-el lst))))))

(defun print-3 (lst)
  (print (list (car lst) (cadr lst) (cl-caddr lst))))

(defvar pattern '(a b c d e f g))
(setq pattern '(1 2 3 4 5 6 7))
(defun fun-time ()
  (let ((fun-timer nil))
    (lambda (op)
      (cond ((eq op 'start)
             (setq fun-timer
                  (run-with-timer 0 5 (lambda () (thing pattern 3)))))
            ((eq op 'stop)
             (cancel-timer fun-timer))))))

(defvar inst (fun-time))
(apply inst '(start))
(apply inst '(stop))
