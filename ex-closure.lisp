(defun person-1 (first-name last-name age)
  (let ((first-name first-name)
	(last-name last-name)
	(age age))
    (list
     ;; getters
     #'(lambda () first-name)
     #'(lambda () last-name)
     #'(lambda () age)
     ;; setters
     #'(lambda (new-first-name) (setf first-name new-first-name))
     #'(lambda (new-last-name) (setf last-name new-last-name))
     #'(lambda (new-age) (setf age new-age))
     ;; printers
     #'(lambda () (format t "~&~A ~A, ~A ans" first-name last-name age)))))

(defun print-person (p)
  (funcall (elt p 6)))

(defun set-first-name (p new-value)
  (funcall (elt p 3) new-value))

(defsetf first-name (p) (new-value)
  `(funcall (elt ,p 3) ,new-value))

(defun first-name (p)
  (funcall (elt p 0)))
