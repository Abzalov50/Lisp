;;; Codes from the book Let Over Lisp, by Doug Hoyte

;;; Utilities
(defun mkstr (&rest args)
  (with-output-to-string (s)
    (dolist (a args)
      (princ a s))))

(defun symb (&rest args)
  (values (intern (apply #'mkstr args))))

(defun group (n args)
  (when (zerop n) (error "Zero length"))
  (labels ((rec (lst args)
	     (cond ((>= n (length args)) (append lst (list args)))
		   (t (rec (append lst (list (subseq args 0 n)))
			   (subseq args n))))))
    (if args (rec nil args) nil)))

(defun flatten (tree)
  (cond ((null tree) nil)
	((atom tree) (list tree))
	(t (append (flatten (car tree))
		   (flatten (cdr tree))))))

;;; Closures
(defun register-allocated-fixnum ()
  (declare (optimize (speed 3) (safety 0)))
  (let ((acc 0))
    (loop for i from 1 to 100 do
	 (incf (the fixnum acc)
	       (the fixnum i)))
    acc))

(defun let-over-lambda-returner ()
  (let ((y 1))
    (lambda (x) (incf y x))))

(defun counter ()
  (let ((c 0))
    (lambda () (incf c))))

(defun person (name age salary)
  (let ((n name)
	(a age)
	(s salary))
    (lambda (aug)
      (format t "~&~A, ~A years old, has now a salary of ~A euros.~%"
	      n a (incf s aug)))))

(defun block-scanner (trigger-string)
  (let* ((trig (coerce trigger-string 'list))
	 (curr trig))
    (lambda (data-string)
      (let ((data (coerce data-string 'list)))
	(dolist (c data)
	  (if curr
	      (setq curr
		    (if (char= c (car curr))
			(cdr curr)  ;; check next char
			trig))))    ;; start over
	(not curr)))))

;;; Macros
(defmacro unit-of-time (value unit)
  `(* ,value
      ,(case unit
	 ((s) 1)
	 ((m) 60)
	 ((h) 3600)
	 ((d) 86400)
	 ((ms) 1/1000)
	 ((us) 1/1000000))))

(defmacro nlet (n letargs &rest body)
  `(labels ((,n ,(mapcar #'car letargs)
	      ,@body))
     (,n ,@(mapcar #'cadr letargs))))

(defun fact (n)
  (nlet nfact-rec ((n n) (res 1))
	(cond ((zerop n) res)
	      (t (nfact-rec (1- n) (* n res))))))
	
	      
(defun g!-symbol-p (s)
  (and (symbolp s)
       (> (length (symbol-name s)) 2)
       (string= (symbol-name s)
		"G!"
		:start1 0
		:end1 2)))

(defmacro defmacro/g! (name args &rest body)
  (let ((syms (remove-duplicates
	       (remove-if-not #'g!-symbol-p
			      (flatten body)))))
    `(defmacro ,name ,args
       (let ,(mapcar
	      (lambda (s)
		`(,s (gensym ,(subseq
			       (symbol-name s)
			       2))))
	      syms)
	 ,@body))))
