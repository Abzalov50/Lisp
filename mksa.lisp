#| Utility for manage units consistently in Physics caluclaions,
using the MKSA-System (Meter, Kilogram, Second and Ampere). 
Some examples of equivalence are:
(Pascal) 1 Pa = 1 kg / (m * s^2)
(Newton) 1 N = 1 (kg * m) / s^2
(Volt) 1 V = 1 (kg * m^2) / (A * s^3)

The general form of a numer with its unit is : Q * m^a * kg^b * s^c * A^d
Where Q, A, b, c and d are real numbers. For example:
230 Volt = 230 m^2 * kg^1 * s^(-3) * A^1

In Lisp, we can use a list as the datastructure to store the coefficients.
The form is (Q a b c d). So, the previous example can be written in Lisp as:
230 V = (230 2 1 -3 1).
|#
(defun *u (a b)
  (cons (* (car a) (car b))
	(mapcar #'+ (cdr a) (cdr b))))

(defun /u (a b)
  (cons (/ (car a) (car b))
	(mapcar #'- (cdr a) (cdr b))))

(defun +u (a b)
  (if (equal (cdr a) (cdr b))
      (cons (+ (car a) (car b)) (cdr a))
      (error "Arguments incompatibles pour l'addition")))

(defun -u (a b)
  (if (equal (cdr a) (cdr b))
      (cons (- (car a) (car b)) (cdr a))
      (error "Arguments incompatibles pour la soustraction")))

(defun exptu (x n)
  (cond ((zerop n) 1)
	((evenp n) (let ((exphalf (exptu x (/ n 2))))
		     (*u exphalf exphalf)))
	(t (*u x (exptu x (1- n))))))

;;                                        M   K   S   A
;;                                        -------------
(defun 1*     (x)    (list x              0   0   0   0))
(defun m      (x)    (list x              1   0   0   0))
(defun kg     (x)    (list x              0   1   0   0))
(defun s      (x)    (list x              0   0   1   0))
(defun a      (x)    (list x              0   0   0   1))
(defun m/s    (x)    (list x              1   0  -1   0))
(defun n      (x)    (list x              1   1  -2   0))
(defun v      (x)    (list x              2   1  -3  -1))

(defun j      (x)    (*u (n x) (m 1)))
(defun ohm    (x)    (/u (v x) (a 1)))

(defun km     (x)    (list (* 1000 x)     1   0   0   0))
(defun inch   (x)    (list (* x 0.0254)   1   0   0   0))
(defun minute (x)    (list (* x 60)       0   0   1   0))
(defun h      (x)    (list (* x 60 60)    0   0   1   0))
  
(defparameter *units*
  (list
   (cons 'm   (cdr (m    1)))
   (cons 'kg  (cdr (kg   1)))
   (cons 's   (cdr (s    1)))
   (cons 'a   (cdr (a    1)))
   (cons 'j   (cdr (j    1)))
   (cons 'm/s (cdr (m/s  1)))
   (cons 'v   (cdr (v    1)))
   (cons 'ohm (cdr (ohm  1)))
   (cons 'Nm  (cdr (*u (n 1) (m 1))))))

(defun find-unit (x)
  (car (find (cdr x) *units*
	     :test #'equal
	     :key #'cdr)))

(defun u (x)
  (let ((unit (find-unit x)))
    (if unit (list unit (car x)) (cons 'list x))))
