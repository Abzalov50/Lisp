;;; Operations on rational numbers
(defun make-rat (n d)
  "Construct a rational number from the given numerator
and denominator."
  (let* ((g (gcd n d))
	 (n (/ n g))
	 (d (/ d g)))
    (cons (if (plusp (* n d)) (abs n) (- (abs n)))
	  (abs d))))

(defun numer (x)
  "Return the numerator of the rational number `x'."
  (first x))

(defun denom (x)
  "Return the denominator of the rational number `x'."
  (rest x))

(defun print-rat (x)
  "Print the given rational number."
  (format t "~&~A/~A~%" (numer x) (denom x)))

(defun add-rat (x y)
  "Add the given rationals."
  (make-rat (+ (* (numer x) (denom y)) (* (denom x) (numer y)))
	    (* (denom x) (denom y))))

(defun neg-rat (x)
  "Return the negation of the given rational number."
  (make-rat (- (numer x)) (denom x)))

(defun sub-rat (x y)
  "Substract the given rationals."
  (add-rat x (neg-rat y)))

(defun inv-rat (x)
  "Return the inverse of the given rational number."
  (make-rat (denom x) (numer x)))

(defun mul-rat (x y)
  "Return the product of the given rationals."
  (make-rat (* (numer x) (numer y)) (* (denom x) (denom y))))

(defun div-rat (x y)
  "Divide `x' by `y'."
  (mul-rat x (inv-rat y)))

(defun equal-rat-p (x y)
  "Is the given rationals equal?"
  (= (* (numer x) (denom y))
     (* (denom x) (numer y))))
