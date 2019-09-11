(defun generate-integers (start end step)
  "Generate a list of integers starting from `start', ending in `end'
stepping by `step'.
E.g., (generate-integers 0 10 1) => (0 1 2 3 4 5 6 7 8 9 10)
      (generate-integers 0 10 3) => (0 3 6 9)"
  (labels ((rec (lst-int start end step)
	     (if (> start end)
		 (nreverse lst-int)
		 (rec (push start lst-int) (incf start step) end step))))
    (rec nil start end step)))

(defun close-p (u v) (> (abs (- u v)) 0.0001))

(defun fixed-point (f i-guess)
  (labels ((try (g)
	     (let ((new-g (funcall f g)))
	       (if (close-p new-g g)
		   new-g
		   (try new-g)))))
    (try i-guess)))
