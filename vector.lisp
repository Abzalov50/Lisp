;;; A library on vector algebra.
;;; Common Lisp (CL) offers a type `vector' with a predicate `vectorp'
;;; to test if an object is a vector. A CL vector is
;;; a `sequence' of data of arbitrary types.
;;; The following are valid CL vectors:
;;; #(1 2 3 -1 0)
;;; #("arnold" -2 'xcopy)

(defun true (arg)
  (if arg
      t
      nil))

(defun vector= (v1 v2)
  "Return T if the given vectors are equal, otherwise return NIL."
  (every #'true (map 'vector #'equal v1 v2)))

(defun vector-compare (pred &rest args)
  "Apply PREDICATE to every two consecutive vectors of the sequences. 
Return NIL as soon as any invocation of PREDICATE returns NIL, or T if every invocation
is non-NIL.
Two local functions are defined. The first one applies PREDICATE to `only' two vectors.
The second one apply the former recursively to two consecutives vectors."
  (labels ((vec-comp (pred v1 v2)
	     (if (not (equal (length v1) (length v2)))
		 (error "Vectors must have the same size.")
		 (block mapvec
		   (map 'vector #'(lambda (elt1 elt2)
					;(format t "~A ~A~%" elt1 elt2)
				    (when (not (funcall pred elt1 elt2))
				      (return-from mapvec nil)))
			v1 v2)
		   t)))
	   (comp-rec (new-args)
	     (cond ((or (= (length new-args) 1) (null new-args)) t)
		   ((not (funcall #'vec-comp
				  pred (car new-args) (cadr new-args)))
		    nil)
		   (t (comp-rec (cdr new-args))))))
    (comp-rec args)))

(defun vector-neg (v)
  "Return the opposite vector, in the numerical sense. That is -v"
  (map 'vector #'- v))

(defun identity-add (size)
  "Return a vector/matrix which is the identity of the vector addition operation.
It is a vector of length `size' filled with 0's."
  (make-array size))

(defun vector-identity-add-p (v)
  "Return T if the vector `v' is an addition identity. Otherwise return NIL."
  (vector= v (identity-add (length v))))

(defun vector-null (v)
  "Return T if all the components of the given vector are 0, otherwise return NIL."
  (vector-identity-add-p v))

(defun identity-mult (size)
  "Return a vector/matrix which is the identity of the vector multiplication operation.
It is a vector of length `size' filled with 1's."
  (make-array size :initial-element 1))

(defun vector-identity-mult-p (v)
  "Return T if the vector `v' is a multiplication identity. Otherwise return NIL."
  (vector= v (identity-mult (length v))))

(defun vector+ (&rest vectors)
  "Return the sum of the vectors given as arguments."
  (labels ((vector-binary-add (v1 v2)
	     ;; Return the sum of the two given vectors
	     (cond ((not (= (length v1) (length v2)))
		    (error "Vectors must have the same size."))
		   ((vector-identity-add-p v1) v2)
		   ((vector-identity-add-p v2) v1)
		   (t (map 'vector #'+ v1 v2)))))
    (if (null vectors)
	0
	(reduce #'vector-binary-add vectors))))

(defun vector- (&rest vectors)
  "Return the substraction of the vectors given as arguments."
  (labels ((binary-sub (v1 v2)
	     ;; Return the difference of the two given vectors
	     (cond ((not (= (length v1) (length v2)))
		    (error "Vectors must have the same size."))
		   ((vector-identity-add-p v1) (vector-neg v2))
		   ((vector-identity-add-p v2) v1)
		   (t (map 'vector #'- v1 v2)))))
    (if (null vectors)
	0
	(reduce #'binary-sub vectors))))

(defun vector-scalar-mult (a v)
  "Multiply the vector `v' by the scalar `a', and return the product."
  (cond ((or (not (numberp a)) (not (vectorp v))) (error "Arguments are not of the correct type."))
	((zerop a) (identity-add (length v)))
	((= a 1) v)
	((vector-null v) v)
	(t (map 'vector #'(lambda (x) (* a x)) v))))

(defun vector* (v1 v2)
  "Return the scalar product of vector `v1' by vector `v2'. 
It is assumed that `v1' and `v2' are given as row vectors, and the scalar product is computed by taking the transpose of `v2'. That is scalar-prod = v1 * transpose(v2)."
  (cond ((or (not (vectorp v1)) (not (vectorp v2)))
	 (error "The arguments must be vectors."))
	((not (= (length v1) (length v2)))
	 (error "Vectors must have the same length."))
	((or (vector-null v1) (vector-null v2)) 0)
	((vector-identity-mult-p v1) (reduce #'+ v2))
	((vector-identity-mult-p v2) (reduce #'+ v1))
	(t (reduce #'+ (map 'vector #'* v1 v2)))))

(defun vector-cross-product (v1 v2)
  "Return the cross product of the given vectors, `v1' and `v2'.
It is assumed that the two vectors are given as row vectors, and the cross product is computed
by taking the transpose of `v1'. That is scalar-prod = transpose(v1) * v2.
This gives a tabular data (or matrix or 2D-array) as a result."
  (let ((n (length v1)))
    (cond ((or (not (vectorp v1)) (not (vectorp v2)))
	   (error "The arguments must be vectors."))
	  ((not (= n (length v2))) (error "Vectors must have the same length."))
	  ((or (vector-null v1) (vector-null v2))
	   (make-array (list n n) :initial-element 0))
	  (t (let ((res (make-array (list n n))))
	       (dotimes (i n)
		 (dotimes (j n)
		   (setf (aref res i j) (* (aref v1 i) (aref v2 j)))))
	       res)))))

(defun square (x)
  (* x x))

(defun vector-norm (v)
  "Return the Euclidean norm (or module) of the given vector."
  (if (not (vectorp v))
      (error "The argument must be a vector.")
      (sqrt (reduce #'+ (map 'vector #'square v)))))

(defun vector-distance (v1 v2)
  "Return the distance the distance between two vectors in the Euclidean space."
  (if (or (not (vectorp v1)) (not (vectorp v2)))
      (error "The arguments must be vectors.")
      (vector-norm (vector- v1 v2))))

;;; Statistics functions
(defun vector-avg (v)
  "Return the aithmetical average of the number in the vector.
The vector is viewed as a plain set."
  (if (not (vectorp v))
      (error "The argument must be a vector.")
      (float (/ (reduce #'+ v) (length v)))))

(defun vector-median (v)
  "Return the median of the vector. It is the element that split the vector (set)
into two subsets of the same size."
  (if (not (vectorp v))
      (error "The argument must be a vector.")
      (let* ((n (length v))
	     (mean-idx (/ n 2))
	     (w (sort v #'<))
	     (median (if (evenp n)
			 (/ (+ (aref w (1- mean-idx)) (aref w mean-idx)) 2.0)
			 (aref v (floor n 2)))))
	median)))

(defun vector-min (v)
  "Return the minimum value of the vector (set) given in argument."
  (apply #'min (coerce v 'list)))

(defun vector-max (v)
  "Return the maximum value of the vector (set) given in argument."
  (apply #'max (coerce v 'list)))

(defun vector-stddev (v)
  "Return the standard deviation of the set `v' assuming that it follows a uniform distribution."
  (let ((avg (vector-avg v))
	(n (length v)))
    (sqrt (/ (reduce #'+ (map 'vector
			      #'(lambda (x) (square (- x avg)))
			      v))
	     n))))


;;; Matrix Algebra
;;; A matrix is a 2D (two-dimensional) array, a vector of vectors.
;;; A (n x m) matrix has `n' rows and `m' columns, or a vector of `n' vectors of length `m'.
;;; `n' is the first dimension and `m' is the second one.
;;; For example:
;;; #((1 2 3) (4 5 6)) is a (2 x 3) array
;;; #((0 -1) (3 3)) is a (2 x 2) array
;;; A vector is just a 1D array.
(defstruct (matrix
	     (:print-function
	      (lambda (struct stream depth)
		(declare (ignore depth))
		(print-matrix stream struct)
		(format stream "~&~%[~A x ~A matrix (~A elements)]~%"
			(matrix-nrows struct) (matrix-ncols struct)
			(* (matrix-nrows struct) (matrix-ncols struct))))))
  (dimensions 0)
  (contents #())
  (nrows 0)
  (ncols 0))

(defun copy-array (m)
  "Return a new array `new-m' that contains the same elements as `m'.
That is (eq m new-m) must evaluate to NIL.
By default, (setq new-m m) does not create a new pointer, 
that is (eq m new-m) ==> T."
  (let ((new-m (make-array (array-dimensions m))))
    (dotimes (i (array-dimension m 0))
      (dotimes (j (array-dimension m 1))
	(setf (aref new-m i j) (aref m i j))))
    new-m))

(defun defmatrix (m)
  "Return a matrix structure from the given array (or list) `m'."
  (when (listp m)
    (setq m (make-array (list (length m) (length (car m)))
			:initial-contents m)))    
  (make-matrix :dimensions (array-dimensions m)
	       :contents (copy-array m)
	       :nrows (array-dimension m 0)
	       :ncols (array-dimension m 1)))

(defun matrix-elt (m row col)
  "Return the element of the matrix `m' at the `row'th row
and `col'th column.
Note that index begins by 0. So to get the element at the first row
and first column, the arguments `row' and `col' must be set to 0 and 0,
respectively."
  (aref (matrix-contents m) row col))

(defun matrix-row (m row)
  "Return the vector that represents the `row'th row of matrix `m'.
Note that for the first row, `row' must be equal to 0; for the second row,
it must be equal to 1, and so on."
  (let* ((row-length (matrix-ncols m))
	 (v (make-array row-length)))
    (dotimes (i row-length)
      (setf (aref v i) (matrix-elt m row i)))
    v))

(defun matrix-column (m col)
  "Return the vector that represents the `col'th column of matrix `m'.
Note that for the first column, `col' must be equal to 0; for the second column,
it must be equal to 1, and so on."
  (let* ((column-length (matrix-nrows m))
	 (v (make-array column-length)))
    (dotimes (i column-length)
      (setf (aref v i) (matrix-elt m i col)))
    v))

(defun matrix-map (fn m)
  "Map function `fn' over the elements of matrix `m', 
and return the resulting matrix."
  (let* ((res (make-array (matrix-dimensions m)))
	 (m1 (matrix-contents m)))
    (dotimes (i (matrix-nrows m))
      (dotimes (j (matrix-ncols m))
	(setf (aref res i j) (funcall fn (aref m1 i j)))))
    (defmatrix res)))

(defun matrix-map-axis (fn m &optional (row nil) (col nil))
  "Map function `fn' over either the `row'th row or the `col'th column
of matrix `m'. Arguments `row' and `col' exclusive, meaning that only one
should be set. If both are set, an exception is raised."
  (cond ((and row col) (error "Only one of arguments `row' or `col' must be set."))
	(t (let ((res m))
	     (if row
		 (dotimes (i (matrix-ncols m))
		   (setf (aref (matrix-contents res) row i)
			 (funcall fn (aref (matrix-contents m) row i))))
		 (dotimes (i (matrix-nrows m))
		   (setf (aref (matrix-contents res) i col)
			 (funcall fn (aref (matrix-contents m) i col)))))
	     res))))

;;; Operations on matrices
(defun matrix-same-dim-p (m1 m2)
  "Return T if the given matrices have the same dimension,
otherwise return NIL."
  (equal (matrix-dimensions m1) (matrix-dimensions m2)))

(defun matrix-flatten (m)
  "Return a list consisting of the concatenation of the rows of
the given matrix in the right order."
  (let ((nrows (matrix-nrows m))
	 (res nil))
     (dotimes (i nrows)
       (setf res (append res (coerce (matrix-row m i) 'list))))
     res))

(defun matrix-compare (pred &rest args)
  "Apply PREDICATE to every two consecutive matrices of the sequences. 
Return NIL as soon as any invocation of PREDICATE returns NIL, or T if every invocation
is non-NIL.
Two local functions are defined. The first one applies PREDICATE to `only' two matrices.
The second one apply the former recursively to two consecutives matrices."
  (labels ((matrix-comp (pred m1 m2)
	     (if (not (equal (matrix-dimensions m1) (matrix-dimensions m2)))
		 (error "Matrices must have the same dimensions.")
		 (block mapvec
		   (dotimes (i (matrix-nrows m1))
		     (when (not (vector-compare pred (matrix-row m1 i)
						(matrix-row m2 i)))
		       (return-from mapvec nil)))
		   t)))
	   (comp-rec (new-args)
	     (cond ((or (= (length new-args) 1) (null new-args)) t)
		   ((not (funcall #'matrix-comp
				  pred (car new-args) (cadr new-args)))
		    nil)
		   (t (comp-rec (cdr new-args))))))
    (comp-rec args)))

(defun matrix-identity-add-p (m)
  "Return T if the given matrix is an addition identity. Otherwise return NIL."
  (matrix-compare #'= m (defmatrix (identity-add (matrix-dimensions m)))))

(defun matrix-null (m)
  "Return T if all the components of the given matrix are 0, otherwise return NIL."
  (matrix-identity-add-p m))

(defun matrix-square-p (m)
  "Return T if the given MATRIX has the same number of rows and columns,
otherwise return NIL."
  (= (matrix-nrows m) (matrix-ncols m)))

(defun matrix-identity-mult (dimensions)
  "Returns the matrix multiplicative identity of the given dimensions.
It is a matrix where the diagonal elements are 1's and the other elements are 0's.
It only makes sense for square matrices."
  (if (not (= (car dimensions) (cadr dimensions)))
      (error "The number of ROWS and COLUMNS must be equal.")
      (let ((id-m (make-array dimensions)))
	(dotimes (i (car dimensions))
	  (setf (aref id-m i i) 1))
	(defmatrix id-m))))

(defun matrix-identity-mult-p (m)
  "Return T if the given matrix is a multiplication identity. Otherwise return NIL."
  (matrix-compare #'= m (matrix-identity-mult (matrix-dimensions m))))

(defun binary-sub (m1 m2)
  "Return the difference of the two given vectors."
  (cond ((not (= (length m1) (length m2))) (error "Vectors don't have the same size."))
	((matrix-identity-add-p m1) (vector-neg m2))
	((matrix-identity-add-p m2) m1)
	(t (map 'vector #'- m1 m2))))

(defun matrix+ (&rest matrices)
  "Return the sum of the given matrices."
  (labels ((binary-add (m1 m2)
	     ;; Return the sum of the two given matrices
	     (cond ((not (matrix-same-dim-p m1 m2))
		    (error "Matrices must have the same size."))
		   ((matrix-identity-add-p m1) m2)
		   ((matrix-identity-add-p m2) m1)
		   (t (let* ((nrows (matrix-nrows m1))
			     (v nil))
			(dotimes (i nrows)  ; Construction of a vector of vectors
			  ;; Each `i'th vector is the sum
			  ;; of the two `i'th rows of both matrices
			  (setf v (append v (list (vector+ (matrix-row m1 i)
							   (matrix-row m2 i))))))
			(defmatrix (make-array (matrix-dimensions m1)
					       :initial-contents v)))))))
    (if (null matrices)
	0
	(reduce #'binary-add matrices))))

(defun matrix-neg (m)
  "Return a matrix where each component is the negative of the corresponding component
in the given matrix, at the same position."
  (cond ((matrix-identity-add-p m) m)
	(t (let ((nrows (matrix-nrows m))
		 (res-m nil))
	     (dotimes (i nrows)
	       (setf res-m (append res-m (list (vector-neg (matrix-row  m i))))))
	     (defmatrix (make-array (matrix-dimensions m)
				    :initial-contents res-m))))))

(defun matrix* (m1 m2)
  "Return the product of the two given matrices."
  (cond ((not (= (matrix-ncols m1) (matrix-nrows m2)))
	 (error "The number of COLUMNS of the first matrix must be equal 
to the number of ROWS of the second one."))
	((or (matrix-identity-add-p m1) (matrix-identity-add-p m2)
	     (matrix-identity-add-p m2)))
	((matrix-identity-mult-p m2) m1)
	((matrix-identity-mult-p m1) m2)
	(t (let* ((nrows (matrix-nrows m1))
		  (ncols (matrix-ncols m2))
		  (res-m (make-array (list nrows ncols))))
	     (dotimes (i nrows)
	       (dotimes (j ncols)
		 (setf (aref res-m i j) (vector* (matrix-row m1 i)
						 (matrix-column m2 j)))))
	     (defmatrix res-m)))))

(defun matrix-ones (dimensions)
  "Return a matrix in which all the elements are 1's."
  (defmatrix (make-array dimensions :initial-element 1)))

(defun matrix-scalar-mult (scalar m)
  "Return a matrix where each element is the product of the given SCALAR
and each element of the given MATRIX."
  (cond ((not (and (matrix-p m) (numberp scalar)))
	      (error "The first argument must be a NUMBER and the second one
must be a MATRIX."))
	(t (let ((nrows (matrix-nrows m))
		 (res-m nil))
	     (dotimes (i nrows)
	       (setf res-m (append res-m
				   (list (vector-scalar-mult scalar (matrix-row m i))))))
	     (defmatrix (make-array (matrix-dimensions m)
				    :initial-contents res-m))))))

(defun matrix-same-elt-p (eq-pred m)
  "Return T if all the elements of the matrix are equal by the given EQUALITY PREDICATE."
  (let* ((arr (matrix-contents m))
	 (pivot (aref arr 0 0)))
    (if (eq eq-pred #'=)
	(let ((dim (matrix-dimensions m)))
	  (matrix-compare #'= (matrix-ones dim) (matrix-scalar-mult (/ 1 pivot) m)))
	(block comp	
	  (dotimes (i (matrix-nrows m))
	    (dotimes (j (matrix-ncols m))
	      (if (not (funcall eq-pred pivot (aref arr i j)))
		  (return-from comp nil))))
	  t))))

(defun matrix-diag (m)
  "Return the diagonal vector of the given MATRIX.
It makes sense only for square matrices."
  (cond ((not (matrix-square-p m))
	 (error "The number of COLUMNS and ROWS of the given MATRIX must be equal."))
	(t (let ((res-v (make-array (matrix-nrows m)))
		 (arr (matrix-contents m)))
	     (dotimes (i (matrix-nrows m))
	       (setf (aref res-v i) (aref arr i i)))
	     arr))))

(defun matrix-diag-set (diag-v &optional (m nil))
  "Set the given VECTOR as the diagonal of the given MATRIX. If no matrix is given,
return a diagonal matrix.
It makes sense only for square matrices."
  (cond ((and m (not (matrix-square-p m)))
	 (error "The number of COLUMNS and ROWS of the given MATRIX must be equal."))
	((not (null m))
	 (let ((arr (copy-array (matrix-contents m))))
	   (dotimes (i (matrix-nrows m))
	     (setf (aref arr i i) (aref diag-v i)))
	   (defmatrix arr)))
	(t (let* ((len (length diag-v))
		  (arr (identity-add (list len len))))
	     (dotimes (i len)
	       (setf (aref arr i i) (aref diag-v i)))
	     (defmatrix arr)))))
#|
(defun copy-matrix (m)
  "Return a copy `new-m' of the given MATRIX `m', so that (eq m new-m) evaluates to NIL."
  (defmatrix (copy-array (matrix-contents m))))
|#

(defun matrix-aref (m row col)
  "Return the element of the given MATRIX at the position specified by the ROW 
and COLUMN arguments."
  (aref (matrix-contents m) row col))

(defun matrix-setf (m row col newval)
  "Set the element of the MATRIX at the given position ROW and COLUMN, 
to the given VALUE."
  (let ((arr (matrix-contents m)))
    (setf (aref arr row col) newval)))

(defun matrix-set-row (m row-number new-row)
  "Set the given ROW at the given row position of the MATRIX."
  (cond ((not (= (length new-row) (matrix-nrows m)))
	 (error "The new row must have the same length as the row the matrix."))
	((not (vectorp new-row)) (error "The new row must be a vector."))
	(t (let ((arr (matrix-contents m)))
	     (dotimes (i (matrix-ncols m))
	       (setf (aref arr row-number i) (aref new-row i)))
	     m))))

(defun matrix-set-column (m col-number new-col)
  "Set the given COLUMN at the given column position of the MATRIX."
  (cond ((not (= (length new-col) (matrix-ncols m)))
	 (error "The new column must have the same length as the column the matrix."))
	((not (vectorp new-col)) (error "The new column must be a vector."))
	(t (let ((arr (matrix-contents m)))
	     (dotimes (i (matrix-ncols m))
	       (setf (aref arr i col-number) (aref new-col i)))
	     m))))

(defun matrix-T (m)
  "Return the transpose of the given matrix.
It makes sense only for square matrices."
  (let ((new-m (copy-matrix m)))
    (format t "~S~%" new-m)
    (cond ((not (matrix-square-p m))
	   (error "The number of COLUMNS and ROWS of the given MATRIX must be equal."))
	  ((or (matrix-identity-add-p m) (matrix-identity-mult-p m)) new-m)
	  ((matrix-same-elt-p #'equalp m) new-m)
	  (t (let ((nrows (matrix-nrows m))
		   (ncols (matrix-ncols m)))
	       (dotimes (i nrows)
		 (dotimes (j ncols)
		   (when (not (= i j))  ; leave the diagonal unchanged
		     (matrix-setf new-m i j (matrix-aref m j i)))))
	       new-m)))))

(defun matrix-symetric-p (m)
  "Return T if the given MATRIX is symmetric, otherwise return NIL.
It makes sense only for square matrices."
  (cond ((not (matrix-square-p m))
	 (error "The number of COLUMNS and ROWS of the given MATRIX must be equal."))
	(t (matrix-compare #'equalp m (matrix-T m)))))

;;; Printing functions
(defun type->string (x)
  "Coerce the given data (of any `type') into string.
E.g.: (type->string 125) ==> \"125\"
      (type->string '(1 2 5)) ==> \"(1 2 5)\""
  (typecase x
    (string x)
    (symbol (symbol-name x))
    (t (write-to-string x))))

(defun cols-width (m)
  "Returns a list that contains the width of each column of the given matrix `m'.
The width is the length of the longest element of the column.
To get the length of an element, it is first converted into string."
  (let ((w-lst nil))
    (dotimes (i (matrix-ncols m))
      (setf w-lst
	    (append w-lst
		    (list
		     (vector-max (matrix-column (matrix-map #'length
							   (matrix-map #'type->string m))
						i))))))
    w-lst))

(defun print-row (destination row widths &key (padding 1) (sep #\Space))
  "Print the row `row' where each column's width is specified in the list `widths'.
The argument `padding' specify the number of blank space between an element 
and the separator `sep'. Separator can be any character."
  (let ((row-lst (coerce row 'list)))
    (labels ((print-row-rec (row widths)
	       (cond ((null row) (format destination "~%"))
		     (t (format destination (concatenate 'string
					       "~" (write-to-string (car widths))
					       "@S")
				(car row))
			(when (not (null (cdr row)))
			  (format destination (concatenate 'string
						 "~," (write-to-string padding)
						 "T~C~," (write-to-string padding) "T")
				  sep))
			(print-row-rec (cdr row) (cdr widths))))))
      (print-row-rec row-lst widths))))

(defun print-matrix (destination m &key (padding 1) (sep #\Space))
  "Print matrix `m' in tabular format."
  (let ((widths (cols-width m)))
    (dotimes (i (matrix-nrows m))
      (print-row destination (matrix-row m i) widths :padding padding :sep sep))))




