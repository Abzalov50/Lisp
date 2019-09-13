;;; A library on vector algebra.
;;; Common Lisp (CL) offers a type `vector' with a predicate `vectorp'
;;; to test if an object is a vector. A CL vector is
;;; a `sequence' of data of arbitrary types.
;;; The following are valid CL vectors:
;;; #(1 2 3 -1 0)
;;; #("arnold" -2 'xcopy)

(defparameter *padding* 2)
(defparameter *sep* #\Space)

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
		     (matrix-max (matrix-aref (matrix-map #'length
							  (matrix-map #'type->string m))
					      :col i))))))
    w-lst))

(defun print-row (destination row widths &key (padding 1) (sep #\Space))
  "Print the row `row' where each column's width is specified in the list `widths'.
The argument `padding' specify the number of blank space between an element 
and the separator `sep'. Separator can be any character."
  (dotimes (i (matrix-nrows row))
    (format destination (concatenate 'string
				     "~" (write-to-string (elt widths i)) "<~S~>")
	    (aref (matrix-contents row) i 0))
    (when (not (= i (matrix-nrows row)))
      (format destination (concatenate 'string
				       "~" (write-to-string padding) "<~C~>")
				       sep)))
  (format destination "~%"))

(defun print-matrix (destination m &key (padding 1) (sep #\Space))
  "Print matrix `m' in tabular format."
  (let ((widths (cols-width m)))
    (dotimes (i (matrix-nrows m))
      (print-row destination (matrix-aref m :row i) widths :padding padding :sep sep))))

(defun copy-array (m)
  "Return a new array `new-m' that contains the same elements as `m'.
That is (eq m new-m) must evaluate to NIL.
By default, (setq new-m m) does not create a new pointer, 
that is (eq m new-m) ==> T."
  (let* ((ndim (array-dimensions m))
	 (new-m (make-array ndim)))
    (cond ((= (length ndim) 1)
	   (dotimes (i (elt ndim 0))
	     (setf (aref new-m i) (aref m i))))
	  ((= (length ndim) 2)
	   (dotimes (i (elt ndim 0))
	     (dotimes (j (elt ndim 1))
	       (setf (aref new-m i j) (aref m i j)))))
	  (t (error "Unsupported dimension of matrix.")))
    new-m))

(defstruct (matrix
	     (:print-function
	      (lambda (struct stream depth)
		(declare (ignore depth))
		(print-matrix stream struct :padding *padding* :sep *sep*)
		(format stream "~&~%[~A x ~A matrix (~A elements)]~%"
			(matrix-nrows struct) (matrix-ncols struct)
			(* (matrix-nrows struct) (matrix-ncols struct))))))
  (dimensions 0)
  (contents #())
  (nrows 0)
  (ncols 0))

(defun defmatrix (m &key (inplace nil))
  "Return a matrix structure from the given array (or list) `m'.
When a list or vecteor of length N is given, a matrix of dimension (N 1) is returned,
that is, ALWAYS a column vector."
  (if (matrix-p m)
      (if inplace
	  m
	  (copy-matrix m))
      (let ((nrows 0) (ncols 0) (contents nil))
	(etypecase m
	  (list
	   (setq nrows (length m)
		 contents (if inplace m (copy-seq m)))
	   (etypecase (car m)
	     (atom
	      (setq ncols 1))
	     (cons
	      (setq ncols (length (car m))))))
	  (vector
	   (setq nrows (length m)
		 ncols 1
		 contents (if inplace m (copy-seq m))))
	  (array
	   (setq nrows (array-dimension m 0)
		 ncols (array-dimension m 1)
		 contents (if inplace m (copy-array m)))))
	(make-matrix :dimensions (list nrows ncols)
		     :contents contents
		     :nrows nrows
		     :ncols ncols))))

(defun matrix-map (fn m)
  "Map function `fn' over the elements of matrix `m', 
and return the resulting matrix."
  (let* ((res (make-array (matrix-dimensions m)))
	 (m1 (matrix-contents m))
	 (ncols (matrix-ncols m)))
    (dotimes (i (matrix-nrows m))
      (dotimes (j ncols)
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
       (setf res (append res (coerce (matrix-aref m :row i) 'list))))
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
		   (let ((arr1 (matrix-contents m1)) (arr2 (matrix-contents m2)))
		     (dotimes (i (matrix-nrows m1))
		       (dotimes (j (matrix-ncols m1))
			 (when (not (funcall pred (aref arr1 i j) (aref arr2 i j)))
			   (return-from mapvec nil)))))
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
		   (t (let* ((res-m (copy-matrix m1))
			     (arr (matrix-contents res-m))
			     (arr-1 (matrix-contents m1))
			     (arr-2 (matrix-contents m2)))
			(dotimes (i (matrix-nrows m1))
			  (dotimes (j (matrix-ncols m1))
			    (setf (aref arr i j) (+ (aref arr-1 i j) (aref arr-2 i j)))))
			(defmatrix arr))))))
    (if (null matrices)
	0
	(reduce #'binary-add matrices))))

(defun matrix-neg (m)
  "Return a matrix where each component is the negative of the corresponding component
in the given matrix, at the same position."
  (cond ((matrix-identity-add-p m) m)
	(t (matrix-map #'- m))))

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
		 (setf (aref res-m i j) (vector* (matrix-aref m1 :row i)
						 (matrix-aref m2 :col j)))))
	     (defmatrix res-m)))))

(defun matrix-ones (dimensions)
  "Return a matrix in which all the elements are 1's."
  (defmatrix (make-array dimensions :initial-element 1)))

(defun matrix-zeros (dimensions)
  "Return a matrix in which all the elements are 0's."
  (defmatrix (make-array dimensions :initial-element 0)))

(defun matrix-scalar-mult (scalar m)
  "Return a matrix where each element is the product of the given SCALAR
and each element of the given MATRIX."
  (cond ((not (and (matrix-p m) (numberp scalar)))
	      (error "The first argument must be a NUMBER and the second one
must be a MATRIX."))
	(t (matrix-map #'(lambda (x) (* scalar x))
		       m))))

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

(defun matrix-aref-2 (m &key row col)
  "Return the element, row or column of the given MATRIX at the position 
specified by the ROW and COLUMN arguments."
  (cond ((and (not row) (not col))
	 m)	 
	((and row (not col))
	 (let* ((ncols (matrix-ncols m))
		(new-arr (make-array (list ncols 1)))
		(arr (matrix-contents m)))
	   (dotimes (j ncols)
	     (setf (aref new-arr j 0) (aref arr row j)))
	   (defmatrix new-arr)))
	((and col (not row))
	 (let* ((nrows (matrix-nrows m))
		(new-arr (make-array (list nrows 1)))
		(arr (matrix-contents m)))
	   (dotimes (i nrows)
	     (setf (aref new-arr i 0) (aref arr i col)))
	   (defmatrix new-arr)))
	(t (aref (matrix-contents m) row col))))

(defun matrix-aref (m &key row col)
  "Return the element, row or column of the given MATRIX at the position 
specified by the ROW and COLUMN arguments."
  (cond ((and (not row) (not col))
	 m)
	(t (let* ((ncols (matrix-ncols m))
		  (nrows (matrix-nrows m))
		  (row (cond ((not row) (list 0 nrows))
			     ((numberp row) (list row (1+ row)))
			     ((listp row) row)
			     (t (error "Unsupported type"))))
		  (col (cond ((not col) (list 0 ncols))
			     ((numberp col) (list col (1+ col)))
			     ((listp col) col)
			     (t (error "Unsupported type"))))
		  (nc (- (second col) (first col)))
		  (nr (- (second row) (first row)))
		  (new-arr (make-array (list nr nc)))
		  (arr (matrix-contents m)))
	     (do ((i (first row) (1+ i))
		  (p 0 (1+ p)))
		 ((= i (second row)) (defmatrix new-arr))
	       (do ((j (first col) (1+ j))
		    (s 0 (1+ s)))
		   ((= j (second col)))
		 (setf (aref new-arr p s) (aref arr i j))))))))

(defun matrix-setf (m newval &key row col)
  "Set the element, row or column of the MATRIX at the given position ROW and COLUMN, 
to the given VALUE."
  (if (and (not row) (not col))
      m
      (let ((arr (matrix-contents m))
	    (arr-1 (matrix-contents newval)))
	(cond
	  ((and col (not row))
	   (dotimes (i (matrix-nrows newval))
	     (setf (aref arr i col) (aref arr-1 i 0))))
	  ((and row (not col))
	   (dotimes (j (matrix-nrows newval))
	     (setf (aref arr row j) (aref arr-1 j 0))))
	  (t (setf (aref arr row col) newval)))
	(defmatrix arr))))

(defun matrix-T (m)
  "Return the transpose of the given matrix.
It makes sense only for square matrices."
  (cond ((and (not (matrix-vector-p m)) (not (matrix-square-p m)))
	 (error "The number of COLUMNS and ROWS of the given MATRIX must be equal or the matrix must be a 1D matrix"))
	((and (not (matrix-vector-p m)) (or (matrix-same-elt-p #'equalp m)
					    (matrix-identity-mult-p m)))
	 (defmatrix m))
	((and (not (matrix-vector-p m)) (matrix-identity-add-p m))
	 (defmatrix m))
	(t
	 (let* ((new-m (copy-matrix m))
		(arr (matrix-contents new-m))
		(nrows (matrix-nrows new-m))
		(ncols (matrix-ncols new-m)))
	   (cond ((matrix-column-p m)
		  (setf (matrix-dimensions new-m) (list 1 nrows)
			(matrix-nrows new-m) 1
			(matrix-ncols new-m) nrows
			(matrix-contents new-m) (let ((arr-1 (make-array
							      (matrix-dimensions new-m))))
						  (dotimes (j nrows)
						    (setf (aref arr-1 0 j)
							  (aref arr j 0)))
						  arr-1)))
		 ((matrix-row-p m)
		  (setf (matrix-dimensions new-m) (list ncols 1)
			(matrix-nrows new-m) ncols
			(matrix-ncols new-m) 1
			(matrix-contents new-m) (let ((arr-1 (make-array
							      (matrix-dimensions new-m))))
						  (dotimes (i ncols)
						    (setf (aref arr-1 i 0)
							  (aref arr 0 i)))
						  arr-1)))
		 (t (dotimes (i nrows)
		      (dotimes (j ncols)
			(when (not (= i j))  ; leave the diagonal unchanged
			  (setf (aref arr i j) (aref arr j i)))))))
	   new-m))))

(defun matrix-column-p (m)
  "Return T is the givem matrix is a column array, that is, its second dimension is 1,
otherwise return NIL."
  (= (matrix-ncols m) 1))

(defun matrix-row-p (m)
  "Return T is the givem matrix is a row array, that is, its first dimension is 1,
otherwise return NIL."
  (= (matrix-nrows m) 1))

(defun matrix-vector-p (m)
  "Return T is the givem matrix is a vector, that is, one of its dimensions is 1,
otherwise return NIL."
  (or (= (matrix-nrows m) 1) (= (matrix-ncols m) 1)))

(defun matrix-symetric-p (m)
  "Return T if the given MATRIX is symmetric, otherwise return NIL.
It makes sense only for square matrices."
  (cond ((not (matrix-square-p m))
	 (error "The number of COLUMNS and ROWS of the given MATRIX must be equal."))
	(t (matrix-compare #'equalp m (matrix-T m)))))

(defun matrix-augment (m1 m2 &key (axis 0))
  "Augment matrix `m1' with matrix `m2' along the given axis defaulted to 0."
  (when (or (and (= axis 1) (not (= (matrix-ncols m1) (matrix-ncols m2))))
	    (and (= axis 0) (not (= (matrix-nrows m1) (matrix-nrows m2)))))
      (error "The two given matrices must have conforming dimensions."))
  (let* ((nrows (matrix-nrows m1)) (ncols (matrix-ncols m1))
	 (nrows-2 (matrix-nrows m2)) (ncols-2 (matrix-ncols m2)))
    (cond ((= axis 0)
	   (let* ((new-m (matrix-zeros (list nrows (+ ncols ncols-2)))))
	     (dotimes (j ncols)
	       (matrix-setf new-m (matrix-aref m1 :col j) :col j))
	     (dotimes (j ncols-2)
	       (matrix-setf new-m (matrix-aref m2 :col j) :col (+ ncols j)))
	     new-m))
	  ((= axis 1)
	   (let* ((new-m (matrix-zeros (list (+ nrows nrows-2) ncols))))
	     (dotimes (j ncols)
	       (matrix-setf new-m (matrix-aref m1 :col j) :col j))
	     (dotimes (i nrows-2)
	       (matrix-setf new-m (matrix-aref m2 :row i) :row (+ nrows i)))
	     new-m))
	  (t (error "Matrix does not have this axis.")))))
 
(defun matrix-max (m)
  "Return the maximum value of each element of the given matrix."
  (let* ((arr (matrix-contents m))
	 (the-max (aref arr 0 0)))
    (dotimes (i (matrix-nrows m))
      (dotimes (j (matrix-ncols m))
	(when (> (aref arr i j) the-max)
	  (setf the-max (aref arr i j)))))
    the-max))

(defun matrix-swap (m idx-1 idx-2 &key (axis 0))
  "Swap vectors of the given indices along the given axis.
If `inplace' is set to T, the given is DESTRUCTIVELY modified and
returned, otherwise a new matrix is created."
  (cond ((= axis 0)
	 (let ((temp (matrix-aref m :row idx-1)))
	   (matrix-setf m (matrix-aref m :row idx-2) :row idx-1)
	   (matrix-setf m temp :row idx-2)))
	((= axis 1)
	 (let ((temp (matrix-aref m :col idx-1)))
	   (matrix-setf m (matrix-aref m :col idx-2) :col idx-1)
	   (matrix-setf m temp :col idx-2)))
	(t (error "Matrix does not have this axis."))))

(defun gauss-elimination (A b)
  "Perform a Gauss elimination on the given matrix with the second argument being the right-hand side of the system of linear equations of the form `Ax+b'"
  (let ((nrows (matrix-nrows A))
	(ncols (matrix-ncols A))
	(tol 1e-6)
	(A (matrix-augment A b :axis 0)))
    (labels ((select-pivot (A col)
	       (print "IN")
	       (print col)
	       (print nrows)
	       (do* ((i col (1+ i))
		     (flag nil (= i nrows)))
		    ((or (= i nrows) (> (abs (matrix-aref A :row i :col col)) tol))
		     (values (if flag nil (matrix-aref A :row i :col col))
			     i flag))
		 (print "INSIDE")
		 (print i))))
      (block outer
	(dotimes (j ncols)
	  (print "OUT")
	  (print j)
	  (block inner
	    (when (= (1+ j) nrows)
	      (print "TRUE")
	      (return-from outer A))
	    (multiple-value-bind (pivot p-idx flag)
		(select-pivot A j)
	      (format t "~&pivot ~A: ~A~%" j pivot)
	      (when flag (return-from inner))
	      (setf A (matrix-swap A j p-idx))
	      (do* ((i (1+ j) (1+ i)))
		   ((= i nrows))
		(print "OUTSIDE")
		(let ((row-modif (matrix-aref A :row i))
		      (lead (matrix-aref A :row i :col j)))
		  (tagbody
		     (when (zerop lead)
		       (go continue))
		     (setf A (matrix-setf
			      A
			      (matrix+ (matrix-aref A :row p-idx)
				       (matrix-neg
					(matrix-scalar-mult (/ pivot lead) row-modif)))
			      :row i))
		     (print A)
		   continue))))))))))

(setf A (defmatrix (make-array '(3 3)
			       :initial-contents '((1 2 3) (-3 -2 -1) (4 4 4)))))

;(setf b (defmatrix (make-array '(3 1) :initial-contents '((1) (2) (3)))))
