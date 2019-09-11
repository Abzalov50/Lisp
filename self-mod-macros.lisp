;; macro to compare two symbols for a match
(defmacro symbol-equal (sym1 sym2)
  `(string= (symbol-name ,sym1) (symbol-name ,sym2)))

;; macro to capture the defintion form of a function
(defmacro get-form (func)
  `(cond
     ((not (listp ,func)) (format t "Form error: not a list ~A~%" ,func) 'err)
     ((null ,func) (format t "Form error: empty list ~A~%" ,func) 'err)
     ((symbol-equal (car ,func) 'LAMBDA-BLOCK) 'defun)
     ((symbol-equal (car ,func) 'LAMBDA) 'lambda)
     ((symbol-equal (car ,func) 'LAMBDA-CLOSURE) 'closure)
     ((symbol-equal (car ,func) 'LAMBDA-BLOCK-CLOSURE) 'labels)
     (t (format t "Not recognized form ~A~%" (car ,func) 'err))))

;; macro to print a function, but with circle-detection on to handle label's environment lists
(defmacro print-func (func)
  `(let ((*print-circle* t)) (format t "~A~%" ,func)))

;; macros to capture the name, parameter list, body, and environment
;; (accessible variables/parameters) of a function
;; (after identifying the form of the function)
(defmacro get-name (func)
  `(case (get-form ,func)
     (defun (if (< (length ,fun) 2) 'err (cadr ,func)))
     (lambda 'lambda)
     (closure 'closure)
     (labels (if (< (length ,func) 5) 'err (cddddr ,func)))
     (t 'err)))

(defmacro get-params (func)
  `(case (get-form ,fun)
     (defun (if (< (length ,func) 3) 'err (caddr ,func)))
     (lambda (if (< (length ,func) 2) 'err (cadr ,func)))
     (closure (if (< (length ,func) 5) 'err (car (cddddr ,func))))
     (labels (if (< (length ,func) 6) 'err (cadr (cddddr ,func))))
     (t 'err)))

(defmacro get-body (func)
  `(case (get-form ,fun)
     (defun (if (< (length ,func) 3) 'err (cdddr ,func)))
     (lambda (if (< (length ,func) 2) 'err (cddr ,func)))
     (closure (if (< (length ,func) 5) 'err (cdr (cddddr ,func))))
     (labels (if (< (length ,func) 6) 'err (cddr (cddddr ,func))))
     (t 'err)))

(defmacro get-env (func)
  `(case (get-form ,fun)
     (defun (format t "No env in a defun~%"))
     (lambda (format t "No env in a defun~%"))
     (closure (if (< (length ,func) 2) 'err (cadr ,func)))
     (labels (if (< (length ,func) 2) 'err (cadr ,func)))
     (t 'err)))

;; function to analyze all the parts of a function
(defun basic-analyze (func)
  (format t "~%Name of function is ~A~%" (get-name func))
  (format t "   param-list of function is ~A~%" (get-params func))
  (format t "   env of function is ~A~%" (get-env func))
  (format t "   body of function is ~A~%" (get-body func)))

;;------------ fcore macro implementations ---------------------------
;; A set of macros to use in generating and modifying functions
;; of the following fixed form:
;;       (lambda-closure envVarList nil nil parameterList
;;            (let* localVarList
;;                  (label localMethodList
;;                      (block SetupStatements statementlist)
;;                      (cond  caseList))))
;;
;;    the parameter list will initially be ( &optional &rest rest), while the
;;    localVarList, localMethodList, and statementList will initially be nil.

;; (fgen-func) - generate a skeleton of the form above
(defmacro fgen-func ()
  `(lambda (&optional &rest rest)
     (let* ()
       (labels nil
	 (block setup-statements nil)
	 (cond (t nil))))))

;; (fcheck-form func) - test if a function has the form above
