;; a tag is of the form (list-of-tags value)
;; Example: ((tel Arnold office) 213)
(defvar *tag-db* nil)

(defparameter *tag-equivalences*
  '((tel telephone number)
    (adr address)
    (pw passwd password)))

(defun tag-insert (tags value)
  (set '*tag-db* (adjoin (list tags value) *tag-db*
			 :test #'equal)))

(defun tag-query (tags)
  (mapcan (lambda (item)
	    (if (not (set-difference tags (car item)
				     :test #'tags-equal))
		(list (list (set-difference (car item) tags
					    :test #'tags-equal)
			    (cadr item)))))
	  *tag-db*))

(defun tag-save ()
  (with-open-file (stream (merge-pathnames
			   (user-homedir-pathname)
			   (pathname "tagdb.dat"))
			  :direction :output
			  :if-exists :supersede)
    (print *tag-db* stream))
  t)

(defun tag-load ()
  (with-open-file (stream (merge-pathnames
			   (user-homedir-pathname)
			   (pathname "tagdb.dat")))
    (set '*tag-db* (read stream))))

(defun tags-equal (a b)
  (or (equal a b)
      (mapcan (lambda (e)
		(and (member a e) (member b e)))
	      *tag-equivalences*)))

(defun ti (tags value)
  (tag-insert tags value)
  (tag-save))

(defun tq (&rest tags)
  (tag-query tags))
