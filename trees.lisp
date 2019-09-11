;;; A tree is a structure that has nodes and branches.
;;; A recursive definition of a tree is that it is either
;;; an atom (terminal node) or a tree.
;;; It may be modelled as (value left-subtree right-subtree)
;;; When each node has only one left branch and one right branch,
;;; it is called `binary tree'.
(defun value (tree)
  (first tree))

(defun left-subtree (tree)
  (second tree))

(defun right-subtree (tree)
  (third tree))

(defun make-tree (value left right)
  (list value left right))

(defun insert-node (node tree)
  (cond ((null tree) (make-tree node nil nil))
	((= node (value tree)) tree)
	((< node (value tree))
	 (make-tree (value tree)
		    (insert-node node (left-subtree tree))
		    (right-subtree tree)))
	(t (make-tree (value tree)
		      (left-subtree tree)
		      (insert-node node (right-subtree tree))))))

(defun delete-node (node tree)
  (cond ((null tree) nil)
	((= node (value tree)) (make-tree nil nil nil))
	(t (make-tree (value tree)
		      (delete-node node (left-subtree tree))
		      (delete-node node (right-subtree tree))))))
		       
(defun search-node (node tree)
  (cond ((null tree) nil)
	((or (= node (value tree))
	     (equal node (first (left-subtree tree)))
	     (equal node (first (right-subtree tree))))
	 tree)
	((< node (value tree)) (search-node node (left-subtree tree)))
	(t (search-node node (right-subtree tree)))))
