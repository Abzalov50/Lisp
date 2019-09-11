(defstruct neural-net
  shape inputs weights)

(defun define-neural-net (nodes inputs weights)
  "Define a new neural network, where:
* `shape' is a list containing the numbers of nodes of each layer 
including the input layer.
* `inputs' is a list containing the input node data.
* `weigths' is a list of lists containing the weights between two layers."
  (make-neural-net :node-struct nodes
		   :inputs inputs
		   :weights nil))

(defun sigmoid (x)
  (/ 1 (+ 1 (exp (- x)))))

(defun scalar-prod (u v)
  (reduce #'+ (mapcar #'(lambda (x v) (* x v))
		      u v)))

(+ 1 2 3)

(defun layer-node-state (layer neural-net)
  (let ((layer-len (nth layer (neural-net-nodes neural-net)))
	(full-net  (cons 
