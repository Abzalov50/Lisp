#| An implementation of the Differential Evolution (DE) algorithm 
for solving constrained optimization problems.
Based on the Python implementation available here: 
https://nathanrooy.github.io/posts/2017-08-27/simple-differential-evolution-with-python/

The basic structure of a DE can be summed up as follows:
1) Initialize a random population of individuals throughout the search space.
2) while iter <= max num of generations
3) cycle through each individual in the population    
   3.A) perform mutation
   3.B) perform recombination ("crossover" in GA lingo)
   3.C) perform selection
4) if stopping criterion has been met:
      exit and return best individual
   else:
      iter = iter + 1
      go back to step #3
|#
(defparameter *maxgen*    10)
(defparameter *popsize*   50)
(defvar       *pop*       nil)

(defun init-pop (bounds)
  (dotimes (i *popsize*)
    (let ((ind nil))
      (setf *pop*
	    (append *pop*
		    (dotimes (j (length bounds) (list ind))
		      (setf ind (append ind (random 
