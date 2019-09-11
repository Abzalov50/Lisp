(defpackage :retro-games
  (:use :cl :cl-who :hunchentoot :parenscript :cl-mongo))
(in-package :retro-games)

;;; Definition of data-structures
(defvar *games* nil)
(defparameter *game-collection* "game")
(cl-mongo:db.use "games")

(defclass game ()
  ((name :reader name
	 :initarg :name)
   (votes :accessor votes
	  :initform 0)))

(defmethod print-object ((object game) stream)
  (print-unreadable-object (object stream :type t)
    (with-slots (name votes) object
      (format stream "name: ~S with ~D votes"  name votes))))

;;; Helping functions
(defmethod vote-for (user-selected-game)
  (incf (votes user-selected-game)))

(defun game-from-name (name)
  (let ((found-games (docs (db.find *game-collection*
				    ($ "name" name)))))
    (when found-games
      (doc->game (first found-games)))))

(defun game-stored-p (game-name)
  (game-from-name game-name))

(defun games ()
  (sort (copy-list *games*) #'> :key #'votes))

(defun add-game (name)
  (unless (game-stored-p name)
    (push (make-instance 'game :name name) *games*)))

;;; Web functions/macros
(setf (html-mode) :html5)

(defmacro standard-page ((&key title script) &body body)
  `(with-html-output-to-string
       (*standard-output* nil :prologue t :indent t)
     (:html :lang "en"
	    (:head
	     (:meta :charset "utf-8")
	     (:title ,title)
	     (:link :type "text/css"
		    :rel "stylesheet"
		    :href "/retro.css"))
	    ,(when script
	       `(:script :type "text/javascript"
			 (str ,script)))
	    (:body
	     (:header
	      (:img :src "/logo.jpg"
		    :alt "Commodore 64"
		    :class "logo")
	      (:span :class "strapline"
		     "Vote on your favorite Retro Game"))
	     ,@body))))

(defun start-server (port)
  (start (make-instance 'easy-acceptor :port port)))

(define-easy-handler (retro-games :uri "/retro-games") ()
  (standard-page
      (:title "Retro Games")			
      (:h1 "Vote on your all time favourite retro games!")
      (:p "Missing a game? Make it available for votes "
	  (:a :href "new-game" "here"))
      (:h2 "Current stand")
      (:div :id "chart"
	    (:ol
	     (dolist (game (games))
	       (htm
		(:li (:a :href (format nil "vote?name=~A"
				       (url-encode  ; avoid injection attacks
					(name game))) "Vote!")
		     (fmt "~A with ~D votes" (escape-string (name game))
			  (votes game)))))))))

(define-easy-handler (vote :uri "/vote") (name)
  (when (game-stored-p name)
    (vote-for (game-from-name name)))
  (redirect "/retro-games"))

(define-easy-handler (new-game :uri "/new-game") ()
  (standard-page (:title "Add a new game"
		  :script (ps  ; client-side validation
			(defvar add-form nil)
			(defun validate-game-name (evt)
			  (when (= (@ add-form name value) "")
			    (chain evt (prevent-default))
			    (alert "Please enter a name")))
			(defun init ()
			  (setf add-form (chain document
						(get-element-by-id "addform")))
			  (chain add-form
			       (add-event-listener "submit"
						   validate-game-name false)))
			(setf (chain window onload) init)))
    (:h1 "Add a new game to chart")
    (:form :action "/game-added" :method "post" :id "addform"
	   (:p "What is the name of the game?" (:br)
	       (:input :type "text" :name "name" :class "txt"))
	   (:p (:input :type "submit" :value "Add" :class "btn")))))

(define-easy-handler (game-added :uri "/game-added") (name)
  (unless (or (null name) (zerop (length name)))
    (add-game name))
  (redirect "/retro-games"))
