(defvar *current-website* nil)
(defparameter *default-content*
  '(format nil "Welcome to this new Web App!"))
(defparameter *default-www-root*
  (merge-pathnames #P"Documents/Lisp" (user-homedir-pathname)))

(defstruct website
  "A website is a set of web `pages' located at a `domain'."
  domain-name (pages (make-hash-table)) (object nil))

(defstruct page
  "A page is a structured markup language text that has a `name' and a `content'.
`get-post-params' is a list of either symbols `var' or lambda-lists of the form `(var &key real-name parameter-type init-form request-type)'."
  name (uri nil) (content *default-content*) (get-post-params nil))

;;; TODO: Replace `push' by the appropriate method to insert into hash-table.
;;; Throw an error if the `name' is already in the `pages' hash-table.
(defun create-page (name &key content
			   (website *current-website*)
			   get-post-params
			   uri)
  "Return a page with the given `name' and `content' and register it into the `website'. If no `website' is given, it is just an orphan page. The `name' must be UNIQUE otherwise an error is returned."
  (let ((page (make-page :name name
			 :uri uri
			 :content content
			 :get-post-params get-post-params)))
    (when website
      (if (gethash name (website-pages website))
	  (when (y-or-n-p "Cette page existe déjà. Souhaitez-vous la mettre à jour ?")
	    (setf (gethash name (website-pages website)) page))
	  (setf (gethash name (website-pages website)) page)))
    page))

(defmacro generate-handlers (pages)
  `(maphash #'(lambda (key page)
		(declare (ignore key))
		(let ((name (page-name page))
		      (uri (page-uri page))
		      (get-post-params
		       (page-get-post-params page))
		      (content (page-content page)))
		  (easy-handler name uri get-post-params content)))
	    `,,pages))

(defmacro easy-handler (name uri get-post-params content)
  `(hunchentoot:define-easy-handler
       (,name :uri ,uri)
       (,@get-post-params)
     ,@content))

(defun serve-website (website &key (port 8000) (mode 'dev)
				(www-root *default-www-root*))
  (let ((address (cond ((eq mode 'dev) "localhost")
		       ((eq mode 'prod) (website-domain-name website))
		       (t (error "Mode not supported!")))))
    (setf (website-object website)
	  (hunchentoot:start (make-instance 'hunchentoot:easy-acceptor
					    :address address
					    :port port
					    :document-root www-root)))
    website))


(hunchentoot:define-easy-handler (home :uri "/home") (name)
	   (format nil "Hey~@[ ~A~]!" name))
