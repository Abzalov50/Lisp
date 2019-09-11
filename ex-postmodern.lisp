(in-package :postmodern)

(defparameter *default-database* "ex-pomo")
(defparameter *database-user* "postgres")
(defparameter *database-password* "Jesuschrist1")

(query (:create-table countries
		      ((id :type int4 :primary-key t)
		       (name :type varchar :default "")
		       (region-id :type int4 :default 0)
		       (latitude :type numeric :default 0)
		       (longitude :type numeric :default 0)
		       (iso :type bpchar :default "")
		       (currency :type varchar :default "")
		       (text :type text :default ""))
		      (:foreign-key (region-id) (regions id))))

(query (:create-table regions
		      ((id :type int4 :primary-key t)
		       (date :type timestamptz)
		       (number-test :type numeric :default 0)
		       (money :type money :default 0)
		       (text :type text :default ""))))

(defun connect-db (&optional (db-name *default-database*)
		     (db-user *database-user*)
		     (db-pwd *database-password*)
		     (host "localhost"))
  (unless *database*
    (setf *database*
	  (connect db-name db-user db-pwd host :pooled-p t))))

(defun disconnect-db (&optional (db-name *database*) (pooled-p nil))
  (if pooled-p
      (clear-connection-pool)
      (disconnect db-name)))
