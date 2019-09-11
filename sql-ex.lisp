(clsql:def-view-class employee ()
  ((emplid :db-kind :key
	   :db-constraints :not-null
	   :type integer
	   :initarg :emplid)
   (first-name :accessor first-name
	       :type (string 30)
	       :initarg :first-name)
   (last-name :accessor last-name
	       :type (string 30)
	       :initarg :last-name)
   (email :accessor employee-email
	  :type (string 100)
	  :nulls-ok t
	  :initarg :email)
   (companyid :type integer
	      :initarg companyid)
   (managerid :type integer
	      :nulls-ok t
	      :initarg :managerid)
   (company :reader employee-company
	    :db-kind :join
	    :db-info (:join-class company
				  :home-key companyid
				  :foreign-key companyid
				  :set nil))
   (manager :accessor employee-manager
	    :db-kind :join
	    :db-info (:join-class employee
				  :home-key managerid
				  :foreign-key emplid
				  :set nil)))
  (:base-table employee))

(clsql:def-view-class company ()
  ((companyid :db-kind :key
	      :db-constraints :not-null
	      :type integer
	      :initarg :companyid)
   (name :type (string 100)
	 :accessor company-name
	 :initarg :name)
   (presidentid :type integer
		:initarg :presidentid)
   (employees :reader company-employees
	      :db-kind :join
	      :db-info (:join-class employee
				    :home-key companyid
				    :foreign-key companyid
				    :set t))
   (president :reader president
	      :db-kind :join
	      :db-info (:join-class employee
				    :home-key presidentid
				    :foreign-key emplid
				    :set nil)))
  (:base-table company))

(defvar company1 (make-instance 'company
				:companyid 1
				:presidentid 1
				:name "Widgets Inc."))

(defvar employee1 (make-instance 'employee
				 :emplid 1
				 :first-name "Vladimir"
				 :last-name "Lenin"
				 :email "lenin@soviet.org"
				 :companyid 1))

(defvar employee2 (make-instance 'employee
				 :emplid 2
				 :first-name "Josef"
				 :last-name "Stalin"
				 :email "stalin@soviet.org"
				 :managerid 1
				 :companyid 1))

(clsql:connect '("localhost" "sqlex" "postgres" "Jesuschrist1")
	       :database-type :postgresql)

;; To run once to register tables in the DB
(clsql:create-view-from-class 'employee)
(clsql:create-view-from-class 'company)

;; Save records in the DB
(clsql:update-records-from-instance employee1)
(clsql:update-records-from-instance employee2)
(clsql:update-records-from-instance company1)
