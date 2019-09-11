(defpackage :vitrine
  (:use :cl :cl-who :hunchentoot :parenscript :cl-mongo))
(in-package :vitrine)

;;; Definition of data-structures
(defvar *objects*
  '((user nil)
    (shop nil)
    (user-shop nil)
    (post nil)
    (product nil)
    (address nil)))

(defclass user ()
  ((id :initform 0)
   (first-name :accessor first-name)
   (last-name :accessor first-name)
   (email :accessor email)
   (password :accessor password)
   (phone :accessor phone)
   (address :accessor address)
   (type :accessor type)))  ; professional or single

(defclass shop ()
  ((id :initform 0)
   (name :accessor name)
   (num-pages :accessor num-pages
	      :initform 2)
   (email :accessor email)
   (phone :accessor phone)
   (address :accessor address)
   (facebook-page :accessor facebook-page)))

(defclass post ()
  ((id :initform 0)
   (title :accessor title)
   (contents :accessor contents)
   (price :accessor price)
   (tags :accessor tags)
   (regions :accessor regions)
   (id-user :accessor id-user)
   (id-shop :accessor id-shop)
   (id-product :accessor id-product)))
