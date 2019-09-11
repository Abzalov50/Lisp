;;;-*- Mode: Lisp; Encoding: (win32:code-page :id 1251) -*-
;;; cl-typesetting copyright 2002 Marc Battyani see license.txt for details of the license
;;; You can reach me at marc.battyani@fractalconcept.com or marc@battyani.net
;;;
;;; Version for Windows-1251 encoding (cyrillic) on LWW 4.4.6
;;; by Dmitry Ivanov (http://lisp.ystok.ru)

(in-package typeset)

#|
(pdf:load-fonts)
(setq cmti10 (pdf:load-t1-font (lw:current-pathname "../files-for-example/cmti10.afm")
                               (lw:current-pathname "../files-for-example/cmti10.pfb"))
      cmex10 (pdf:load-t1-font (lw:current-pathname "../files-for-example/cmex10.afm")
                               (lw:current-pathname "../files-for-example/cmex10.pfb")))

(pushnew (lw:pathname-location (lw:current-pathname "../../cl-pdf/afm/cyr/"))
          pdf:*afm-files-directories*
         :test #'equal)
;(setq ari1251 (pdf:load-t1-font (lw:current-pathname "Arial-1251.afm")
;                                (lw:current-pathname "Arial-1251.pfb"))
;      (pdf:encoding-scheme ari1251) :win-1251-encoding)
(setq ari1251 (pdf:require-font "Arial-1251" :type1 :encoding :win-1251-encoding))
|#

;(defparameter *boxes* nil "for debugging...")

(defun my-get-text () "Это моя строка")

(defun hello (&optional (file #P"/tmp/hello.pdf"))
  (pdf:with-document ()
    (pdf:with-page ()
      (pdf:with-outline-level ("Example" (pdf:register-page-reference))
	(pdf:set-line-width 0.1)
	(let*((*default-font* (pdf:get-font "Arial-1251" :win-1251-encoding))
              (content
	       (compile-text ()
			     (vspace 100)
		 (paragraph (:h-align :centered :font "Helvetica-Bold" :font-size 50
                             :color '(0.0 0 0.8))
			    "cl-typesetting" :eol
			    (vspace 2)
			    (hrule :dy 1)
			    (with-style (:font "Times-Italic" :font-size 26)
			      "The cool Common Lisp typesetting system")
			    (vspace 50)
			    (with-style (:font "Helvetica-Oblique" :font-size 100)
			      "Hello World!") :eol)
		 (paragraph (:font-size 12)
			    (verbatim
"(defmethod stroke ((box char-box) x y)
  (pdf:in-text-mode
   (pdf:move-text x (+ y (offset box)))
   (pdf:set-text-x-scale (* *text-x-scale* 100))
   (pdf:show-char (boxed-char box))))")
                            :eol
                            (verbatim (my-get-text))
                            ))))
	  (draw-block content 20 800 545 700))))
    (pdf:write-document file)))


;; The Fancy Example!

(defparameter *par1*
  "Lisp is a family of languages with a long history. Early key ideas in Lisp were developed by John McCarthy during the 1956 Dartmouth Summer Research Project on Artificial Intelligence. McCarthy's motivation was to develop an algebraic list processing language for artificial intelligence work. Implementation efforts for early dialects of Lisp were undertaken on the IBM 704, the IBM 7090, the Digital Equipment Corporation (DEC) PDP-1, the DEC PDP-6, and the PDP-10. The primary dialect of Lisp between 1960 and 1965 was Lisp 1.5. By the early 1970's there were two predominant dialects of Lisp, both arising from these early efforts: MacLisp and Interlisp. For further information about very early Lisp dialects, see The Anatomy of Lisp or Lisp 1.5 Programmer's Manual.")

(defparameter *par1-cyrillic-1251*
  "Лисп - универсальный язык программирования, изобретенный Джоном Маккарти в 1959 году. Цитата из \"Lisp 1.5 Programmers Manual\", опубликованного в 1960 году, гласит: \"это был очень специализированный язык, в котором программный код всегда представлялся в виде данных, а данные могли служить кодом.\" Благодаря стандарту Коммон Лиспа ANSI Common Lisp, опубликованному в 1994 году, он получил широкое распространение.")

(defparameter *par2*
  "MacLisp improved on the Lisp 1.5 notion of special variables and error handling. MacLisp also introduced the concept of functions that could take a variable number of arguments, macros, arrays, non-local dynamic exits, fast arithmetic, the first good Lisp compiler, and an emphasis on execution speed. By the end of the 1970's, MacLisp was in use at over 50 sites. For further information about Maclisp, see Maclisp Reference Manual, Revision 0 or The Revised Maclisp Manual.")

;; example of extension

(defclass rotated-char-box (soft-box h-mode-mixin)
  ((boxed-char :accessor boxed-char :initarg :boxed-char)
   (rotation :accessor rotation :initarg :rotation)))

(defun put-rotated-char-string (string)
  (loop for char across string
	do (add-box (make-instance 'rotated-char-box :dx *font-size*
				   :dy *font-size* :boxed-char char :baseline (* *font-size* 0.8)
				   :rotation (- (random 120) 60)))))

(defmethod stroke ((box rotated-char-box) x y)
  (let ((dx (dx box))(dy (dy box))
	(width (pdf:get-char-width (boxed-char box) *font* *font-size*)))
    (pdf:with-saved-state
      (pdf:translate (+ x (* dx 0.5)) (+ y (* dy 0.3)))
      (pdf:set-line-width 0.5)
      (pdf:set-gray-fill 0.8)
      (pdf:circle 0 0 (* dx 0.45))
      (pdf:fill-and-stroke)
      (pdf:set-gray-fill 0)
      (pdf:rotate (rotation box))
      (pdf:in-text-mode
       (pdf:move-text (* -0.5 width)(* -0.18 *font-size*))
       (pdf:set-font *font* (* *font-size* 0.8))
       (pdf:show-text (make-string 1 :initial-element (boxed-char box)))))))

;; a draw function for the functional rule...

(defun draw-wavelet-rule (box x0 y0)
  (let ((dx/2 (* (dx box) 0.5))
	(dy/2 (* (dy box) 0.5)))
    (pdf:with-saved-state
      (pdf:translate (+ x0 dx/2) (- y0 dy/2))
      (pdf:set-line-width 1)
      (pdf:set-color-stroke (color box))
      (pdf:move-to (- dx/2) 0)
      (loop for x from (- dx/2) by 0.2
	    for y = (* dy/2 (cos (* x 0.8)) (exp (* x x -0.006)))
	    while (< x dx/2)
	    do (pdf:line-to x y))
      (pdf:stroke))))

;; user-drawn box

(defun user-drawn-demo (box x y)
  (draw-block (compile-text ()
			    (paragraph (:h-align :justified :top-margin 5 :first-line-indent 10
						 :font "Times-Italic" :font-size 6.5)
				       *par1*))
	      x (- y (dy box)) (- (dy box) 10) (dx box)))

;; a chart (I will have to change this in cl-pdf: it's a real mess!)

(defun draw-pie (box x y)
  (pdf:draw-object (make-instance
		    'pdf:pie-chart :x (+ x 30) :y (- y 100) :width 90 :height 90
		    :serie '(12 23 65 33)
		    :labels&colors
		    '(("Winter" (1.0 0.0 0.0))
		      ("Spring" (0.0 1.0 0.0))
		      ("Summer" (0.0 0.0 1.0))
		      ("Autumn" (0.0 1.0 1.0))))))

;; a stupid trick
;; brute force!!!
(defun link-all-a (box x y)
  (pdf:with-saved-state
      (let ((all-a ()))
	(map-boxes box 0 0 #'(lambda (box x y)
			       (when (and (char-box-p box) (char= (boxed-char box)#\a))
				 (push (list (+ x (* 0.5 (dx box)))(+ y (* 0.2 (dy box))) box) all-a))))
	(pdf:set-line-width 1)
	(pdf:set-rgb-stroke 1.0 0.8 0.4)
	(loop for (x y box) in all-a
	      for sorted-a = (sort (copy-seq all-a)
				   #'(lambda (item1 item2)
				       (let ((dx1 (- (first item1) x))
					     (dy1 (- (second item1) y))
					     (dx2 (- (first item2) x))
					     (dy2 (- (second item2) y)))
					 (<= (sqrt (+ (* dx1 dx1)(* dy1 dy1)))(sqrt (+ (* dx2 dx2)(* dy2 dy2)))))))
	      do (loop repeat 4
		   for (x2 y2 box) in sorted-a
		   do
	         (pdf:set-gray-fill 0.8)
		   (pdf::move-to x y)
		   (pdf::line-to x2 y2)
		   (pdf::stroke)))
	(pdf:set-gray-fill 0.8)
	(pdf:set-rgb-stroke 0.5 0.7 1.0)
	(loop for (x y box) in all-a
	      do
	      (pdf:circle x y (* (dx box) 0.7))
	      (pdf:fill-and-stroke)))))

;;; rivers detection (still brute force not to be used in real life...)
(defun link-all-spaces (box x y)
  (pdf:with-saved-state
      (let ((all-spaces ()))
	(map-boxes box 0 0 #'(lambda (box x y)
			       (when (white-char-box-p box)
				 (push (list (+ x (* 0.5 (dx box)))(+ y (* 0.5 (dx box))) box) all-spaces))))
	(pdf:set-line-width 1)
	(pdf:set-rgb-stroke 1.0 0.4 0)
	(loop for (x y box) in all-spaces
	      for sorted-spaces = (sort (copy-seq all-spaces)
					#'(lambda (item1 item2)
					    (let ((dx1 (- (first item1) x))
						  (dx2 (- (first item2) x)))
					      (<= (abs dx1)(abs dx2)))))
	      do (loop repeat 5
		   for (x2 y2 box) in sorted-spaces
		   for dy = (abs (- y2 y))
		   for dx = (abs (- x2 x))
		   do
		   (when (and (< dx (* 1.5 (+ (dx box)(delta-size box)))) (< 0 dy (* 5 (dx box))))
		     (pdf:set-gray-fill 0.8)
		     (pdf::move-to x y)
		     (pdf::line-to x2 y2)
		     (pdf::stroke)
		     (pdf:circle x y (* (dx box) 0.7))
		     (pdf:circle x2 y2 (* (dx box) 0.7))
		     (pdf:fill-and-stroke)))))))

(defun link-all-a-and-spaces (box x y)
  (link-all-a box x y)
  (link-all-spaces box x y))
  
;;example document

(defun ex-1251 (&optional (file #P"/tmp/ex.pdf")
	 	          (banner #P"/tmp/banner.jpg") (fractal #P"/tmp/fractal.jpg"))
  (pdf:with-document ()
   (let* ((*default-font* (pdf:get-font "Helvetica"))
          (*font* *default-font*)
          (pdf::*default-chart-font* *default-font*))
    (pdf:with-page ()
      (pdf:with-outline-level ("Example" (pdf:register-page-reference))
	(pdf:set-line-width 0.1)
	(let ((content
	       (compile-text ()
		 (paragraph (:h-align :centered :font "Helvetica-Bold" :font-size 30 :color '(0.0 0 0.8))
			    "cl-typesetting" :eol
			    (vspace 2)
			    (hrule :dy 1)
			    (with-style (:font "Times-Italic" :font-size 13)
			      "The cool Common Lisp typesetting system"))
		 (paragraph (:h-align :justified :top-margin 10 :first-line-indent 10
				      :font "Times-Italic" :font-size 10)
			      "This typesetting system's goal is to be an alternative to the TeX typesetting system. It is written in Common Lisp and uses cl-pdf as its backend. This will enable it to be powerful, extensible and fast. Though it is not considered very difficult, it is already better than Word...")
		 (paragraph (:h-align :centered :font "Helvetica-BoldOblique" :font-size 20 :color '(1.0 0 0))
			    "Now in Color! "
			    (colored-box :dx 15.0 :dy 15.0 :color "#FFC0C0" :border-width 0.5) " "
			    (colored-box :dx 15.0 :dy 15.0 :color "#C0FFC0" :border-width 0.5) " "
			    (colored-box :dx 15.0 :dy 15.0 :color "#C0C0FF" :border-width 0.5))
		 (paragraph (:h-align :centered :font "Times-Italic" :font-size 12 :color '(0.0 0.6 0.3))
			    "With user defined "
			    (put-rotated-char-string "extensions") :eol
			    (with-style (:font "Times-Italic" :font-size 11)
			      "Support for images and functional rules" :eol
			      (image :file banner :dx 100 :dy 20)))
		 (hrule :dy 15 :stroke-fn 'draw-wavelet-rule)
		 (vspace 3)
		 (table (:col-widths '(60 80 80) :border 0.5 :background-color '(1 1 0.8)
				     :cell-padding 1 :padding 2)
			(row ()
			     (cell (:background-color '(0.8 1 0.8) :col-span 3)
				   (paragraph (:h-align :centered :font "Times-Italic" :font-size 12)
						"Title with a col-span of 3")))
			(row ()
			     (cell (:background-color '(0.8 0.8 0.8))
				   (paragraph (:h-align :left :font "Times-Italic" :font-size 9)
						"Left aligned"))
			     (cell (:background-color '(0.8 0.8 0.8))
				   (paragraph (:h-align :centered :font "Times-Roman" :font-size 9)
						"Centered cell content"))
			     (cell (:background-color '(0.8 0.8 0.8))
				   (paragraph (:h-align :right :font "Times-Bold" :font-size 9)
						"Right cell content")))
			(row ()
			     (cell ()(paragraph (:h-align :left :font "Times-Italic" :font-size 9)
						"This cell content should take three lines."))
			     (cell (:background-color '(1 1 1))
				   (paragraph (:h-align :centered :font "Times-Italic" :font-size 9)
						"A jpeg "
						(image :file fractal :dx 15 :dy 15 :inline t
						       :offset 9)
						" in the text"))
			     (cell ()(paragraph (:h-align :left :font "Times-Italic" :font-size 11)
						(put-rotated-char-string "common lisp is cool"))))
			(row ()
			     (cell ()(paragraph (:h-align :left :font "Times-Italic" :font-size 9)
						"An example of table inside a cell"))
			     (cell (:background-color '(1 1 1))
				   (table (:col-widths '(14 14 21) :border 0.2
							 :background-color '(0.4 0.4 0.8))
					    (row () (cell () "12")(cell () "34")(cell () "567"))
					    (row () (cell () "ab")(cell () "cd")(cell () "efg"))))
			     (cell ()(paragraph (:h-align :left :font "Times-Italic" :font-size 9)
						"You can nest as many tables as you want, like you do in HTML."))))
		 (paragraph (:h-align :justified :top-margin 5 :first-line-indent 10 :color '(0 0 0)
				      :left-margin 5 :right-margin 5 
				      :font "Times-Roman" :font-size 10 :text-x-scale 0.7)
			    (with-style (:color '(0 0.6 0.4))
			      "This paragraph has been horizontally strechted by a 0.7 ratio. ")
			    *par1*)
		 (vspace 10)
		 (user-drawn-box :dx 210 :dy 100 :stroke-fn 'user-drawn-demo) :eol
		 (paragraph (:h-align :centered :font "Times-Italic" :font-size 8 :top-margin 5)
			    "An example of using cl-typesetting in an user-drawn box.")
		 (paragraph (:h-align :left :top-margin 15
				      :left-margin 5 :right-margin 5 :font "courier" :font-size 8)
			    (verbatim
"(defmethod stroke ((box char-box) x y)
  (pdf:in-text-mode
   (pdf:move-text x (+ y (offset box)))
   (pdf:set-font *font* *font-size*)
   (pdf:set-text-x-scale (* *text-x-scale* 100))
   (pdf:show-char (boxed-char box))))"))
		 (paragraph (:h-align :center :font "Times-Italic" :font-size 8 :top-margin 3)
			    "An example of verbatim code.")
		 ;(paragraph (:h-align :justified :top-margin 9 :font "Helvetica-Oblique"
	;			      :left-margin 5 :right-margin 5 
	;			      :font-size 9 :first-line-indent 20)
	;		   *par1*)
		 (paragraph (:h-align :center :top-margin 9
                             :font "Helvetica" :font-size 10)
			    "An example of Cyrillic paragraph")
		 (paragraph (:h-align :justified :top-margin 3
                             :font (pdf:get-font "Arial-1251" :win-1251-encoding)
                             :font-size 9 
                             :left-margin 5 :right-margin 5 
                             :first-line-indent 20)
			   *par1-cyrillic-1251*)
		 (user-drawn-box :dx 240 :dy 100 :stroke-fn 'draw-pie) :eol
		 (paragraph (:h-align :center :font "Times-Italic" :font-size 8)
			    "An example of cl-pdf pie chart inserted.")
		 (paragraph (:h-align :justified :top-margin 9 :font "helvetica" :font-size 9
				      :left-margin 40 :right-margin 40)
			    *par2*)
		 (vspace 10)
		 (paragraph (:h-align :center :top-margin 20 :font "Times-Bold" :font-size 20)
			    "Kerning test" :eol
			    (with-style (:font "Helvetica" :font-size 40 :left-margin 20 :right-margin 20)
			      "Yes, AWAY"))
		 (paragraph (:h-align :center :top-margin 10 :font "CMTI10"
				      :font-size 16 :color '(0 0 0))
			    (with-style (:font "Times-Bold" :font-size 20)
			      "Basic Math Mode Test" :eol)
			    (vspace 5)
			    (display-formula ()
			      (with-style (:font (pdf:get-font "CMEX10" nil) :font-size 30)
				(with-offset (23) "H"))
			     "E"(math-super-and-sub-script () ("n+1") ("k,m"))"="
			     (fraction ()
				       ("x"(with-superscript () "2")"+x-1")
				       ("F(x)+b-3"))
			     "-e"(with-superscript () "-x"(with-superscript () "2")))
			    (vspace 5)
			    (with-style (:font "Times-Roman" :font-size 10)
			      "This test now uses a TeX font (cmti10). Note the italic" :eol "correction for the super/subscript of the E."))
		 #|(paragraph (:h-align :center :top-margin 20 :font "Helvetica"
				      :font-size 40 :color '(0.8 0 0))
			    "Warning!" :eol
			    (with-style (:font "Times-Italic" :font-size 14)
			      "This test pdf file has been made with" :eol "cl-typesetting 0.67" :eol
			      (vspace 10)
			      "Marc Battyani"))|#
		 (paragraph (:h-align :center :top-margin 20
                             :font "Helvetica" :font-size 15
                             :color '(0.8 0 0))
                   (format-string "With cl-pdf version ~a," pdf:*version*)
                   " boxes and tables can look smoother due to rounded corners."
                   :eol
                   "Just specify the " (with-style (:font "courier") (verbatim "raduis"))
                   " argument" :eol
                   "for the " (with-style (:font "courier") (verbatim "table")) " macro"
                   :eol
                   "or the " (with-style (:font "courier") (verbatim "draw-box")) " function."
                   (vspace 10)
                   "Marc Battyani and Dmitry Ivanov")
		 :vfill
		 (hrule :dy 20 :stroke-fn 'draw-wavelet-rule :color '(0.8 0 0))
		 :vfill
		 (paragraph (:h-align :center :font "Helvetica-Oblique" :font-size 8)
			    "This project needs contributors. So if you are interested contact "
			    (with-style (:font "Times-Italic" :font-size 9)
			      "marc.battyani@fractalconcept.com") "."
			    ))))
	  (pdf::draw-bar-code128 "CODE128BARCODE" 10 35 :height 25 :width 150 :start-stop-factor 0.25
				 :font-size 7 :show-string t)
	  (draw-block content 40 800 250 380 :rotation 5 :border 0.1)
	  (draw-block content 50 425 250 380 :rotation -5 :border 0.1)
	  (draw-block content 330 800 250 380 :rotation -2 :border 0.1
                      :special-fn 'link-all-a-and-spaces)
	  (draw-block content 310 400 250 380 :v-align :justified
                      :border 0.1))))
    (pdf:write-document file))))

(defun pdf-rarr (&optional (pathname (lw:current-pathname "pdf-rarr.pdf")))
  (pdf:with-document ()
    (pdf:with-page ()
      (let ((helvetica (pdf:get-font "Helvetica"))
            (symbol-font (pdf:get-font "Symbol" pdf::*symbol-encoding*)))
        (pdf:in-text-mode
          (pdf:set-font helvetica 24)
          (pdf:move-text 20 800)
          (pdf:draw-text "Drawing rarr via CL-PDF: File -> Open")
          (pdf:move-text 0 -50)
          (pdf:set-font helvetica 12)
          (pdf:show-text "File")
          (pdf:set-font symbol-font 12)
          (pdf:show-text (code-char (gethash "arrowright" (pdf::char-codes
                                                           pdf::*symbol-encoding*))))
          (pdf:set-font helvetica 12)
          (pdf:draw-text "Open")
    ) ) )
    (pdf:write-document pathname)))

(defun tt-rarr (&optional (file (lw:current-pathname "tt-rarr.pdf")))
  (pdf:with-document ()
    (pdf:with-page ()
      (let* ((*default-font* (pdf:get-font "Helvetica"))
             (symbol-font (pdf:get-font "Symbol" pdf::*symbol-encoding*))
             (content
              (compile-text ()
                (paragraph (:font-size 24)
                  "Drawing rarr via CL-TYPESETTING: File -> Open")
                (paragraph () ;:font-size 12)
                  "File"
                  (with-style (:font symbol-font)
                    (verbatim (string (code-char (gethash "arrowright"
                                                          (pdf::char-codes
                                                           pdf::*symbol-encoding*))))))
                  "Open"))))
        (draw-block content 20 800 545 700)))
    (pdf:write-document file)))

#||
(setq pdf:*compress-streams* nil)
(hello (lw:current-pathname "hello-1251.pdf"))

(ex-1251 (lw:current-pathname "single-page-1251.pdf")
         (lw:current-pathname "../files-for-example/banner.jpg")
         (lw:current-pathname "../files-for-example/fractal.jpg"))
(pdf-rarr)
(tt-rarr)
||#
