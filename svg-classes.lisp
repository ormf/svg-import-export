;;; svg-classes.lisp
;;;
;;; class definitions and their print methods.
;;;
;;; **********************************************************************
;;; Copyright (C) 2012-2018 Orm Finnendahl
;;;
;;; This program is free software; you can redistribute it and/or
;;; modify it under the terms of the Lisp Lesser Gnu Public License.
;;; See http://www.cliki.net/LLGPL for the text of this agreement.
;;; **********************************************************************

(in-package #:svg-import-export)

(def-exporting-class svg-file ()
   ((header :accessor header :initarg :header :initform *svg-header* :export t)
    (inverse :accessor inverse :initarg :inverse :initform nil :export t)
    (elements :accessor elements :initarg :elements :initform nil :export t)
    (last-id :accessor last-id :initarg :last-id :initform 1000 :export t)
    (group-ids :accessor group-ids :initarg :group-ids :initform nil :export t)
    (layer-ids :accessor layer-ids :initarg :layer-ids :initform nil :export t)
    (rect-ids :accessor rect-ids :initarg :rect-ids :initform nil :export t)
    (point-ids :accessor point-ids :initarg :point-ids :initform nil :export t)
    (line-ids :accessor line-ids :initarg :line-ids :initform nil :export t)
    (text-ids :accessor text-ids :initarg :text-ids :initform nil :export t)
    (clone-ids :accessor clone-ids :initarg :clone-ids :initform nil :export t)
    )
   )

;; (let ((test (make-instance 'svg-file)))
;;   (slot-value test 'last-id))

(def-exporting-class svg-layer ()
  ((id :accessor id :initarg :id :initform 0 :export t)
   (name :accessor name :initarg :name :initform "Ebene" :export t)))

(def-exporting-class svg-group ()
  ((id :accessor id :initarg :id :initform 0 :export t)))

(def-exporting-class svg-point ()
  ((id :accessor id :initarg :id :initform 0 :export t)
   (cx :accessor cx :initarg :cx :initform 0 :export t)
   (cy :accessor cy :initarg :cy :initform 0 :export t)
   (rx :accessor rx :initarg :rx :initform 10 :export t)
   (ry :accessor ry :initarg :ry :initform 10 :export t)
   (fill-color :accessor fill-color :initarg :fill-color :initform "none" :export t)
   (fill-rule :accessor fill-rule :initarg :fill-rule :initform "evenodd")
   (fill-opacity :accessor fill-opacity :initarg :fill-opacity :initform 1 :export t)
   (stroke-color :accessor stroke-color :initarg :stroke-color :initform "#000000" :export t)
   (stroke-width :accessor stroke-width :initarg :stroke-width :initform 1 :export t)
   (stroke-linecap :accessor stroke-linecap :initarg :stroke-linecap :initform "butt" :export t)
   (stroke-linejoin :accessor stroke-linejoin :initarg :stroke-linejoin :initform "miter" :export t)
   (stroke-miterlimit :accessor stroke-miterlimit :initarg :stroke-miterlimit :initform 4 :export t)
   (stroke-dasharray :accessor stroke-dasharray :initarg :stroke-dasharray :initform "none" :export t)
   (stroke-opacity :accessor stroke-opacity :initarg :stroke-opacity :initform 1 :export t)))

(def-exporting-class svg-line ()
  ((id :accessor id :initarg :id :initform 0 :export t)
  (x1 :accessor x1 :initarg :x1 :initform 0 :export t)
  (y1 :accessor y1 :initarg :y1 :initform 0 :export t)
  (x2 :accessor x2 :initarg :x2 :initform 10 :export t)
  (y2 :accessor y2 :initarg :y2 :initform 0 :export t)
  (fill-color :accessor fill-color :initarg :fill-color :initform "none" :export t)
  (fill-rule :accessor fill-rule :initarg :fill-rule :initform "evenodd" :export t)
  (fill-opacity :accessor fill-opacity :initarg :fill-opacity :initform 1 :export t)
  (stroke-color :accessor stroke-color :initarg :stroke-color :initform "#000000" :export t)
  (stroke-width :accessor stroke-width :initarg :stroke-width :initform 1 :export t)
  (stroke-linecap :accessor stroke-linecap :initarg :stroke-linecap :initform "butt" :export t)
  (stroke-linejoin :accessor stroke-linejoin :initarg :stroke-linejoin :initform "miter" :export t)
  (marker-end :accessor marker-end :initarg :marker-end :initform nil :export t)
  (stroke-opacity :accessor stroke-opacity :initarg :stroke-opacity :initform 1 :export t)))

(def-exporting-class svg-rect ()
  ((id :accessor id :initarg :id :initform 0 :export t)
  (x :accessor x :initarg :x :initform 0 :export t)
  (y :accessor y :initarg :y :initform 0 :export t)
  (width :accessor width :initarg :width :initform 100 :export t)  
  (height :accessor height :initarg :height :initform 100 :export t)
  (stroke-color :accessor stroke-color :initarg :stroke-color :initform "#000000" :export t)
  (stroke-width :accessor stroke-width :initarg :stroke-width :initform 1 :export t)
  (stroke-linecap :accessor stroke-linecap :initarg :stroke-linecap :initform "butt" :export t)
  (stroke-linejoin :accessor stroke-linejoin :initarg :stroke-linejoin :initform "miter" :export t)
  (stroke-opacity :accessor stroke-opacity :initarg :stroke-opacity :initform 1 :export t)
  (fill-color :accessor fill-color :initarg :fill-color :initform "none" :export t)
  (fill-rule :accessor fill-rule :initarg :fill-rule :initform "evenodd" :export t)
  (fill-opacity :accessor fill-opacity :initarg :fill-opacity :initform 1 :export t)
  ))

(def-exporting-class svg-text ()
  ((id :accessor id :initarg :id :initform 0 :export t)
   (label :accessor label :initarg :label :initform "" :export t)
   (y :accessor y :initarg :y :initform 0 :export t)
   (x :accessor x :initarg :x :initform 0 :export t)
   (font-size :accessor font-size :initarg :font-size :initform 12 :export t)
   (font-style :accessor font-style :initarg :font-style :initform "normal" :export t)
   (font-weight :accessor font-weight :initarg :font-weight :initform "normal" :export t)
   (fill-color :accessor fill-color :initarg :fill-color :initform "#000000" :export t)
   (fill-opacity :accessor fill-opacity :initarg :fill-opacity :initform 1 :export t)
   (stroke-color :accessor stroke-color :initarg :stroke-color :initform "none" :export t)
   (stroke-width :accessor stroke-width :initarg :stroke-width :initform 1 :export t)
   (stroke-linecap :accessor stroke-linecap :initarg :stroke-linecap :initform "butt" :export t)
   (stroke-linejoin :accessor stroke-linejoin :initarg :stroke-linejoin :initform "miter" :export t)
   (stroke-opacity :accessor stroke-opacity :initarg :stroke-opacity :initform 1 :export t)
   (font-family :accessor font-family :initarg :font-family :initform "Bitstream Vera Sans" :export t)
  ))

(def-exporting-class svg-clone ()
  (
(id :accessor id :initarg :id :initform 0 :export t)
(href :accessor href :initarg :href :initform "" :export t)
(transform :accessor transform :initarg :transform :initform "matrix(1,0,0,1,0,0)" :export t)))

;; (make-class-args '(id href transform))



;; helper function to avoid too much typing on class definitions ;-)

(defun make-class-args (liste)
  (format t "~%")
  (loop for arg in liste
     do (format t "(~a :accessor ~a :initarg :~a :initform \"\")~%" arg arg arg)))


;; (make-class-args '("id" "y" "x" "font-size" "font-style" "font-weight" "fill" "fill-opacity" ))

;;;   scratch to be implemented:
;;;
;;;
;;;   <g
;;;      inkscape:label="Ebene 1"
;;;      inkscape:groupmode="layer"
;;;      id="layer1">

;;;     <g
;;;        id="g3179">

;; (defconstant +alphabet+ '(
;; "a" "b" "c" "d" "e" "f" "g" "h" "i" "j" "k" "l" "m" "n" "o" "p" "q" "r" "s" "t" "u" "v" "w" "x" "y" "z"
;; ))


(defmacro format-with-slots (stream obj string &rest slots)
  `(let ((ob ,obj))
     (apply #'format 
         (append (list ,stream ,string) 
                 (loop for x in ',slots collect (funcall x ob))))))

(defgeneric print-to-stream (obj stream))

(defmethod print-to-stream ((obj svg-text) stream)
  (format-with-slots stream obj
"<text
         id=\"text~a\"
         y=\"~a\"
         x=\"~a\"
         style=\"font-size:~apx;font-style:~a;font-weight:~a;fill:~a;fill-opacity:~a;stroke:~a;stroke-width:~apx;stroke-linecap:~a;stroke-linejoin:~a;stroke-opacity:~a;font-family:~a\"
         xml:space=\"preserve\"><tspan
           y=\"~a\"
           x=\"~a\"
           id=\"tspan~a\"
           sodipodi:role=\"line\">~a</tspan>
      </text>"
id y x font-size font-style font-weight fill-color fill-opacity stroke-color stroke-width stroke-linecap stroke-linejoin stroke-opacity font-family y x id label))

;; (print-to-stream (make-instance 'svg-text :label "Testtext" :x 100 :y 117) t)


(defmethod print-to-stream ((obj svg-rect) stream)
  (format-with-slots stream obj
      "<rect
         y=\"~a\"
         x=\"~a\"
         height=\"~a\"
         width=\"~a\"
         id=\"rect~a\"
         style=\"fill:~a;fill-rule:~a;stroke:~a;stroke-width:~apx;stroke-linecap:~a;stroke-linejoin:~a;stroke-opacity:~a;opacity:~a\" />"
y x height width id fill-color fill-rule stroke-color stroke-width stroke-linecap stroke-linejoin stroke-opacity fill-opacity))

;; (print-to-stream (make-instance 'svg-rect :width 117 :height 139 :x 100 :y 117) nil)



(defmethod print-to-stream ((obj svg-line) stream)
  (if (marker-end obj)
      (format-with-slots stream obj
                         "    <path
       style=\"fill:~a;fill-opacity:~a;fill-rule:~a;stroke:~a;stroke-width:~apx;stroke-linecap:~a;stroke-linejoin:~a;stroke-opacity:~a;marker-end:url(#Arrow1Lend)\"
       d=\"M ~a,~a L ~a,~a\"
       id=\"path~a\" />
"
                         fill-color fill-opacity fill-rule stroke-color stroke-width stroke-linecap stroke-linejoin stroke-opacity
                         x1 y1 x2 y2 id )
      (format-with-slots stream obj
                         "    <path
       style=\"fill:~a;fill-opacity:~a;fill-rule:~a;stroke:~a;stroke-width:~apx;stroke-linecap:~a;stroke-linejoin:~a;stroke-opacity:~a\"
       d=\"M ~a,~a L ~a,~a\"
       id=\"path~a\" />
"
                         fill-color fill-opacity fill-rule stroke-color stroke-width stroke-linecap stroke-linejoin stroke-opacity
                         x1 y1 x2 y2 id ))
)

;; (print-to-stream (make-instance 'svg-line :x1 :y1 :x2 :y2 :marker-end t) nil)

(defmethod print-to-stream ((obj svg-point) stream)
      (format-with-slots stream obj
                         "    <path
       sodipodi:type=\"arc\"
       style=\"opacity:1;fill:~a;fill-opacity:~a;fill-rule:~a;stroke:~a;stroke-width:~a;stroke-linecap:~a;stroke-linejoin:~a;stroke-miterlimit:~a;stroke-dasharray:~a;stroke-opacity:~a\"
       id=\"path~a\"
       sodipodi:cx=\"~a\"
       sodipodi:cy=\"~a\"
       sodipodi:rx=\"~a\"
       sodipodi:ry=\"~a\"
       d=\"M 1,0 A 1,1 0 1 1 -1,1.2246064e-16 A 1,1 0 1 1 1,-2.4492127e-16 z\"
       sodipodi:open=\"false\"
       />
"
                         fill-color fill-opacity fill-rule stroke-color stroke-width stroke-linecap stroke-linejoin stroke-miterlimit stroke-dasharray
                         stroke-opacity id
                         cx cy rx ry)
)

(defmethod print-to-stream ((obj svg-clone) stream)
  (format-with-slots stream obj
"    <use
         x=\"0\"
         y=\"0\"
         xlink:href=\"#~a\"
         id=\"use~a\"
         transform=\"~a\"
         height=\"40\"
         width=\"40\" />
"
href id transform))


(defgeneric print-head-to-stream (obj stream))
(defgeneric print-tail-to-stream (obj stream))

(defmethod print-head-to-stream ((obj svg-file) stream)
  (if (inverse obj)
      (format stream (header obj) "000000" "1.0")
      (format stream (header obj) "FFFFFF" "0.0")))

(defmethod print-head-to-stream ((obj svg-layer) stream)
  (format-with-slots stream obj
"  <g
     inkscape:label=\"~a\"
     inkscape:groupmode=\"layer\"
     id=\"layer~a\">~%" name id ))

;; (print-head-to-stream (make-instance 'svg-layer) t)

(defmethod print-head-to-stream ((obj svg-group) stream)
  (format-with-slots stream obj
    "<g~%id=\"g~a\">~%" id ))

;; (print-head-to-stream (make-instance 'svg-group) t)

(defmethod print-tail-to-stream ((obj svg-layer) stream)
  (format stream "  </g>~%"))

(defmethod print-tail-to-stream ((obj svg-group) stream)
  (format stream "  </g>~%"))

(defmethod print-tail-to-stream ((obj svg-file) stream)
  (format stream "  </svg>~%"))
