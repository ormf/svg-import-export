;;;; svg-export.lisp

(in-package #:svg-export)

;;; "svg-export" goes here. Hacks and glory await!

(setf *print-case* :downcase)

(defmacro def-exporting-class (name (&rest superclasses) (&rest slot-specs)
                               &optional class-option)
  (let ((exports (mapcan (lambda (spec)
                           (when (getf (cdr spec) :export)
                             (let ((name (or (getf (cdr spec) :accessor)
                                             (getf (cdr spec) :reader)
                                             (getf (cdr spec) :writer))))
                               (when name (list name)))))
                         slot-specs)))
    `(progn
       (defclass ,name (,@superclasses)
         ,(append 
	   (mapcar (lambda (spec)
		     (let ((export-pos (position :export spec)))
		       (if export-pos
			   (append (subseq spec 0 export-pos)
				   (subseq spec (+ 2 export-pos)))
			   spec)))
		   slot-specs)
	   (when class-option (list class-option))))
       ,@(mapcar (lambda (name) `(export ',name))
                 exports))))

;; (defmacro def-exporting-class (name (&rest superclasses) (&rest slot-specs)
;;                                &optional class-option)
;;   (let ((exports (mapcan (lambda (spec)
;;                            (when (getf (cdr spec) :export)
;;                              (let ((name (or (getf (cdr spec) :accessor)
;;                                              (getf (cdr spec) :reader)
;;                                              (getf (cdr spec) :writer))))
;;                                (when name (list name)))))
;;                          slot-specs)))
;;     `(progn
;;        (defclass ,name (,@superclasses)
;;          ,(append `(,@(mapcar (lambda (spec)
;; 			       (let ((export-pos (position :export spec)))
;; 				 (if export-pos
;; 				     (append (subseq spec 0 export-pos)
;; 					     (subseq spec (+ 2 export-pos)))
;; 				     spec)))
;; 			      slot-specs))
;; 		  (if class-option (list class-option))))
;;        ,@(mapcar (lambda (name) `(export ',name))
;;                  exports))))










(defparameter *svg-header* "<?xml version=\"1.0\" encoding=\"UTF-8\" standalone=\"no\"?>
<!-- Created with Inkscape (http://www.inkscape.org/) -->
<svg
   xmlns:dc=\"http://purl.org/dc/elements/1.1/\"
   xmlns:cc=\"http://web.resource.org/cc/\"
   xmlns:rdf=\"http://www.w3.org/1999/02/22-rdf-syntax-ns#\"
   xmlns:svg=\"http://www.w3.org/2000/svg\"
   xmlns:xlink=\"http://www.w3.org/1999/xlink\"
   xmlns=\"http://www.w3.org/2000/svg\"
   xmlns:sodipodi=\"http://sodipodi.sourceforge.net/DTD/sodipodi-0.dtd\"
   xmlns:inkscape=\"http://www.inkscape.org/namespaces/inkscape\"
   width=\"297mm\"
   height=\"210mm\"
   id=\"svg2392\"
   sodipodi:version=\"0.32\"
   inkscape:version=\"0.45.1\"
   sodipodi:docbase=\"/tmp\"
   sodipodi:docname=\"Zeichnung.svg\"
   inkscape:output_extension=\"org.inkscape.output.svg.inkscape\">
  <defs
     id=\"defs2394\">
    <marker
       inkscape:stockid=\"Arrow1Lend\"
       orient=\"auto\"
       refY=\"0.0\"
       refX=\"0.0\"
       id=\"Arrow1Lend\"
       style=\"overflow:visible;\">
      <path
         id=\"path3225\"
         d=\"M 0.0,0.0 L 5.0,-5.0 L -12.5,0.0 L 5.0,5.0 L 0.0,0.0 z \"
         style=\"fill-rule:evenodd;stroke:#000000;stroke-width:1.0pt;marker-start:none;\"
         transform=\"scale(0.8) rotate(180) translate(12.5,0)\" />
    </marker>
    <inkscape:perspective
       sodipodi:type=\"inkscape:persp3d\"
       inkscape:vp_x=\"0 : 372.04724 : 1\"
       inkscape:vp_y=\"0 : 1000 : 0\"
       inkscape:vp_z=\"1052.3622 : 372.04724 : 1\"
       inkscape:persp3d-origin=\"526.18109 : 248.03149 : 1\"
       id=\"perspective2445\" />
  </defs>
  <sodipodi:namedview
     id=\"base\"
     pagecolor=\"#~a\"
     bordercolor=\"#666666\"
     borderopacity=\"1.0\"
     inkscape:pageopacity=\"~f\"
     inkscape:pageshadow=\"2\"
     inkscape:zoom=\"1.4\"
     inkscape:cx=\"347.18919\"
     inkscape:cy=\"708.98923\"
     inkscape:document-units=\"px\"
     inkscape:current-layer=\"layer1\"
     inkscape:window-width=\"1400\"
     inkscape:window-height=\"994\"
     inkscape:window-x=\"0\"
     inkscape:window-y=\"25\" />
  <metadata
     id=\"metadata2397\">
    <rdf:RDF>
      <cc:Work
         rdf:about=\"\">
        <dc:format>image/svg+xml</dc:format>
        <dc:type
           rdf:resource=\"http://purl.org/dc/dcmitype/StillImage\" />
      </cc:Work>
    </rdf:RDF>
  </metadata>
")


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



(defmacro sv (obj slot)
  `(slot-value ,obj ,slot))

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
     id=\"layer~a\">
" name id ))

;; (print-head-to-stream (make-instance 'svg-layer) t)

(defmethod print-head-to-stream ((obj svg-group) stream)
  (format-with-slots stream obj
    "<g
       id=\"g~a\">
" id ))

;; (print-head-to-stream (make-instance 'svg-group) t)

(defmethod print-tail-to-stream ((obj svg-layer) stream)
  (format stream "  </g>
"))

(defmethod print-tail-to-stream ((obj svg-group) stream)
  (format stream "  </g>
"))

(defmethod print-tail-to-stream ((obj svg-file) stream)
  (format stream "  </svg>
"))

;; (defgeneric initialize-instance (obj svg-file &rest data))

(defun new-id (svg-file id-type &key (id nil))
  (let ((curr (slot-value svg-file 'last-id)))
    (setf (slot-value svg-file id-type) 
	  (cons
	   (if (and (not (numberp id)) id) 
	       (if (member id (slot-value svg-file id-type) :test #'string=)
		   (let ((curr 1))
		     (loop
			while (member (format nil "~a-~d" id curr)
				      (slot-value svg-file id-type) :test #'string=)
			do (incf curr)
			finally (return (format nil "~a-~d" id curr))))
		   id)
	       (loop
		  do (incf curr)
		  while (member curr (slot-value svg-file id-type))
		  finally (return curr)))
	   (slot-value svg-file id-type)))
  (if id id (setf (slot-value svg-file 'last-id) curr))))


;; (let ((test (make-instance 'svg-file)))
;;     (new-id test 'group-ids :id "gen3")
;;     (new-id test 'clone-ids)
;;     (new-id test 'group-ids :id "gen2")
;;     (new-id test 'group-ids :id "gen2")
;;     (list
;;      (slot-value test 'group-ids)
;;      (slot-value test 'clone-ids)
;;      (slot-value test 'last-id))
;;     (slot-value test 'group-ids))


(defun print-elements-to-stream (elems stream)
  (cond ((null elems) nil)
        ((listp (car elems))
         (progn
           (print-head-to-stream (caar elems) stream)
           (print-elements-to-stream (cdar elems) stream)
           (print-tail-to-stream (caar elems) stream)
           (print-elements-to-stream (cdr elems) stream)))
        (t (progn
           (print-to-stream (car elems) stream)
           (print-elements-to-stream (cdr elems) stream)))))

(defun export-svg-file (svg-file &key (fname "/tmp/test.svg"))
  (with-open-file (outstream fname :direction :output :if-exists :supersede)
    (print-head-to-stream svg-file outstream)
    (print-elements-to-stream (elements svg-file) outstream)
    (print-tail-to-stream svg-file outstream)))

;; (setf *test*
;;       (let ((svg-file (make-instance 'svg-file)))
;;         (setf (slot-value svg-file 'elements) 
;;               (list
;;                (append (list (make-instance 'svg-layer :name "Ebene 1" :id (new-id svg-file 'layer-ids)))
;;                        (loop for x in '(0 10 20 30 40)
;;                           collect (make-instance 'svg-rect :x x :y 200 :width 100 :height 100 :id (new-id svg-file 'rect-ids))))))
;;               svg-file))
;; (elements *test*)
;; (export-svg-file *test*)


;;  (setf *test*
;;        (let ((svg-file (make-instance 'svg-file)))
;;          (setf (slot-value svg-file 'elements) 
;;                (list
;;                 (append (list (make-instance 'svg-layer :name "Ebene 1" :id (new-id svg-file 'layer-ids)))
;;                         (loop for x in '(0 10 20 30 40)
;;                            collect (make-instance 'svg-rect :x x :y 200 :width 100 :height 100 :fill-color "#333333"
;;                                                   :id (new-id svg-file 'rect-ids))))))
;;                svg-file))
;; (elements *test*)
;; (export-svg-file *test*)


;; (progn
;;   (setf *test*
;;         (let ((svg-file (make-instance 'svg-file))
;;               (mins t) (halfmins t) (tensecs t) (fivesecs t) (secs t)
;;               (xscale 5)
;;               (yscale 5))
;;           (setf (slot-value svg-file 'elements) 
;;                 (list
;;                  (append (list (make-instance 'svg-layer :name "Zeitbeschriftung" :id (new-id svg-file 'layer-ids)))
;;                          (loop for time from 0 to 900 by 30
;;                             append
;;                             (cond
;;                               ((and (zerop (mod time 60)) mins)
;;                                (list
;;                                 (make-instance 'svg-text 
;;                                                :x (* xscale time)
;;                                                :y (* -1 xscale 20)
;;                                                :label (format nil "~d.00" (floor time 60)
;;                                                               :id (new-id svg-file 'text-ids)))))
;;                               ((and (zerop (mod time 30)) halfmins)
;;                                (list
;;                                 (make-instance 'svg-text 
;;                                                :x (* xscale time)
;;                                                :y (* -1 xscale 20)
;;                                                :label (format nil "~d.30" (floor time 60)
;;                                                               :id (new-id svg-file 'text-ids)))))
;;                               (t nil))))
;;                  (append (list (make-instance 'svg-layer :name "Zeitachse" :id (new-id svg-file 'layer-ids)))
;;                          (loop for time from 0 to 900
;;                             append
;;                             (cond
;;                               ((and (zerop (mod time 60)) mins)
;;                                (list
;;                                 (make-instance 'svg-line :x1 (* xscale time) :x2 (* xscale time) 
;;                                                :y1 (* -1 xscale 10) :y2 (* -1 xscale 20) 
;;                                                :id (new-id svg-file 'line-ids))))
;;                               ((and (zerop (mod time 30)) halfmins)
;;                                (list
;;                                 (make-instance 'svg-line :x1 (* xscale time) :x2 (* xscale time) 
;;                                                :y1 (* -1 xscale 10) :y2 (* -1 xscale 17.5) 
;;                                                :id (new-id svg-file 'line-ids))))
;;                               ((and (zerop (mod time 10)) tensecs)
;;                                (list
;;                                 (make-instance 'svg-line :x1 (* xscale time) :x2 (* xscale time) 
;;                                                :y1 (* -1 xscale 10) :y2 (* -1 xscale 15) 
;;                                                :id (new-id svg-file 'line-ids))))
;;                               ((and (zerop (mod time 5)) fivesecs)
;;                                (list
;;                                 (make-instance 'svg-line :x1 (* xscale time) :x2 (* xscale time) 
;;                                                :y1 (* -1 xscale 10) :y2 (* -1 xscale 12.5) 
;;                                                :id (new-id svg-file 'line-ids))))
;;                               ((and (zerop (mod time 1)) secs)
;;                                (list
;;                                 (make-instance 'svg-line :x1 (* xscale time) :x2 (* xscale time) 
;;                                                :y1 (* -1 xscale 10) :y2 (* -1 xscale 11) 
;;                                                :id (new-id svg-file 'line-ids))))
;;                               (t nil))))
;;                  (append 
;;                   (list (make-instance 'svg-layer :name "Linien" :id (new-id svg-file 'layer-ids)))
;;                   (loop for point in *graph*
;;                      collect
;;                      (destructuring-bind (cx cy) point
;;                        (make-instance 'svg-point :cx (* xscale cx) :cy (* yscale cy) 
;;                                       :rx 1 :ry 1
;;                                       :stroke-width 1
;; ;;                                      :stroke-color (color-lookup color) 
;;                                       :fill-color "#000000"
;;                                       :rx 1
;;                                       :ry 1
;;                                       :id (new-id svg-file 'point-ids)))))))
;;           svg-file))
;;   (export-svg-file *test*)
;;   )


