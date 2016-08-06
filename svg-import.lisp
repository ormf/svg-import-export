;;;;; svg-import.lisp

(in-package :svg-export)

;;; Matrix transformations:

(defun mtx-mult (m1 m2)
    "multiply two 2x3 matrixes given as lists of the format
     (x11 y11 x12 y12 x13 y13) as in svg transforms."
  (cond ((and m1 m2)
         (destructuring-bind (x11 y11 x12 y12 x13 y13) m1
           (destructuring-bind (x21 y21 x22 y22 x23 y23) m2
             (list
              (+ (* x11 x21) (* x12 y21))   ;;; x31
              (+ (* y11 x21) (* y12 y21))   ;;; y31
              (+ (* x11 x22) (* x12 y22))   ;;; x32
              (+ (* y11 x22) (* y12 y22))   ;;; y32
              (+ (* x11 x23) (* x12 y23) x13) ;;; x33
              (+ (* y11 x23) (* y12 y23) y13) ;;; y33
              ))))
        (m1 m1)
        (m2 m2)
        (t nil)))

;;; svg transformation functions returning the transformation matrix
;;; of their operation

(defun matrix (x1 y1 x2 y2 x3 y3)
  (list x1 y1 x2 y2 x3 y3))

(defun translate (tx ty)
  (list 1 0 0 1 tx ty))

(defun ang->rad (angle)
  (* (mod angle 360) 2/360 pi))

(defun rotate (angle)
  (let ((rad (ang->rad angle)))
    (list (cos rad) (sin rad) (- (sin rad)) (cos rad) 0 0)))

(defun scale (sx sy)
  (list sx 0 0 sy 0 0))

(defun x-skew (angle)
  (list 1 0 (tan angle) 1 0 0))

(defun y-skew (angle)
  (list 1 (tan angle) 0 1 0 0))

(defun skew (x-angle y-angle)
  (list 1 (tan y-angle) (tan x-angle) 1 0 0))
        
;;; parsing of svg transformation string:

(defun split-by-one-space (string)
    "Returns a list of substrings of string
divided by ONE space each.
Note: Two consecutive spaces will be seen as
if there were an empty string between them."
    (loop for i = 0 then (1+ j)
          as j = (position #\Space string :start i)
          collect (subseq string i j)
       while j))

(defun read-op (str)
  "parse a svg transformation string and return the transformation
name and its arguments as a list. Returns nil for empty transformation
string."
  (unless (string= str "")
    (let ((count 0))
      (list
       (read-from-string
        (coerce
         (loop
            for char across str
            until (char= char #\()
            do (incf count)
            collect char)
         'string))
       (read-from-string (substitute #\SPACE #\, (subseq str count)))))))

;;; (read-op "translate(12,27)") -> (translate (12 27))
;;; (read-op "") -> nil

(defun get-transformation-mtx (transforms)
  "returns the 2x3 matrix of the supplied svg transformation string
(a series of transformations separated by a single space). The
transformations are executed from left to right."
  (reduce
   #'mtx-mult
   (reverse
    (loop for transformation in
         (split-by-one-space transforms)
       collect (destructuring-bind (fn args)
                   (read-op transformation)
                 (apply (symbol-function fn) args))))))

#|
(get-transformation-mtx
 "translate(196.02814,156.95225) matrix(0.9330498,0,0,2.8503305,0.24844852,-155.11236)") (0.9330498 0.0 0.0 2.8503306 183.15245 292.25345)
|#

(defun vec-mtx-mult (vec mtx)
  "multiply a 2-dimensional vector with a 2x3 matrix"
  (if mtx
      (destructuring-bind (x1 y1 x2 y2 x3 y3) mtx
        (destructuring-bind (x y) vec
          (list
           (+ (* x x1) (* y x2) x3)
           (+ (* x y1) (* y y2) y3))))
      vec))

#|
(vec-mtx-mult
 '(0 0)
 (get-transformation-mtx
  "translate(2,4) rotate(90)"))

(vec-mtx-mult
 '(0 0)
 (get-transformation-mtx
  "rotate(90) translate(2,4)"))

(vec-mtx-mult
 '(0 0)
 (get-transformation-mtx
  "rotate(3.14159)"))

(get-transformation-mtx
 "translate(196.02814,156.95225) matrix(0.9330498,0,0,2.8503305,0.24844852,-155.11236)")


(get-transformation-mtx
 "translate(2,4) rotate(3.14159)")


(get-transformation-mtx
 "rotate(3.14159) translate(2,4)")
|#

;;;
;;; xml parsing:
;;;

(defun get-layer (layer-local-name xml &key (URI "http://www.inkscape.org/namespaces/inkscape"))
  (cxml-stp:find-recursively-if
   (lambda (child)
     (and (typep child 'cxml-stp:element)
	  (equal (cxml-stp:local-name child) "g")
          (if (cxml-stp:find-attribute-named child "groupmode" URI)
              (equal (cxml-stp:value (cxml-stp:find-attribute-named child "groupmode" URI)) "layer"))
          (if
           (cxml-stp:find-attribute-named child "label" URI)
           (equal (cxml-stp:value (cxml-stp:find-attribute-named child "label" URI)) layer-local-name))))
   xml))

(defun group? (node)
  (and (typep node 'cxml-stp:element)
       (equal (cxml-stp:local-name node) "g")))

#|
(defun visible? (node &key (URI "http://sodipodi.sourceforge.net/DTD/sodipodi-0.dtd"))
  (if (and (cxml-stp:find-attribute-named node "label" URI)
           (cxml-stp:find-attribute-named node "style"))
      (break "layer: ~a, type: ~a val" (cxml-stp:value (cxml-stp:find-attribute-named node "label" URI))
             (cxml-stp:value (cxml-stp:find-attribute-named node "style"))))
  (and (typep node 'cxml-stp:element)
       (equal (cxml-stp:local-name node) "g")
       (if (and (cxml-stp:find-attribute-named node "groupmode" URI)
                (equal (cxml-stp:value (cxml-stp:find-attribute-named node "groupmode" URI)) "layer"))
           (let ((val (cxml-stp:value (cxml-stp:find-attribute-named node "style"))))
             (and val (not (string= val "display:none"))))
           t)))
|#

(defun visible? (node &key (URI "http://www.inkscape.org/namespaces/inkscape"))
  (if (and (typep node 'cxml-stp:element)
           (equal (cxml-stp:local-name node) "g")
           (if (cxml-stp:find-attribute-named node "groupmode" URI)
               (equal (cxml-stp:value (cxml-stp:find-attribute-named node "groupmode" URI)) "layer"))
           (cxml-stp:find-attribute-named node "style"))
      (not (equal (cxml-stp:value (cxml-stp:find-attribute-named node "style")) "display:none"))
      t))

#|
(defun point? (node &key (URI "http://sodipodi.sourceforge.net/DTD/sodipodi-0.dtd"))
  (and (typep node 'cxml-stp:element)
       (string= (cxml-stp:value (cxml-stp:find-attribute-named node "type" URI)) "arc")))
|#

(defun circle? (node)
  (and (typep node 'cxml-stp:element)
       (string= (cxml-stp:local-name node) "circle")))

(defun path? (node)
  (and (typep node 'cxml-stp:element)
       (string= (cxml-stp:local-name node) "path")))

#|
;;; ohne Quantisierung

(defun get-coords (node transformation)
  (destructuring-bind (x y)
      (apply #'vec-mtx-mult 
             (list
              (list
               (read-from-string (cxml-stp:value (cxml-stp:find-attribute-named node "cx")))
               (read-from-string (cxml-stp:value (cxml-stp:find-attribute-named node "cy"))))
              transformation))
    (list x (* y -1))))

;;; Quantisierung auf 1/4-Töne

(defun get-coords (node transformation)
  (destructuring-bind (x y)
      (apply #'vec-mtx-mult 
             (list
              (list
               (read-from-string (cxml-stp:value (cxml-stp:find-attribute-named node "cx")))
               (read-from-string (cxml-stp:value (cxml-stp:find-attribute-named node "cy"))))
              transformation))
    (list x (* (round (* 2 y)) -0.5))))

;;; Quantisierung auf 1/4-Töne und ganze Schläge
|#

(defun get-coords (node transformation &key (xquantize t) (yquantize t))
  (destructuring-bind (x y)
      (apply #'vec-mtx-mult 
             (list
              (list
               (read-from-string (cxml-stp:value (cxml-stp:find-attribute-named node "cx")))
               (read-from-string (cxml-stp:value (cxml-stp:find-attribute-named node "cy"))))
              transformation))
    (list
     (if xquantize
         (* (round (* 2 x)) 0.5)
         x)
     (if yquantize
         (* (round (* 2 y)) -0.5)
         (* -1 y)))))

(defun style-stroke-width (style-string)
  (multiple-value-bind (_1 _2 rstart rend) (cl-ppcre:scan "stroke-width:\([0-9\.]\+\)px" style-string)
    (declare (ignore _1 _2))
    (if rstart
        (read-from-string (subseq style-string (aref rstart 0) (aref rend 0))))))

;;; (style-stroke-width "fill:#000000;fill-opacity:1;fill-rule:evenodd;stroke:#000000;stroke-width:0.5px;stroke-linecap:butt;stroke-linejoin:miter;stroke-opacity:1")

(defun style-stroke-color (style-string)
  (multiple-value-bind (_1 _2 rstart rend) (cl-ppcre:scan "stroke:\(#[0-9]\+\)" style-string)
    (declare (ignore _1 _2))
    (if rstart
        (subseq style-string (aref rstart 0) (aref rend 0)))))

;;; (style-stroke-color "fill:#000000;fill-opacity:1;fill-rule:evenodd;stroke:#000000;stroke-width:0.5px;stroke-linecap:butt;stroke-linejoin:miter;stroke-opacity:1")

(defun style-stroke-opacity (style-string)
  (multiple-value-bind (_1 _2 rstart rend) (cl-ppcre:scan "stroke-opacity:\([0-9\.]\+\)" style-string)
    (declare (ignore _1 _2))
    (read-from-string (subseq style-string (aref rstart 0) (aref rend 0)))))

;;; (style-stroke-opacity "fill:#000000;fill-opacity:1;fill-rule:evenodd;stroke:#000000;stroke-width:0.5px;stroke-linecap:butt;stroke-linejoin:miter;stroke-opacity:0.35")

(defun parse-color-opacity (str)
  (list
   (style-stroke-color str)
   (style-stroke-opacity str)))

(defun parse-path (str)
  (let ((res (read-from-string (format nil "(~a)" (substitute #\SPACE #\, (subseq str 2 nil))))))
    (if (= 4 (length res))
        (destructuring-bind (x y x2 y2) res
            (list (list x y) (list x2 y2)))
        (destructuring-bind (x y op x2 dy) res
          (declare (ignore op))
          (list (list x y) (list x2 (+ y dy)))))))

(defun make-string-parser (str)
  (let ((i 0) (j 0))
    (lambda ()
      (if j
          (progn
            (setf j (position #\Space (string-left-trim '(#\Space #\Tab #\Newline) str) :start i))
            (prog1
                (subseq str i j)
              (if j (setf i (1+ j)))))))))

#|
(defparameter fn (make-string-parser (substitute #\SPACE #\, "m 10,40 l 5,8")))
(funcall fn)
"m 10,40 l 5,8"
(parse-path2 "M 10,40 15,8")
|#

(defun move-abs (curr-coords mv-coords)
  (declare (ignore curr-coords))
  mv-coords)

(defun lineto-abs (curr-coords mv-coords)
  (declare (ignore curr-coords))
  mv-coords)

(defun move-rel (curr-coords mv-coords)
  (mapcar #'+ curr-coords mv-coords))

(defun lineto-rel (curr-coords mv-coords)
  (mapcar #'+ curr-coords mv-coords))


(defun parse-path2 (str)
  (let ((fn (make-string-parser (substitute #\SPACE #\, str)))
        (cmd nil)
        (curr-coords '(0 0)))
    (mapcar
     (lambda (x) (setf curr-coords (funcall (first x) curr-coords (rest x))))
     (loop
        for s = (funcall fn)
        while s
        collect (cond
                  ((string= s "m")
                   (progn
                     (setf cmd 'lineto-rel)
                     (list 'move-rel (read-from-string (funcall fn)) (read-from-string (funcall fn)))))
                  ((string= s "l")
                   (progn
                     (setf cmd 'lineto-rel)
                     (list 'lineto-rel (read-from-string (funcall fn)) (read-from-string (funcall fn)))))
                  ((string= s "M")
                   (progn
                     (setf cmd 'lineto-abs)
                     (list 'move-abs  (read-from-string (funcall fn)) (read-from-string (funcall fn)))))
                  ((string= s "L")
                   (progn
                     (setf cmd 'lineto-abs)
                     (list 'lineto-abs  (read-from-string (funcall fn)) (read-from-string (funcall fn)))))
                  (t (list cmd (read-from-string s) (read-from-string (funcall fn)))))))))

#|
(subseq "1 2 3" 3 nil)

(parse-path2 "M 10,40 5,8")
|#

(defun get-path-coords (node transformation &key (xquantize t) (yquantize t))
  (let* ((path (parse-path2 (cxml-stp:value (cxml-stp:find-attribute-named node "d"))))
         (style-string (cxml-stp:value (cxml-stp:find-attribute-named node "style")))
         (p1 (funcall #'vec-mtx-mult
                    (first path)
                    transformation))
         (p2 (funcall #'vec-mtx-mult 
                    (second path)
                    transformation)))
    (destructuring-bind ((x1 y1) (x2 y2)) (sort (list p1 p2) (lambda (x y) (< (first x) (first y))))
      (declare (ignore y2))
      (list
       (if xquantize
           (* (round (* 2 x1)) 0.5)
           x1)
       (if yquantize
           (* (round (* 2 y1)) -0.5)
           (* -1 y1))
       (if xquantize
           (* (round (* 2 (- x2 x1))) 0.5)
           (- x2 x1))
       (style-stroke-color style-string)
       (style-stroke-opacity style-string)))))

(defun update-transformation (curr-transformation node)
  (let ((new-transform (cxml-stp:find-attribute-named node "transform")))
    (if new-transform
        (mtx-mult
         curr-transformation
         (get-transformation-mtx
          (cxml-stp:value new-transform))))))

(defun collect-points (layer transformation &key (xquantize t) (yquantize t))
  (let ((result '()))
    (cxml-stp:map-children
     'list
     (lambda (child)
       (cond
         ((and (group? child) (visible? child))
          (let ((inner-transformation (update-transformation transformation child)))
            (let ((res (collect-points child inner-transformation :xquantize xquantize
                                       :yquantize yquantize)))
              (if res (push res result)))))
         ((circle? child) (push (get-coords child transformation :xquantize xquantize
                                                      :yquantize yquantize) result))))
     layer)
    (reverse result)))

(defun collect-lines (layer transformation &key (xquantize t) (yquantize t))
  (let ((result '()))
    (cxml-stp:map-children
     'list
     (lambda (child)
       (cond
         ((group? child) (let ((inner-transformation (update-transformation transformation child)))
                           (let ((res (collect-lines child inner-transformation :xquantize xquantize
                                                      :yquantize yquantize)))
                             (if res (push res result)))))
         ((path? child) (push (get-path-coords child transformation :xquantize xquantize
                                                      :yquantize yquantize) result))))
     layer)
    (reverse result)))

;;; (collect-points (get-layer "Punkte" *xml*))

(defun find-element (local-name xml)
  (cxml-stp:find-recursively-if
   (lambda (child)
     (and (typep child 'cxml-stp:element)
	  (equal (cxml-stp:local-name child) local-name)))
   xml))

;; (make-pathname :directory '(:absolute "tmp") :name "test.svg")

(defun get-points-from-file (&key (fname #P"/tmp/test.svg") (xquantize t) (yquantize t))
  "extract all circle objects (points) in the layer \"Punkte\" of svg infile."
  (collect-points
   (get-layer
    "Punkte"
    (cxml:parse
     fname
     (stp:make-builder)))
   nil :xquantize xquantize :yquantize yquantize))


(defun get-lines-from-file (&key (fname #P"/tmp/test.svg") (xquantize t) (yquantize t))
  "extract all line objects) in the layer \"Punkte\" of svg infile."
  (collect-lines
   (get-layer
    "Punkte"
    (cxml:parse
     fname
     (stp:make-builder)))
   nil :xquantize xquantize :yquantize yquantize))

#|

(loop
   for x in (get-points-from-file)
   collect (sort x #'(lambda (x y) (< (first x) (first y)))))

   (get-layer
    "Punkte"
    (cxml:parse
     #P"/tmp/testchords02.svg"
     (stp:make-builder)))

|#

#|
(get-layer
 "Linien"
 (cxml:parse
  #P"/tmp/test.svg"
  (stp:make-builder)))
|#