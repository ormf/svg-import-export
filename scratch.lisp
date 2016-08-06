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
         (,@(mapcar (lambda (spec)
                      (let ((export-pos (position :export spec)))
                        (if export-pos
                            (append (subseq spec 0 export-pos)
                                    (subseq spec (+ 2 export-pos)))
                            spec)))
                    slot-specs))
         ,class-option)
       ,@(mapcar (lambda (name) `(export ,name))
                 exports))))

(macroexpand-1 '(def-exporting-class test1 ()
                           ((test-1 :accessor test-1 :export nil)
                            (test-2 :initform 1 :export t :reader test-2)
                            (test-3 :export t))))

(getf '(:accessor test-1) :export)

(mapcan #'(lambda (x) (if (numberp x) x))
	'(a 1 b 2 3 c d 4 5 6))

(getf)

(when nil 23)

(mapcan #'car
	'((a 1) (b 2) (3 c) (d 4) (5 6)))

(PROGN
  (DEFCLASS TEST1 NIL
           ((TEST-1 :ACCESSOR TEST-1) (TEST-2 :INITFORM 1 :READER TEST-2)
            (TEST-3))
           NIL)
 (EXPORT TEST-1)
 (EXPORT TEST-2))
