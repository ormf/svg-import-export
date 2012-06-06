;;; (ql:quickload "svg-export")

(in-package :svg-export)



(setf *test*
      (let ((svg-file (make-instance 'svg-file)))
	(setf (slot-value svg-file 'elements) 
	      (list
	       (append (list 
			(make-instance 'svg-layer 
				       :name "Ebene 1" 
				       :id (new-id svg-file 'layer-ids)))
		       (loop for x from 0 to 1000 by 120
			  collect (make-instance 'svg-rect 
						 :x x 
						 :y 200
						 :width 100
						 :height 100
						 :fill-color "#333333"
						 :id (new-id svg-file 'rect-ids))))))
	svg-file))

(elements *test*)

(export-svg-file *test*)
