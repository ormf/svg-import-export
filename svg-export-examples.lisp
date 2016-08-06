;;; (ql:quickload "svg-export")
;;; (ql:quickload "orm-utils")

(in-package :svg-export)
(defparameter *test* nil)

(progn
  (setf *test*
        (let ((end-time 100)
              (xscale 10)
              (gxoffs 20)
              (svg-file (make-instance 'svg-file)))
          (setf (slot-value svg-file 'elements) 
                (list
                 (append (list 
                          (make-instance 'svg-layer 
                                         :name "Ebene 1" 
                                         :id (new-id svg-file 'layer-ids)))
                         (loop for x from 0 to end-time by 12
                            collect (make-instance 'svg-rect
                                                   :x (+ gxoffs (* xscale x)) 
                                                   :y 200
                                                   :width (* xscale 10)
                                                   :height 100
                                                   :fill-color "#333333"
                                                   :id (new-id svg-file 'rect-ids))))))
          (push (svg-zeitachse end-time svg-file
                               :gxoffs gxoffs :gyoffs 100 :xscale xscale) (elements svg-file))
          svg-file))
  (export-svg-file *test*))

(print-to-stream (make-instance 'svg-text) t)

(elements *test*)


(push (svg-zeitachse 2000 *test*) (elements *test*))
