;;; svg-export.lisp
;;;
;;; main export-svg-file routine and helpers
;;;
;;; **********************************************************************
;;; Copyright (C) 2012-2018 Orm Finnendahl
;;;
;;; This program is free software; you can redistribute it and/or
;;; modify it under the terms of the Lisp Lesser Gnu Public License.
;;; See http://www.cliki.net/LLGPL for the text of this agreement.
;;; **********************************************************************


(in-package #:svg-import-export)

;;; "svg-import-export" goes here. Hacks and glory await!

(setf *print-case* :downcase)

(defparameter *inverse* nil)

(defun get-color (color)
  (if (and *inverse* (string/= color "none"))
      (format nil "#~6,'0X" (- #XFFFFFF (read-from-string (format nil "#X~a" (string-left-trim '(#\#) color)))))
      color))

(defun new-id (svg-file id-type)
  (incf (gethash id-type (slot-value svg-file 'id-hash) 0)))

(defun print-elements-to-stream (elems stream)
  (cond ((null elems) nil)
        ((consp (car elems))
         (progn
           (print-head-to-stream (caar elems) stream)
           (print-elements-to-stream (cdar elems) stream)
           (print-tail-to-stream (caar elems) stream)
           (print-elements-to-stream (cdr elems) stream)))
        (t (progn
           (print-to-stream (car elems) stream)
           (print-elements-to-stream (cdr elems) stream)))))

(defun export-svg-file (svg-file &key (fname "/tmp/test.svg") (inverse nil) (showgrid t) (width 10000))
  (setf (sv svg-file 'inverse) inverse)
  (setf (sv svg-file 'showgrid) showgrid)
  (setf (sv svg-file 'width) width)
  (with-open-file (outstream fname :direction :output :if-exists :supersede)
    (print-head-to-stream svg-file outstream)
    (print-elements-to-stream (elements svg-file) outstream)
    (print-tail-to-stream svg-file outstream))
  fname)
