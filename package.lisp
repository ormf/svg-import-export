;;;; package.lisp

(defpackage #:svg-export
  (:use #:cl #:cl-user #:orm-utils)
  (:export 
   #:new-id
   #:export-svg-file
   #:svg-file
   #:svg-layer
   #:svg-group
   #:svg-point
   #:svg-rect
   #:svg-line
   #:svg-text
   #:svg-clone
   #:print-to-stream
   #:make-piano-roll
   #:make-staff-system
   #:list->svg-points
   #:print-head-to-stream
   #:print-tail-to-stream
   #:format-with-slots
   #:svg-zeitachse
   #:pd-color->svg-color
   #:points->svg
   #:lines->svg
   #:svg->points
   #:svg->lines
   #:svg->pd-text
   #:points->pd-text
   ))
