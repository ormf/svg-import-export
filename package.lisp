;;;; package.lisp

(defpackage #:svg-export
  (:use #:cl #:cl-user #:orm-utils)
  (:export 
   #:NEW-ID
   #:EXPORT-SVG-FILE
   #:SVG-FILE
   #:SVG-LAYER
   #:SVG-GROUP
   #:SVG-POINT
   #:SVG-RECT
   #:SVG-LINE
   #:SVG-TEXT
   #:SVG-CLONE
   #:PRINT-TO-STREAM
   #:MAKE-PIANO-ROLL
   #:MAKE-STAFF-SYSTEM
   #:LIST->SVG-POINTS
   #:PRINT-HEAD-TO-STREAM
   #:PRINT-TAIL-TO-STREAM
   #:FORMAT-WITH-SLOTS
   #:SVG-ZEITACHSE
   #:PD-COLOR->SVG-COLOR
   #:POINTS->SVG
   #:POINTS->PD-TEXT
   #:POINTS->MIDI
   #:LINES->SVG
   #:SVG->POINTS
   #:SVG->LINES
   #:SVG->BC-LIST
   #:BC-LIST->KEYNUM-LIST
   #:BC-LIST->SVG
   #:SVG->PD-TEXT
   #:MIDI->POINTS
   #:MIDI->LINES
   #:MIDI->SVG
   #:TRANSLATE
   ))
