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


#|
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
|#

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

(defun export-svg-file (svg-file &key (fname "/tmp/test.svg") (inverse nil))
  (let ((*inverse* inverse))
    (with-open-file (outstream fname :direction :output :if-exists :supersede)
      (print-head-to-stream svg-file outstream)
      (print-elements-to-stream (elements svg-file) outstream)
      (print-tail-to-stream svg-file outstream)))
  fname)
