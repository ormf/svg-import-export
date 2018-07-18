;;;; svg-export.lisp

(in-package #:svg-export)

;;; "svg-export" goes here. Hacks and glory await!

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

(defparameter *svg-header* "<?xml version=\"1.0\" encoding=\"UTF-8\" standalone=\"no\"?>
<!-- Created with Inkscape (http://www.inkscape.org/) -->
<svg
   xmlns:dc=\"http://purl.org/dc/elements/1.1/\"
   xmlns:cc=\"http://creativecommons.org/ns#\"
   xmlns:rdf=\"http://www.w3.org/1999/02/22-rdf-syntax-ns#\"
   xmlns:svg=\"http://www.w3.org/2000/svg\"
   xmlns=\"http://www.w3.org/2000/svg\"
   xmlns:sodipodi=\"http://sodipodi.sourceforge.net/DTD/sodipodi-0.dtd\"
   xmlns:inkscape=\"http://www.inkscape.org/namespaces/inkscape\"
   width=\"~dpx\"
   height=\"~dpx\"
   viewBox=\"0 0 ~d ~d\"
   id=\"svg2392\"
   inkscape:document-unit=\"px\"
   sodipodi:version=\"0.32\"
   inkscape:version=\"0.92+devel 15617 custom\"
   sodipodi:docname=\"test.svg\"
   inkscape:output_extension=\"org.inkscape.output.svg.inkscape\"
   version=\"1.1\">

  <defs
     id=\"defs2394\">
    <marker
       inkscape:stockid=\"DotS\"
       orient=\"auto\"
       refY=\"0.0\"
       refX=\"0.0\"
       id=\"DotS\"
       style=\"overflow:visible\"
       inkscape:isstock=\"true\">
      <path
         id=\"path6619\"
         d=\"M -2.5,-1.0 C -2.5,1.7600000 -4.7400000,4.0 -7.5,4.0 C -10.260000,4.0 -12.5,1.7600000 -12.5,-1.0 C -12.5,-3.7600000 -10.260000,-6.0 -7.5,-6.0 C -4.7400000,-6.0 -2.5,-3.7600000 -2.5,-1.0 z \"
         style=\"fill-rule:evenodd;stroke:#000000;stroke-width:1pt;stroke-opacity:1;fill:#000000;fill-opacity:1\"
         transform=\"scale(0.2) translate(7.4, 1)\" />
    </marker>
    <marker
       inkscape:stockid=\"Arrow1Mend\"
       orient=\"auto\"
       refY=\"0.0\"
       refX=\"0.0\"
       id=\"Arrow1Mend\"
       style=\"overflow:visible;\"
       inkscape:isstock=\"true\">
      <path
         id=\"path4186\"
         d=\"M 0.0,0.0 L 5.0,-5.0 L -12.5,0.0 L 5.0,5.0 L 0.0,0.0 z \"
         style=\"fill-rule:evenodd;stroke:#000000;stroke-width:1pt;stroke-opacity:1;fill:#000000;fill-opacity:1\"
         transform=\"scale(0.4) rotate(180) translate(10,0)\" />
    </marker>
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
     inkscape:pageopacity=\"~3,1f\"
     inkscape:pageshadow=\"2\"
     inkscape:zoom=\"1.4\"
     inkscape:cx=\"356.52313\"
     inkscape:cy=\"365.77098\"
     inkscape:document-units=\"px\"
     inkscape:current-layer=\"layer1\"
     inkscape:window-width=\"1920\"
     inkscape:window-height=\"1041\"
     inkscape:window-x=\"1366\"
     inkscape:window-y=\"0\"
     showgrid=\"false\"
     gridtolerance=\"10000\"
     inkscape:window-maximized=\"1\">
    <inkscape:grid
       type=\"xygrid\"
       id=\"grid3796\"
       spacingy=\"1\"
       spacingx=\"1\"
       dotted=\"false\" 
       empspacing=\"4\" />
  </sodipodi:namedview>
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
      (print-tail-to-stream svg-file outstream))))
