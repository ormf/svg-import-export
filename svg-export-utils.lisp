;;;; svg-export-utils.lisp

(in-package #:svg-export)

(defparameter *color-lookup* nil)

;; Original quo Farben:

(defparameter *white-keys*
  (append
   (loop for startpitch from 12 to 108 by 12
      append (ou:integrate (cons startpitch '(2 2 1 2 2 2)))) '(120)))

(defparameter *black-keys*
  (loop for startpitch from 13 to 109 by 12
     append (ou:integrate (cons startpitch '(2 3 2 2)))))

(defparameter *staff-lines*
  '(19 23 26 29 33
    43 47 50 53 57
    64 67 71 74 77
    88 91 95 98 101
    112 115 119))

(defparameter *ledger-lines*
  '(12 16
    36 40
    60
    81 84
    105 108
    ))

(defun make-piano-roll (svg-file)
  (append (list (make-instance 'svg-tl-layer :name "PianoRoll" :id (new-id svg-file 'layer-ids)
                               :insensitive t))
          (list (cons (make-instance 'svg-group
                                     :id (new-id svg-file 'group-ids))
                      (append
                       (loop for pitch in *white-keys*
                          collect (make-instance 'svg-rect 
                                                 :x 0
                                                 :y (- pitch 0.5)
                                                 :width 10000
                                                 :height 1
                                                 :stroke-color "#aaaaaa"
                                                 :stroke-width 0.1
                                                 :id (new-id svg-file 'rect-ids)))
                       (loop for pitch in *black-keys*
                          collect (make-instance 'svg-rect 
                                                 :x 0
                                                 :y (- pitch 0.5)
                                                 :width 10000
                                                 :height 1
                                                 :stroke-width 0.1
                                                 :stroke-color "#aaaaaa"
                                                 :fill-color "#dddddd"
                                                 :id (new-id svg-file 'rect-ids))))))))


(defun make-staff-system (svg-file)
  (append (list (make-instance 'svg-tl-layer :name "Stafflines" :id (new-id svg-file 'layer-ids) :insensitive t))
          (list (cons (make-instance 'svg-group :id (new-id svg-file 'group-ids))
                      (append
                       (loop for pitch in *staff-lines*
                          collect (make-instance 'svg-line
                                                 :x1 0
                                                 :y1 pitch
                                                 :x2 10000
                                                 :y2 pitch
                                                 :stroke-color "#000000"
                                                 :stroke-width 0.2
                                                 :id (new-id svg-file 'line-ids)))
                       (loop for pitch in *ledger-lines*
                          collect (make-instance 'svg-line
                                                 :x1 0
                                                 :y1 pitch
                                                 :x2 10000
                                                 :y2 pitch
                                                 :stroke-color "#eeeeee"
                                                 :stroke-width 0.4
                                                 :id (new-id svg-file 'line-ids))))))))

(defun list->svg-points (list svg-file)  
  (loop for point in list
     collect
       (if (numberp (car point))
           (destructuring-bind (cx cy color) point
             (make-instance 'svg-point :cx cx :cy (+ (* -1 cy)) 
                            :rx 0.5 :ry 0.5
                            :stroke-width 0
                            ;;                                      :stroke-color (color-lookup color) 
                            :fill-color color
                            :id (new-id svg-file 'point-ids)))
           (cons (make-instance 'svg-group :id (new-id svg-file 'group-ids))
                 (list->svg-points point svg-file)))))

(let ((*inverse* t))
  (setf *color-lookup*
        (mapcar (lambda (x)
                  (list (first x) (get-color (second x))))
                '((0 "#000000") (1 "#0000FF") (2 "#FF0000") (3 "#00FFFF") (4 "#00FF00")
                  (5 "#FFB400") (6 "#FF00FF") (7 "#FFFFFF") (8 "#00008F") (9 "#0000B0")
                  (10 "#0000D1") (11 "#87CFFF") (12 "#008F00") (13 "#00B000") (14 "#00D100")
                  (15 "#008F8F") (16 "#00B0B0") (17 "#00D1D1") (18 "#8F0000") (19 "#B00000")
                  (20 "#D10000") (21 "#8F008F") (22 "#B000B0") (23 "#D100D1") (24 "#803000")
                  (25 "#A14000") (26 "#BF6100") (27 "#FF8080") (28 "#FFA1A1") (29 "#FFBFBF")
                  (30 "#FFE0E0") (31 "#FFD600") (32 "#FF334C") (33 "#CCCCCC") (34 "#999999")
                  (35 "#666666") (36 "#333333") (37 "#00E500") (38 "#007FFF") (39 "#CC7F19")
                  (40 "#8C19FF") (41 "#FFD5DA") (42 "#A5E5A5") (43 "#A0D0FF") (44 "#CCA570")
                  (45 "#CF9EFF") (46 "#CF9EFF")))))

(defun color-lookup (coloridx)
  (cadr (assoc coloridx *color-lookup*)))

(defparameter *pd-colors* '((888 0) (555 1) (900000 2) (77 3) (80 4) (959452 5) (43004 6) (673356 7)
 (99 8) (909 9) (9 10) (77 11) (758 12) (1009 13) (821 14) (676 15) (115 16)
 (1724 17) (1652 18) (202 19) (1201 20) (591 21) (1670 22) (1428 23) (1789 24)
 (1272 25) (555 26) (573 27) (934 28) (1042 29) (542 30) (148 31) (1912 32)
 (1855 33) (1269 34) (1140 35) (286 36) (1785 37) (271 38) (331 39) (1069 40)
 (530 41) (1577 42) (593 43) (734 44) (1649 45) (1240 46) (270 47) (324 48)
 (1782 49) (1096 50) (555 51) (1839 52) (536 53) (532 54) (1285 55) (1029 56)
 (934 57) (12 58) (48 59) (801 60) (1195 61) (1632 62) (510 63) (1371 64)
 (98 65) (44 66) (13 67) (1172 68) (1466 69) (544 70) (1944 71) (87 72)
 (1756 73) (614 74) (1247 75) (1048 76) (1271 77) (601 78) (51 79) (425 80)
 (1692 81) (560 82) (207 83) (324 84) (1110 85) (944 86) (1226 87) (1748 88)
 (1175 89) (301 90) (1329 91) (702 92) (1605 93) (164 94) (1705 95) (308 96)
 (1255 97) (1125 98) (458 99) (1383 100) (551 101) (1530 102) (736 103)
 (809 104) (870 105) (440 106) (847 107) (1179 108) (758 109) (31 110)))

(defun pd-color->svg-color (pd-color)
  (color-lookup (cadr (assoc pd-color *pd-colors*))))

;;; (pd-color->svg-color 758)

(defun get-tick-lines (end-time svg-file &key
                       (start 0) (interval 10) (length 5) (non-drawing-modulo nil)
                       (ypos 0) (gxoffs 20) (gyoffs 300) (xscale 1))
  (let ((modulo (if (numberp non-drawing-modulo)
                    (* non-drawing-modulo interval))))
    (loop
       for time from start to end-time by interval
       append (if (or (not modulo) (> (mod time modulo) 0))
                  (list (make-instance 'svg-line
                                       :id (new-id svg-file 'line-ids)
                                       :x1 (+ gxoffs (* xscale (- time start)))
                                       :x2 (+ gxoffs (* xscale (- time start)))
                                       :y1 (+ gyoffs ypos)
                                       :y2 (+ gyoffs ypos length)
                                       :stroke-width 0.2
                                       :fill-color +black+
                                       :stroke-color +black+))))))

;;; Mit Gruppierung:

(defun svg-zeitachse 
    (end-time svg-file &key 
                         (ypos -50)
                         (gxoffs 20)
                         (gyoffs 300)
                         (xscale 1)
                         (ticks '(t t t t t)))
  
  
  (list
   (make-instance 'svg-layer 
                  :name "Zeitachse" 
                  :id (new-id svg-file 'layer-ids))
   (append
    (list
     (make-instance 'svg-group
                    :id (new-id svg-file 'group-ids))
     (make-instance 'svg-line
                    :id (new-id svg-file 'line-ids)
                    :x1 gxoffs
                    :x2 (+ gxoffs (* xscale end-time))
                    :y1 (+ gyoffs ypos)
                    :y2 (+ gyoffs ypos)
                    :stroke-width 0.2
                    :fill-color +black+
                    :stroke-color +black+))
    (if (nth 0 ticks) 
        (get-tick-lines end-time svg-file 
                        :interval 60
                        :length 10
                        :xscale xscale
                        :gxoffs gxoffs
                        :gyoffs gyoffs
                        :ypos ypos))
    (if (nth 1 ticks) 
        (get-tick-lines end-time svg-file
                        :interval 30
                        :non-drawing-modulo 2
                        :length 8
                        :xscale xscale
                        :gxoffs gxoffs
                        :gyoffs gyoffs
                        :ypos ypos))
    (if (nth 2 ticks)
        (get-tick-lines end-time svg-file
                        :interval 10
                        :non-drawing-modulo 3
                        :length 5
                        :xscale xscale
                        :gxoffs gxoffs
                        :gyoffs gyoffs
                        :ypos ypos))
    (if (nth 3 ticks) 
        (get-tick-lines end-time svg-file
                        :interval 5
                        :non-drawing-modulo 2
                        :length 3
                        :xscale xscale
                        :gxoffs gxoffs
                        :gyoffs gyoffs
                        :ypos ypos))
    (if (nth 4 ticks) 
        (get-tick-lines end-time svg-file
                        :interval 1
                        :non-drawing-modulo 5
                        :length 1
                        :xscale xscale
                        :gxoffs gxoffs
                        :gyoffs gyoffs
                        :ypos ypos)))
   (cons (make-instance 'svg-group
                        :id (new-id svg-file 'group-ids))
         (get-time-text end-time svg-file
                        :xscale xscale
                        :gxoffs (- gxoffs 7.5)
                        :gyoffs gyoffs
                        :ypos (- ypos 2)))))

#|

;;; Ohne Gruppierung:

(defun svg-zeitachse 
    (end-time svg-file &key 
     (ypos -50)
     (gxoffs 20)
     (gyoffs 300)
     (xscale 1)
     (ticks '(t t t t t)))
  (append
   (list
    (make-instance 'svg-layer 
                   :name "Zeitachse" 
                   :id (new-id svg-file 'layer-ids))
    (make-instance 'svg-line
                   :id (new-id svg-file 'line-ids)
                   :x1 gxoffs
                   :x2 (+ gxoffs (* xscale end-time))
                   :y1 (+ gyoffs ypos)
                   :y2 (+ gyoffs ypos)
                   :stroke-width 0.2
                   :fill-color +black+
                   :stroke-color +black+))
   (if (nth 0 ticks) 
       (get-tick-lines end-time svg-file 
		       :interval 60
		       :length 10
		       :xscale xscale
		       :gxoffs gxoffs
		       :gyoffs gyoffs
		       :ypos ypos))
   (if (nth 1 ticks) 
       (get-tick-lines end-time svg-file
		       :interval 30
		       :non-drawing-modulo 2
		       :length 8
		       :xscale xscale
		       :gxoffs gxoffs
		       :gyoffs gyoffs
		       :ypos ypos))
   (if (nth 2 ticks)
       (get-tick-lines end-time svg-file
		       :interval 10
		       :non-drawing-modulo 3
		       :length 5
		       :xscale xscale
		       :gxoffs gxoffs
		       :gyoffs gyoffs
		       :ypos ypos))
   (if (nth 3 ticks) 
       (get-tick-lines end-time svg-file
		       :interval 5
		       :non-drawing-modulo 2
		       :length 3
		       :xscale xscale
		       :gxoffs gxoffs
		       :gyoffs gyoffs
		       :ypos ypos))
   (if (nth 4 ticks) 
       (get-tick-lines end-time svg-file
		       :interval 1
		       :non-drawing-modulo 5
		       :length 1
		       :xscale xscale
		       :gxoffs gxoffs
		       :gyoffs gyoffs
		       :ypos ypos))
   (cons
    (get-time-text end-time svg-file
                   :xscale xscale
                   :gxoffs (- gxoffs 7.5)
                   :gyoffs gyoffs
                   :ypos (- ypos 2)))))
|#

(defun get-time-text (end-time svg-file
                      &key
                        (start 0)
                        (font-family "Arial")
                        (font-size 6)
                        (font-style "normal")
                        (ypos 0) (gxoffs 20) (gyoffs 300) (xscale 1))
  (loop for time from start to end-time by 30
     append 
     (list (make-instance 'svg-text
			  :id (new-id svg-file 'text-ids)
			  :x (+ gxoffs (* xscale (- time start)))
			  :y (+ gyoffs ypos)
			  :font-family font-family
			  :font-size font-size
			  :font-style font-style
			  :label (format nil "~2,'0d:~2,'0d"
					 (round (floor time 60))
					 (mod time 60))
			  :stroke-width 0.2
			  :fill-color +black+
			  :stroke-color +black+))))
