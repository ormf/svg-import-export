;;;(ql:quickload "csound-export")
(in-package :svg-export)

(load "/home/orm/work/kompositionen/ast/lisp/svg-import.lisp")

(defparameter *basedir* nil)
(defparameter *tuning-base* 443)
(defun grafikpath (fname)
  (pathname (format nil "~a/grafik/~a" *basedir* fname)))

(defun cljpath (fname)
  (pathname (format nil "~a/clojure/~a" *basedir* fname)))
(setf *basedir* "/home/orm/work/kompositionen/ast")

(defun qpm->usecs (bpm)
  (round (* (/ 60 bpm) 1000000)))

(defparameter *csound-header-sine* nil)

(setf *csound-header-sine*
    "<CsoundSynthesizer>
<CsOptions>
-s -W -d -o ~a
</CsOptions>

<CsInstruments>

  sr        =           44100
  kr        =           44100
  ksmps     =           1
  nchnls    =           1
  gituning = ~a
  
instr 1
  icps      =           (gituning/440)*cpsoct(p5/12.0+3)
  iamp      =           ampdb(p4)
  a1        oscili       iamp, icps, 19
  kenv      expseg      1,p3,0.01
            out         kenv*a1
endin 

instr 2
  icps      =           (gituning/440)*cpsoct(p5/12.0+3)
  iamp      =           ampdb(p4)
  a1        oscili       iamp, icps, 19
  kenv      expseg      0.01, 0.1, 1, p3-0.2, 1, 0.1, 0.01
            out         kenv*a1
endin 


instr 3
  icps      =           (gituning/440)*cpsoct(p5/12.0+3)
  iamp      =           ampdb(p4)
  a1        oscili      iamp, icps, 19
  a2        oscili      iamp, icps*1.943, 19
  a3        oscili      iamp, icps*2.866, 19
  a4        oscili      iamp, icps*3.775, 19
  a5        oscili      iamp, icps*4.676, 19
  kenv      expseg      0.01, 0.1, 1, p3-0.2, 1, 0.1, 0.01
            out         kenv*0.2*(a1+a2+a3+a4+a5)
endin 



</CsInstruments>

<CsScore>
f1      0       0       1       \"BOSEN_mf_A0_mn.wav\"   0       4       0       ;set    SSDIR   =       /home/orm/work/snd
f2      0       0       1       \"BOSEN_mf_D1_mn.wav\"   0       4       0
f3      0       0       1       \"BOSEN_mf_C2_mn.wav\"   0       4       0
f4      0       0       1       \"BOSEN_mf_F2_mn.wav\"   0       4       0
f5      0       0       1       \"BOSEN_mf_D#3_mn.wav\"  0       4       0
f6      0       0       1       \"BOSEN_mf_C#4_mn.wav\"  0       4       0
f7      0       0       1       \"BOSEN_mf_F#4_mn.wav\"  0       4       0
f8      0       0       1       \"BOSEN_mf_B4_mn.wav\"   0       4       0


f10     0       128     -17     0       1       36      2       43      3       51      4       58      5       68      6 76 7 81 8 ;map notnum to table
f11     0       16      -2      0       33      38      48      53      63      73      78      83      ;map    table   to basnot
f12     0       16      -2      0       121642  93488   95464   77922   82246   64379   57945   48970   ;map    table   to loopstarts
f13     0       16      -2      0       197454  166611  151802  137052  132839  104238  59609   50748   ;map    table   to loopends
f14     0       0       1       \"click02.wav\"   0       4       0
f15     0       0       1       \"click01.wav\"   0       4       0
f16     0       0       1       \"click03.wav\"   0       4       0
f17     0       0       1       \"click04.wav\"   0       4       0
f18     0       8388608       1      \"english.wav\"   0       4       0
f19     0       32768   10     1 ;simple Sine Wave

t 0 ~a
")

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
  (append (list (make-instance 'svg-layer :name "PianoRoll" :id (new-id svg-file 'layer-ids)
                               :insensitive t))
          (list (cons (make-instance 'svg-group
                                     :id (new-id svg-file 'group-ids))
                      (append
                       (loop for pitch in *white-keys*
                          collect (make-instance 'svg-rect 
                                                 :x 0
                                                 :y (* -1 (+ pitch 0.5))
                                                 :width 10000
                                                 :height 1
                                                 :stroke-color "#aaaaaa"
                                                 :stroke-width 0.1
                                                 :id (new-id svg-file 'rect-ids)))
                       (loop for pitch in *black-keys*
                          collect (make-instance 'svg-rect 
                                                 :x 0
                                                 :y (* -1 (+ pitch 0.5))
                                                 :width 10000
                                                 :height 1
                                                 :stroke-width 0.1
                                                 :stroke-color "#aaaaaa"
                                                 :fill-color "#dddddd"
                                                 :id (new-id svg-file 'rect-ids))))))))

;; (make-piano-roll (make-instance 'svg-file))

(defun make-staff-system (svg-file)
  (append (list (make-instance 'svg-layer :name "Stafflines" :id (new-id svg-file 'layer-ids) :insensitive t))
          (list (cons (make-instance 'svg-group :id (new-id svg-file 'group-ids))
                      (append
                       (loop for pitch in *staff-lines*
                          collect (make-instance 'svg-line
                                                 :x1 0
                                                 :y1 (* -1 pitch)
                                                 :x2 10000
                                                 :y2 (* -1 pitch)
                                                 :stroke-color "#000000"
                                                 :stroke-width 0.2
                                                 :id (new-id svg-file 'line-ids)))
                       (loop for pitch in *ledger-lines*
                          collect (make-instance 'svg-line
                                                 :x1 0
                                                 :y1 (* -1 pitch)
                                                 :x2 10000
                                                 :y2 (* -1 pitch)
                                                 :stroke-color "#eeeeee"
                                                 :stroke-width 0.4
                                                 :id (new-id svg-file 'line-ids))))))))

(defun make-zeitachse (svg-file &key (mins t) (halfmins t) (tensecs t) (fivesecs t) (secs t) (xscale 1))
  (append (list (make-instance 'svg-layer :name "Zeitachse" :id (new-id svg-file 'layer-ids) :insensitive t))
                         (list (cons (make-instance 'svg-group :id (new-id svg-file 'group-ids))
                                     (loop for time from 0 to 900 by 30
                                        append
                                          (cond
                                           ((and (zerop (mod time 60)) mins)
                                             (list
                                              (make-instance 'svg-text 
                                                             :x (* xscale time)
                                                             :y (* -1 xscale 20)
                                                             :label (format nil "~d.00" (floor time 60))
                                                             :id (new-id svg-file 'text-ids))))
                                            ((and (zerop (mod time 30)) halfmins)
                                             (list
                                              (make-instance 'svg-text 
                                                             :x (* xscale time)
                                                             :y (* -1 xscale 20)
                                                             :label (format nil "~d.30" (floor time 60))
                                                             :id (new-id svg-file 'text-ids))))
                                            (t nil)))))
                         (list (cons (make-instance 'svg-group :id (new-id svg-file 'group-ids))
                                     (loop for time from 0 to 900
                                        append
                                          (cond
                                            ((and (zerop (mod time 60)) mins)
                                             (list
                                              (make-instance 'svg-line :x1 (* xscale time) :x2 (* xscale time) 
                                                             :y1 (* -1 xscale 10) :y2 (* -1 xscale 20) 
                                                             :id (new-id svg-file 'line-ids))))
                                            ((and (zerop (mod time 30)) halfmins)
                                             (list
                                              (make-instance 'svg-line :x1 (* xscale time) :x2 (* xscale time) 
                                                             :y1 (* -1 xscale 10) :y2 (* -1 xscale 17.5) 
                                                             :id (new-id svg-file 'line-ids))))
                                            ((and (zerop (mod time 10)) tensecs)
                                             (list
                                              (make-instance 'svg-line :x1 (* xscale time) :x2 (* xscale time) 
                                                             :y1 (* -1 xscale 10) :y2 (* -1 xscale 15) 
                                                             :id (new-id svg-file 'line-ids))))
                                            ((and (zerop (mod time 5)) fivesecs)
                                             (list
                                              (make-instance 'svg-line :x1 (* xscale time) :x2 (* xscale time) 
                                                             :y1 (* -1 xscale 10) :y2 (* -1 xscale 12.5) 
                                                             :id (new-id svg-file 'line-ids))))
                                            ((and (zerop (mod time 1)) secs)
                                             (list
                                              (make-instance 'svg-line :x1 (* xscale time) :x2 (* xscale time) 
                                                             :y1 (* -1 xscale 10) :y2 (* -1 xscale 11) 
                                                             :id (new-id svg-file 'line-ids))))
                                            (t nil)))))))

(defun list->svg-points (list svg-file &key (color +black+) (radius 0.5))  
  (loop for point in list
     collect
       (if (numberp (car point))
           (if (nth 3 point)
               (destructuring-bind (cx cy color opacity) point
                 (make-instance 'svg-point :cx cx :cy (+ (* -1 cy)) 
                                :rx radius :ry radius
                                :stroke-width 0
                                ;;                                      :stroke-color (color-lookup color) 
                                :fill-opacity opacity
                                :fill-color color
                                :id (new-id svg-file 'point-ids)))
               (if (nth 2 point)
                   (destructuring-bind (cx cy color) point
                     (make-instance 'svg-point :cx cx :cy (+ (* -1 cy)) 
                                    :rx radius :ry radius
                                    :stroke-width 0
                                    ;;                                      :stroke-color (color-lookup color) 
                                    :fill-color color
                                    :id (new-id svg-file 'point-ids)))
                   (destructuring-bind (cx cy) point
                     (make-instance 'svg-point :cx cx :cy (+ (* -1 cy)) 
                                    :rx radius :ry radius
                                    :stroke-width 0
                                    ;;                                      :stroke-color (color-lookup color) 
                                    :fill-color color
                                    :id (new-id svg-file 'point-ids)))))
           (cons (make-instance 'svg-group :id (new-id svg-file 'group-ids))
                 (list->svg-points point svg-file)))))

(defun list->svg-lines (list svg-file &key (color +black+) (stroke-width 0.5))  
  (loop for line in list
     collect
       (if (numberp (car line))
           (cond ((nth 4 line)
                  (destructuring-bind (x1 y1 width color opacity) line
                    (make-instance 'svg-line :x1 x1 :y1 (* -1 y1)
                                
                                   :x2 (+ x1 width) :y2 (* -1 y1)
                                   :stroke-width stroke-width
                                   :stroke-opacity opacity
                                   :stroke-color color 
;;                                   :fill-color color
                                   :id (new-id svg-file 'line-ids)
                                   )))
                 ((nth 3 line)
                  (destructuring-bind (x1 y1 width color) line
                    (make-instance 'svg-line :x1 x1 :y1 (* -1 y1)
                                
                                   :x2 (+ x1 width) :y2 (* -1 y1)
                                   :stroke-width stroke-width
                                   :stroke-color color
;;                                   :fill-color color
                                   :id (new-id svg-file 'line-ids))))
                 (t
                  (destructuring-bind (x1 y1 width) line
                    (make-instance 'svg-line :x1 x1 :y1 (+ (* -1 y1))
                                
                                   :x2 (+ x1 width) :y2 (* -1 y1)
                                   :stroke-width stroke-width
                                   :stroke-color color
;;                                   :fill-color color
                                   :id (new-id svg-file 'line-ids)))))
           (cons (make-instance 'svg-group :id (new-id svg-file 'group-ids))
                 (list->svg-lines line svg-file)))))

(defun regenerate-points (svg-file &key (fname #P"/tmp/test.svg") (xquantize t) (yquantize t))
  (append 
   (list (make-instance 'svg-layer :name "Punkte" :id (new-id svg-file 'layer-ids)))
   (list->svg-points (get-points-from-file :fname fname :xquantize xquantize :yquantize yquantize) svg-file)))

(defun renew-svg-file (&key (infile #P"/tmp/test.svg")
                         (outfile #P"/tmp/test.svg"))
  (let ((svg-file (make-instance 'svg-file)))
    (setf (slot-value svg-file 'elements) 
          (list
           (make-piano-roll svg-file)
           (make-staff-system svg-file)
           (regenerate-points svg-file :fname infile)
           ))
    (export-svg-file svg-file :fname outfile)))

;; (renew-svg-file)

(defun get-p-dur (start l)
  (loop
     for x in l
     if (< start (first x)) return (return (- (first x) start))
     finally (return 1)))

(defun adjust-timing (list &key (timescale 1) (dynamik 20))
  (let* ((offs 0)
         (min-pitch (apply #'min (mapcar #'second list)))
         (max-pitch (apply #'max (mapcar #'second list)))
         (get-vol (lambda (pitch) (+ 96
                                 (* (- min-pitch pitch)
                                    (/ dynamik (- max-pitch min-pitch))))))
         (last-elem (copy-list (first (nthcdr (- (length list) 1) list)))))
    (loop
       for point in list
       for plist on (append (cdr list) (list (cons (+ (first last-elem) 1) (cdr last-elem))))
       collect (destructuring-bind (x y) point
                 (list (* (+ x offs) timescale) y (funcall get-vol y) (* timescale (get-p-dur x plist)))))))

(defun points->wav (points &key (outfile "test-sine")
                             (timescale 1/2) (dynamik 20) (decay 0.02) (db 0)
                             (tuning-base *tuning-base*))
  (let ((path "/home/orm/work/kompositionen/ast/csound")
        (basename outfile))
    (with-open-file (outstream 
                     (format nil "~a/csd/~a.csd" path basename)
                     :direction :output :if-exists :supersede)
      (format outstream (format nil *csound-header-sine* (format nil "~a/wav/~a.wav" path basename) tuning-base 60))
      (loop 
         for x in (adjust-timing
                   (remove-duplicates
                    (ou:flatten-fn points :fn #'caar)
                    :test #'equal)
                   :timescale timescale :dynamik dynamik)
         do (format outstream "i1 ~6,2f ~6,2f ~6,2f ~6,2f~%"
                    (first x)
                    (* decay (expt 2 (/ (- 105 (second x)) 24)))
                    (+ db (float (third x)))
                    (second x)))
      (format outstream csound-export:*csound-footer*))
    (sb-ext:run-program "/usr/bin/csound" (list (format nil "~a/csd/~a.csd" path basename)))
    (sb-ext:run-program "/usr/local/bin/snd" (list (format nil "~a/wav/~a.wav" path basename)))))

(defun points->lwav (points &key (outfile "test-sine")
                              (timescale 1/2) (dynamik 20) (decay 0.02) (db 0)
                              (tuning-base *tuning-base*))
  (declare (ignore decay))
  (let ((path "/home/orm/work/kompositionen/ast/csound")
        (basename outfile))
    (with-open-file (outstream 
                     (format nil "~a/csd/~a.csd" path basename)
                     :direction :output :if-exists :supersede)
      (format outstream (format nil *csound-header-sine* (format nil "~a/wav/~a.wav" path basename)
                                tuning-base 60))
      (let ((evts (adjust-timing
                   (remove-duplicates
                    (ou:flatten-fn points :fn #'caar)
                    :test #'equal)
                   :timescale timescale :dynamik dynamik)))
        (loop 
           for evt in (mapcar (lambda (x y) (append x (list (- (first y) (first x)))))
                              evts (append (rest evts) (nthcdr (- (length evts) 1) evts)) )
           do (format outstream "i2 ~6,2f ~6,2f ~6,2f ~6,2f~%"
                      (first evt)
                      (+ (fourth evt) 0.2)
                      (+ db (float (third evt)))
                      (second evt))))
      (format outstream csound-export:*csound-footer*))
    (sb-ext:run-program "/usr/bin/csound" (list (format nil "~a/csd/~a.csd" path basename)))
    (sb-ext:run-program "/usr/local/bin/snd" (list (format nil "~a/wav/~a.wav" path basename)))))


(defun keynum->db (keynum)
  (cond
    ((> keynum 72) (+ 60 (* (/ (- keynum 72) 12) -6)))
    ((< keynum 48) (+ 60 (* (/ (- 48 keynum) 12) -6)))
    (t 60)))

(defun lines->lwav (lines &key (outfile "test-lsine")
                            (timescale 1/2) (db 0) (tuning-base *tuning-base*))
  (let ((path "/home/orm/work/kompositionen/ast/csound")
        (basename outfile))
    (with-open-file (outstream 
                     (format nil "~a/csd/~a.csd" path basename)
                     :direction :output :if-exists :supersede)
      (format outstream (format nil *csound-header-sine* (format nil "~a/wav/~a.wav" path basename) tuning-base 60))
      (let ((evts (remove-duplicates
                   (ou:flatten-fn lines :fn #'caar)
                   :test #'equal)))
        (loop 
           for evt in evts
           do (format outstream "i2 ~6,2f ~6,2f ~6,2f ~6,2f~%"
                      (* timescale (first evt))
                      (* timescale (third evt))
                      (+ (keynum->db (second evt)) db)
                      (second evt))))
      (format outstream csound-export:*csound-footer*))
    (sb-ext:run-program "/usr/bin/csound" (list (format nil "~a/csd/~a.csd" path basename)))
    (sb-ext:run-program "/usr/local/bin/snd" (list (format nil "~a/wav/~a.wav" path basename)))))

(defun points->bwav (points &key (outfile "test-sine")
                              (timescale 1/2) (dynamik 20) (decay 0.02) (db 0)
                              (tuning-base *tuning-base*))
  (declare (ignore decay))
  (let ((path "/home/orm/work/kompositionen/ast/csound")
        (basename outfile))
    (with-open-file (outstream 
                     (format nil "~a/csd/~a.csd" path basename)
                     :direction :output :if-exists :supersede)
      (format outstream (format nil *csound-header-sine* (format nil "~a/wav/~a.wav" path basename) tuning-base 60))
      (let ((evts (adjust-timing
                   (remove-duplicates
                    (ou:flatten-fn points :fn #'caar)
                    :test #'equal)
                   :timescale timescale :dynamik dynamik)))
        (loop 
           for evt in (mapcar (lambda (x y) (append x (list (- (first y) (first x)))))
                              evts (append (rest evts) (nthcdr (- (length evts) 1) evts)) )
           do (format outstream "i3 ~6,2f ~6,2f ~6,2f ~6,2f~%"
                      (first evt)
                      (+ (fourth evt) 0.2)
                      (+ db (float (third evt)))
                      (second evt))))
      (format outstream csound-export:*csound-footer*))
    (sb-ext:run-program "/usr/bin/csound" (list (format nil "~a/csd/~a.csd" path basename)))
    (sb-ext:run-program "/usr/local/bin/snd" (list (format nil "~a/wav/~a.wav" path basename)))))

(defun svg->wav (&key (infile #P"/tmp/test.svg") (outfile "test-sine")
                   (timescale 1/2) (dynamik 20) (decay 0.02) (db 0) (xquantize t) (yquantize t)
                   (tuning-base *tuning-base*))
  (points->wav
   (get-points-from-file :fname infile :xquantize xquantize :yquantize yquantize)
   :outfile outfile
   :timescale timescale
   :dynamik dynamik
   :decay decay
   :db db)
  :tuning-base tuning-base)

(defun svg->lwav (&key (infile #P"/tmp/test.svg") (outfile "test-sine")
                    (timescale 1/2) (dynamik 20) (decay 0.02) (db 0) (xquantize t) (yquantize t)
                    (tuning-base *tuning-base*))
  (points->lwav
   (get-points-from-file :fname infile :xquantize xquantize :yquantize yquantize)
   :outfile outfile
   :timescale timescale
   :dynamik dynamik
   :decay decay
   :db db)
  :tuning-base tuning-base)

#|

(defun svg->lwav (&key (infile #P"/tmp/test.svg") (outfile "test-sine") (timescale 1/2) (dynamik 20) (decay 0.02) (db 0) (quantize t))
  (declare (ignore decay))
  (let ((path "/home/orm/work/kompositionen/ast/csound")
        (basename outfile))
    (with-open-file (outstream 
                     (format nil "~a/csd/~a.csd" path basename)
                     :direction :output :if-exists :supersede)
      (format outstream (format nil *csound-header-sine* (format nil "~a/wav/~a.wav" path basename) 60))
      (let ((evts (adjust-timing
                   (remove-duplicates
                    (ou:flatten-fn (get-points-from-file :fname infile :quantize quantize) :fn #'caar)
                    :test #'equal)
                   :timescale timescale :dynamik dynamik)))
        (loop 
           for evt in (mapcar (lambda (x y) (append x (list (- (first y) (first x)))))
                              evts (append (rest evts) (nthcdr (- (length evts) 1) evts)) )
           do (format outstream "i2 ~6,2f ~6,2f ~6,2f ~6,2f~%"
                      (first evt)
                      (+ (fourth evt) 0.2)
                      (+ db (float (third evt)))
                      (second evt))))
      (format outstream csound-export:*csound-footer*))
    (sb-ext:run-program "/usr/bin/csound" (list (format nil "~a/csd/~a.csd" path basename)))
    (sb-ext:run-program "/usr/local/bin/snd" (list (format nil "~a/wav/~a.wav" path basename)))))
|#




(defun get-sixteenth (dur tempo)
  (float (* 1/16 (/ 60 (* dur tempo)))))

(defun svg->midi (&key (infile #P"/tmp/test.svg") (outfile "test-sine") (timescale 1/4) (dynamik 20) (play nil) (xquantize t) (yquantize t))
  (let ((path "/home/orm/work/kompositionen/ast/midi")
        (basename outfile))
    (cm:events
     (cons (cm:new cm:midi-tempo-change :time 0 :usecs (qpm->usecs (/ 15 timescale)))
           (loop 
              for x in (adjust-timing (ou:flatten-fn (get-points-from-file :fname infile :xquantize xquantize
                                                                           :yquantize yquantize) :fn #'caar) :timescale timescale :dynamik dynamik)
              collect (cm:new cm:midi
                        :time (first x)
                        :duration 1/4
                        :amplitude (/ (float (third x)) 96)
                        :keynum (second x))))
     (format nil "~a/~a.midi" path basename)
     :play play)))


(defun points->midi (points &key (outfile "/tmp/test.midi") (timescale 1))
  (cm:events
   (mapcar (lambda (pt) (cm:new cm:midi :time (* 1/4 timescale (first pt)) :keynum (second pt) :duration 1/4))
           points)
   outfile
   :play nil))

(defun lines->midi (lines &key (outfile "/tmp/test.midi") (timescale 1))
  (cm:events
   (mapcar (lambda (line) (cm:new cm:midi :time (* 1/4 timescale (first line)) :keynum (second line) :duration (* 1/4 timescale (third line))))
           lines)
   outfile
   :play nil))


;;; (cm:midi-file-print "/home/orm/work/kompositionen/ast/midi/test-sine.midi")
#|

(let ((svg-file (make-instance 'svg-file))
      (xoffs 0)
      (yoffs 0)
      (xscale 1)
      (yscale -1))
  (setf (slot-value svg-file 'elements) 
        (list
         (make-piano-roll svg-file)
         (make-staff-system svg-file)
         (append 
          (list (make-instance 'svg-layer :name "Punkte" :id (new-id svg-file 'layer-ids)))
          (list
           (cons (make-instance 'svg-group :id (new-id svg-file 'group-ids))
                 (loop for point in
'((0 74) (17 71) (34 68) (51 65) (68 62) (85 59) (102 56) (119 53) (136 50)
 (153 47) (12 75) (29 72) (46 69) (63 66) (80 63) (97 60) (114 57) (131 54)
 (148 51) (165 48) (24 76) (41 73) (58 70) (75 67) (92 64) (109 61) (126 58)
 (143 55) (160 52) (177 49) (36 77) (53 74) (70 71) (87 68) (104 65) (121 62)
 (138 59) (155 56) (172 53) (189 50) (48 78) (65 75) (82 72) (99 69) (116 66)
 (133 63) (150 60) (167 57) (184 54) (201 51) (60 79) (77 76) (94 73) (111 70)
 (128 67) (145 64) (162 61) (179 58) (196 55) (213 52) (72 80) (89 77) (106 74)
 (123 71) (140 68) (157 65) (174 62) (191 59) (208 56) (225 53) (84 81)
 (101 78) (118 75) (135 72) (152 69) (169 66) (186 63) (203 60) (220 57)
 (237 54) (96 82) (113 79) (130 76) (147 73) (164 70) (181 67) (198 64)
 (215 61) (232 58) (249 55) (108 83) (125 80) (142 77) (159 74) (176 71)
 (193 68) (210 65) (227 62) (244 59) (261 56))
                    collect
                      (destructuring-bind (cx cy) point
                        (make-instance 'svg-point :cx (+ xoffs (* xscale cx)) :cy (+ yoffs (* yscale cy)) 
                                       :rx 0.5 :ry 0.5
                                       :stroke-width 0
                                       ;;                                      :stroke-color (color-lookup color) 
                                       :fill-color +black+
                                       :id (new-id svg-file 'point-ids)))))))))
  (export-svg-file svg-file))

(cm:map-objects (lambda (midi) (list :time (sv midi 'cm::time)
                                     :keynum (sv midi 'cm::keynum))) 
                (cm:import-events "/home/orm/work/kompositionen/ast/midi/test-sine.midi")
                :class 'cm:midi)


(cm:midi-file-print "/home/orm/work/kompositionen/ast/midi/test-sine.midi")

;; (svg->wav :timescale (get-sixteenth 1 104) :dynamik 10)
;; (svg->wav :timescale (get-sixteenth 1/4 60) :dynamik 10)
;; (svg->midi :timescale (get-sixteenth 1/4 60))
;; (midi->svg
(cm:map-objects (lambda (midi) (list :time (sv midi 'cm::time)
                                     :keynum (sv midi 'cm::keynum))) 
                (cm:import-events "/home/orm/work/kompositionen/ast/midi/test-sine.midi")
                :class 'cm:midi)


(mapcar
 (lambda (midi)
   (list :time (sv midi 'cm::time)
         :keynum (sv midi 'cm::keynum)))
        (cm:subobjects (cm:import-events "/home/orm/work/kompositionen/ast/midi/test-sine.midi") :class 'cm:midi))

(cm:map-objects (lambda (midi) (list :time (sv midi 'cm::time)
                                     :keynum (sv midi 'cm::keynum))) 
                (cm:import-events "/home/orm/work/kompositionen/ast/midi/test-sine.midi")
                :class 'cm:midi)

(cm:map-objects (lambda (midi) (list :time (sv midi 'cm::time)
                                     :keynum (sv midi 'cm::keynum))) 
                (cm:import-events "/home/orm/work/kompositionen/ast/midi/test-sine.midi")
                :class 'cm:midi)
|#
(defun digest-track (track &key (timescale 1) (quantize nil) (x-offs 0))
  (sort
   (mapcar
    (lambda (midi)
      (let ((time (* timescale (+ x-offs (sv midi 'cm::time)))))
        (list (if quantize (round time) time)
              (sv midi 'cm::keynum))))
    (cm:subobjects track :class 'cm:midi))
   #'(lambda (x y) (< (first x) (first y)))))

(defun digest-track-lines (track &key (timescale 1) (quantize nil) (x-offs 0))
  (sort
   (mapcar
    (lambda (midi)
      (let ((time (* timescale (+ x-offs (sv midi 'cm::time))))
            (dur (* timescale (sv midi 'cm::duration))))
        (list (if quantize (round time) time)
              (sv midi 'cm::keynum)
              (if quantize (round dur) dur))))
    (cm:subobjects track :class 'cm:midi))
   #'(lambda (x y) (< (first x) (first y)))))

(defun midi->points (&key (infile "/home/orm/work/kompositionen/ast/midi/test-sine.midi")
                       (timescale 1) (x-offs 0) (quantize nil))
  (let ((seq (cm:import-events infile)))
;;    (break "seq: ~a" seq)
    (if (consp seq)
        (remove nil (mapcar (lambda (track) (digest-track track :timescale timescale :x-offs x-offs :quantize quantize)) seq))
        (digest-track seq :timescale timescale :quantize quantize))))

(defun midi->lines (&key (infile "/home/orm/work/kompositionen/ast/midi/test-sine.midi")
                      (timescale 1) (x-offs 0) (quantize nil))
  (let ((seq (cm:import-events infile)))
    ;;    (break "seq: ~a" seq)
    (if (consp seq)
        (remove nil (mapcar (lambda (track) (digest-track-lines track :timescale timescale :x-offs x-offs :quantize quantize)) seq))
        (digest-track-lines seq :timescale timescale :quantize quantize))))

(defun midi->svg (&key (infile "/home/orm/work/kompositionen/ast/midi/test-sine.midi")
                    (outfile #P"/tmp/test.svg")
                    (timescale 1)
                    (quantize nil))
  (let ((svg-file (make-instance 'svg-file)))
    (setf (slot-value svg-file 'elements) 
          (list
           (make-piano-roll svg-file)
           (make-staff-system svg-file)
           (append 
            (list (make-instance 'svg-layer :name "Punkte" :id (new-id svg-file 'layer-ids)))
            (list->svg-points (midi->points :infile infile :timescale timescale :quantize quantize) svg-file))))
    (export-svg-file svg-file :fname outfile)))

;;; (midi->svg :infile (format nil "~a/midi/~a" *basedir* "ast01-export-4.mid"))

(defun svg->points (&key (infile #P"/tmp/test.svg") (timescale 1) (xquantize t) (yquantize t))
  "extract all circle objects (points) in the layer \"Punkte\" of svg infile.
Also removes duplicates and flattens subgroups. Points are simple
two-element lists containing x and y coordinates. The y coordinate is
supposed to be a midifloat value, x ist translated into secs/beats."
  (mapcar (lambda (x) (setf (first x) (* (first x) timescale)) x)
          (sort
           (remove-duplicates
            (ou:flatten-fn (get-points-from-file :fname infile :xquantize xquantize :yquantize yquantize) :fn #'caar)
            :test #'equal)
           (lambda (x y) (< (first x) (first y))))))


(defun svg->lines (&key (infile #P"/tmp/test.svg") (timescale 1) (xquantize t) (yquantize t))
  "extract all circle objects (points) in the layer \"Punkte\" of svg infile.
Also removes duplicates and flattens subgroups. Points are simple
two-element lists containing x and y coordinates. The y coordinate is
supposed to be a midifloat value, x ist translated into secs/beats."
  (mapcar (lambda (x) (setf (first x) (* (first x) timescale)) x)
          (sort
           (remove-duplicates
           (ou:flatten-fn (get-lines-from-file :fname infile :xquantize xquantize :yquantize yquantize) :fn #'caar)
            :test #'equal)
           (lambda (x y) (< (first x) (first y))))))

(defun xscale-points (points xscale)
  (cond ((null points) '())
        ((consp (caar points))
         (cons (xscale-points (car points) xscale)
               (xscale-points (cdr points) xscale)))
        (t (mapcar (lambda (p) (cons (* xscale (first p)) (rest p)))
                   points))))

(defun xscale-lines (lines xscale)
  (cond ((null lines) '())
        ((consp (caar lines))
         (cons (xscale-lines (car lines) xscale)
               (xscale-lines (cdr lines) xscale)))
        (t (mapcar (lambda (p) (append (list (* xscale (first p)) (second p)
                                             (* xscale (third p))) (nthcdr 3 p)))
                   lines))))

;;; (xscale-lines '(((0 3 2) (2 4 1)) (2 -1 4) (3 5 3)) 0.5)

(defun points->svg (points &key (outfile #P"/tmp/test.svg")
                             (timescale 1)
                             (color "#000000")
                             (radius 0.5))
  (let ((svg-file (make-instance 'svg-file)))
    (setf (slot-value svg-file 'elements) 
          (list
           (make-piano-roll svg-file)
           (make-staff-system svg-file)
           (append 
            (list (make-instance 'svg-layer :name "Punkte" :id (new-id svg-file 'layer-ids)))
            (list->svg-points (xscale-points points timescale) svg-file
                              :radius radius
                              :color color))
           ))
    (export-svg-file svg-file :fname outfile)))

(defun lines->svg (lines &key (outfile #P"/tmp/test.svg")
                             (timescale 1)
                             (color "#000000")
                             (stroke-width 0.5))
  (let ((svg-file (make-instance 'svg-file)))
    (setf (slot-value svg-file 'elements) 
          (list
           (make-piano-roll svg-file)
           (make-staff-system svg-file)
           (append 
            (list (make-instance 'svg-layer :name "Punkte" :id (new-id svg-file 'layer-ids)))
            (list->svg-lines (xscale-lines lines timescale) svg-file
                              :stroke-width stroke-width
                              :color color))))
    (export-svg-file svg-file :fname outfile)))

#|
(cm:import-events (format nil "~a/midi/~a" *basedir* "ast01-export-4.mid"))


(let ((midi-events (second (cm:import-events (format nil "~a/midi/~a" *basedir* "ast01-export-5.mid")))))
  (cm:map-objects #'(lambda (x) (setf (sv x 'cm::keynum)
                                      (+ 120 (* -1 (sv x 'cm::keynum)))))
                  midi-events
                  :class 'cm:midi)
  (cm:events midi-events "/tmp/invers.midi"))

;;(svg->midi :outfile "keinpuls-invers")
;; (renew-svg-file)

;; (defun svg->midi (&key (infile #P"/tmp/test.svg") (outfile "test-sine") (timescale 1/4) (dynamik 20) (play nil))
;;   (let ((path "/home/orm/work/kompositionen/ast/midi")
;;         (basename outfile))
;;     (cm:events
;;      (cons (cm:new cm:midi-tempo-change :time 0 :usecs (qpm->usecs (/ 15 timescale)))
;;            (loop 
;;               for x in (adjust-timing (ou:flatten-fn (get-points-from-file :fname infile) :fn #'caar) :timescale timescale :dynamik dynamik)
;;               collect (cm:new cm:midi
;;                         :time (first x)
;;                         :duration 1/4
;;                         :amplitude (/ (float (third x)) 96)
;;                         :keynum (second x))))
;;      (format nil "~a/~a.midi" path basename)
;;      :play play)))

(get-points-from-file :fname infile)
|#

;;; (get-wf-coords-from-string "m -73.6362,581.3953 3.5275,0")

(defun get-wf-color-from-string (str)
  (format nil "#~a" (subseq str 18 24)))

;;; (get-wf-color-from-string "fill:none;stroke:#ff6609;stroke-width:2.83455014;stroke-linecap:butt;stroke-linejoin:miter;stroke-miterlimit:10;stroke-dasharray:none;stroke-opacity:1")

(defun get-wf-coords-from-string (str)
  (destructuring-bind (x y)
      (read-from-string
       (format nil "(~a)"
               (substitute #\SPACE #\,
                           (coerce (loop for x across (string-left-trim '(#\m #\SPACE) str)
                                      until (char= x #\SPACE)
                                      collect x)
                                   'string))))
    (list (round (- (* 24/15 0.2822 (+ x 73.6362)) 42 -8.21))
          (float (+ -247.5 (/ (round (* 2 0.6349157 14/15 (+ y -581.3953) -1)) 2))))))

(defun get-webfehler-coords (node transformation)
  (destructuring-bind ((x y) z)
      (list
       (apply #'vec-mtx-mult
              (list
               (get-wf-coords-from-string (cxml-stp:value (cxml-stp:find-attribute-named node "d")))
               transformation))
       (get-wf-color-from-string (cxml-stp:value (cxml-stp:find-attribute-named node "style"))))
    (list x y z)))

(defun collect-webfehler-points (layer &optional (transformation nil))
  (let ((result '()))
    (cxml-stp:map-children
     'list
     (lambda (child)
       (cond
         ((group? child) (let ((inner-transformation (update-transformation transformation child)))
                           (push (collect-webfehler-points child inner-transformation) result)))
         ((path? child) (push (get-webfehler-coords child transformation) result))))
     layer)
    (reverse result)))

(defun group-points (points)
  (let ((mytracks '(92)))
    (labels ((get-track (pitch)
;;               (break "mytracks: ~a" mytracks)
               (loop
                  for x in mytracks
                  for count from 0
                  do (progn
;;                       (break "x: ~a, pitch: ~a mytracks: ~a" x pitch mytracks)
                       (if (< (abs (- x pitch)) 4)
                           (return (progn (setf mytracks
                                                (append (butlast mytracks
                                                                 (- (length mytracks) count))
                                                        (list pitch) (nthcdr (+ count 1) mytracks)))
                                          count))))
                  finally (return (progn
                                    (setf mytracks (append mytracks (list pitch)))
                                    (+ count 1))))))
      (loop for x in points
         with result = '(nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil)
         do (push x (nth (get-track (second x)) result))
         finally (return (mapcar #'reverse result))))))

(defun group-to-chords (l)
  (loop
     for evt in l
     with curr-chord = '()
     with curr-time = (caar l)
     append (progn
;;              (break "evt: ~a, curr-chd: ~a, curr-time: ~a, chordlist: ~a" evt curr-chord curr-time chordlist)
                   (if (= (first evt) curr-time)
                       (progn
                         (push (second evt) curr-chord)
                         nil)
                       (prog1
                           (list (reverse curr-chord))
                         (setf curr-time (first evt))
                         (setf curr-chord (list (second evt))))))
     into chordlist
     finally (return (append chordlist (list curr-chord)))))

;;; -73.6362 581.3953 "#ff6609") (-70.1087 590.8433 "#ff6609")

;;; (/ (- 73.6362 70.1087)) 0.28348717

;;; (/ (- 581.3953 579.8203)) 0.6349157





;; (* (/ 450 454) 0.28348717) 




;;; Klänge werden kürzer und bekommen mikrotonale Dispersion! ->
;;; Akzenten, die dann wie in einem Zoom wieder die gleichen
;;; Entwicklungen machen...


#|
(svg->wav :infile #P"/tmp/test2.svg" :outfile "ast-schluss01" :timescale 1/16)

(svg->wav :infile #P"/tmp/test-midi-neu02.svg" :outfile "ast-anfang-test02" :timescale 1/8)

(cm:midi-file-print "/home/orm/work/kompositionen/ast/midi/test-sine.midi")

(midi->svg :infile "/home/orm/work/kompositionen/ast/midi/ast01-export-6.mid" :outfile #P "/tmp/test-midi.svg")


(svg->lwav :infile #P"/tmp/test3.svg" :outfile "ast-schluss01" :timescale 1 :quantize nil :db -12)

(adjust-timing
 (remove-duplicates
  (ou:flatten-fn (get-points-from-file :fname #P"/tmp/test3.svg" :quantize nil) :fn #'caar)
  :test #'equal)
 :timescale 1 :dynamik 0)

(mapcar #'second
        '((0 12) (12 5) (17 7) (24 4) (28 6) (34 2) (36 3) (39 7) (46 2) (48 2) (50 1)
          (51 7) (58 1) (59 2) (61 1) (62 6) (68 2) (70 2) (72 1) (73 3) (76 4) (80 2)
          (82 2) (84 1) (85 1) (86 5) (91 1) (92 1) (93 1) (94 1) (95 1) (96 6) (102 1)
          (103 1) (104 2) (106 1) (107 1) (108 2) (110 4) (114 1) (115 1) (116 1)
          (117 1) (118 1) (119 1) (120 4) (124 1) (125 1) (126 1) (127 1) (128 1)
          (129 1) (130 1) (131 5) (136 1) (137 1) (138 2) (140 1) (141 1) (142 2)
          (144 4) (148 1) (149 1) (150 1) (151 1) (152 1) (153 1) (154 4) (158 1)
          (159 1) (160 1) (161 1) (162 1) (163 1) (164 1) (165 5) (170 1) (171 1)
          (172 2) (174 1) (175 1) (176 2) (178 4) (182 1) (183 1) (184 1) (185 1)
          (186 1) (187 1) (188 4) (192 1) (193 1) (194 1) (195 1) (196 1) (197 1)
          (198 1) (199 5) (204 1) (205 1) (206 2) (208 1) (209 1) (210 2) (212 4)
          (216 1) (217 1) (218 1) (219 1) (220 1) (221 1) (222 4) (226 1) (227 1)
          (228 1) (229 1) (230 1) (231 1) (232 1) (233 5) (238 1) (239 1) (240 2)
          (242 1) (243 1) (244 2) (246 4) (250 1) (251 1) (252 1) (253 1) (254 1)
          (255 1) (256 4) (260 1) (261 1) (262 1) (263 1) (264 1) (265 1) (266 1)
          (267 6) (273 1) (274 2) (276 1) (277 1) (278 2) (280 5) (285 1) (286 1)
          (287 1) (288 2) (290 4) (294 1) (295 2) (297 1) (298 1) (299 2) (301 6)
          (307 3) (310 2) (312 2) (314 5) (319 2) (321 3) (324 4) (328 1) (329 3)
          (332 3) (335 6) (341 5) (346 7) (353 2) (355 7) (362 7)))

(nthcdr 42
        '(12 5 7 4 6 2 3 7 2 2 1 7 1 2 1 6 2 2 1 3 4 2 2 1 1 5 1 1 1 1 1 6

          1 1 2 1 1 2 4
               1 1 1 1 1 1 4 1 1 1 1 1 1 1 5 1 1 2 1 1 2 4 1 1 1 1 1 1 4 1 1 1 1 1 1 1 5 1 1
               2 1 1 2 4 1 1 1 1 1 1 4 1 1 1 1 1 1 1 5 1 1 2 1 1 2 4 1 1 1 1 1 1 4 1 1 1 1 1
               1 1 5 1 1 2 1 1 2 4 1 1 1 1 1 1 4 1 1 1 1 1 1 1 6 1 2 1 1 2 5 1 1 1 2 4 1 2 1
               1 2 6 3 2 2 5 2 3 4 1 3 3 6 5 7 2 7 7))

(1 1 1 4 1 1 1 1 1 1 1 5 1 1 2 1 1 2 4 1 1 1 1 1 1 4 1 1 1 1 1 1 1 5 1 1 2 1 1
 2 4 1 1 1 1 1 1 4 1 1 1 1 1 1 1 5 1 1 2 1 1 2 4 1 1 1 1 1 1 4 1 1 1 1 1 1 1 5
 1 1 2 1 1 2 4 1 1 1 1 1 1 4 1 1 1 1 1 1 1 6 1 2 1 1 2 5 1 1 1 2 4 1 2 1 1 2 6
 3 2 2 5 2 3 4 1 3 3 6 5 7 2 7 7)


(let ((tracks '(1 2 3)))
  (setf (nth 2 tracks) 5)
  tracks)

(nthcdr 2 '(1 2 3 4 5))



(group-points (sort
               (svg->points :infile #P"/tmp/test-midi-neu03.svg" :timescale 1)
               (lambda (x y) (< (first x) (first y)))))

((0 0 93.0) (0 12 94.0) (1 17 90.0) (0 24 95.0) (1 28 91.0) (2 34 85.0)
 (0 36 96.0) (1 39 92.0) (2 46 86.0) (0 48 97.0) (1 50 93.0) (3 51 82.0)
 (2 58 87.0) (0 59 98.0) (1 61 94.0) (3 62 83.0) (4 68 77.0) (2 70 88.0)
 (0 72 99.0) (3 73 84.0) (1 76 95.0) (4 80 78.0) (2 82 89.0) (3 84 85.0)
 (5 85 74.0) (0 86 102.0) (0 86 100.0) (1 91 96.0) (4 92 79.0) (2 93 90.0)
 (0 94 103.0) (0 94 101.0) (3 95 86.0) (5 96 75.0) (6 102 69.0) (1 103 97.0)
 (4 104 80.0) (2 106 91.0) (5 107 76.0) (0 108 104.0) (0 108 102.0)
 (3 110 87.0) (6 113 70.0) (1 114 98.0) (4 115 81.0) (0 116 105.0)
 (0 116 103.0) (5 117 77.0) (7 118 66.0) (2 119 94.0) (2 119 92.0) (1 122 99.0)
 (3 123 88.0) (6 124 71.0) (4 125 82.0) (2 126 95.0) (2 126 93.0) (5 127 78.0)
 (7 128 67.0) (0 129 106.0) (0 129 104.0) (1 132 100.0) (0 133 105.0)
 (3 134 89.0) (6 135 72.0) (1 136 101.0) (4 137 83.0) (7 138 68.0) (2 139 96.0)
 (2 139 94.0) (5 141 79.0) (0 143 106.0) (3 144 90.0) (6 145 73.0) (2 146 97.0)
 (2 146 95.0) (7 147 69.0) (1 148 102.0) (4 149 86.0) (4 149 84.0) (3 150 91.0)
 (5 151 80.0) (0 152 107.0) (6 153 74.0) (4 154 87.0) (4 154 85.0) (7 155 70.0)
 (1 156 103.0) (2 157 98.0) (2 157 96.0) (3 158 92.0) (2 159 97.0) (5 160 81.0)
 (0 161 108.0) (3 162 93.0) (6 163 75.0) (1 164 104.0) (4 165 88.0)
 (4 165 86.0) (7 166 71.0) (2 167 98.0) (5 168 82.0) (0 169 109.0) (4 170 89.0)
 (4 170 87.0) (1 171 105.0) (3 172 94.0) (6 173 78.0) (6 173 76.0) (5 174 83.0)
 (7 175 72.0) (2 176 99.0) (0 177 110.0) (6 178 79.0) (6 178 77.0)
 (1 179 106.0) (3 180 95.0) (4 181 90.0) (4 181 88.0) (5 182 84.0) (4 183 89.0)
 (7 184 73.0) (2 185 100.0) (5 186 85.0) (0 187 111.0) (3 188 96.0)
 (6 189 80.0) (6 189 78.0) (1 190 107.0) (4 191 90.0) (7 192 74.0)
 (2 193 101.0) (6 194 81.0) (6 194 79.0) (3 195 97.0) (5 196 86.0)
 (0 197 114.0) (0 197 112.0) (7 198 75.0) (1 199 108.0) (4 200 91.0)
 (2 201 102.0) (0 202 115.0) (0 202 113.0) (3 203 98.0) (5 204 87.0)
 (6 205 82.0) (6 205 80.0) (7 206 76.0) (6 207 81.0) (1 208 109.0) (4 209 92.0)
 (7 210 77.0) (2 211 103.0) (5 212 88.0) (0 213 116.0) (0 213 114.0)
 (6 214 82.0) (3 215 99.0) (7 215 78.0) (1 216 110.0) (4 217 93.0)
 (0 218 117.0) (0 218 115.0) (5 219 89.0) (2 220 106.0) (2 220 104.0)
 (1 221 111.0) (3 222 100.0) (6 223 83.0) (4 224 94.0) (2 225 107.0)
 (2 225 105.0) (5 226 90.0) (7 227 79.0) (0 228 118.0) (0 228 116.0)
 (3 229 101.0) (6 230 84.0) (4 232 95.0) (7 233 80.0) (1 234 108.0)
 (1 234 106.0) (5 236 91.0) (2 241 102.0) (6 242 85.0) (1 243 109.0)
 (1 243 107.0) (7 244 81.0) (3 246 98.0) (3 246 96.0) (2 250 103.0)
 (4 251 92.0) (6 253 86.0) (3 254 99.0) (3 254 97.0) (7 255 82.0) (1 257 110.0)
 (1 257 108.0) (4 263 93.0) (6 266 87.0) (2 268 100.0) (2 268 98.0)
 (7 270 83.0) (3 275 94.0) (2 277 101.0) (2 277 99.0) (4 280 90.0) (4 280 88.0)
 (3 284 95.0) (6 285 84.0) (4 288 91.0) (4 288 89.0) (2 291 102.0)
 (2 291 100.0) (6 297 85.0) (3 305 92.0) (3 305 90.0) (4 312 86.0) (3 314 93.0)
 (3 314 91.0) (4 321 87.0) (3 328 94.0) (3 328 92.0))



(svg->wav :infile #P"/tmp/test-midi-neu03.svg" :outfile "ast-anfang-test04" :timescale 1/4)

(svg->wav :infile #P"/tmp/test-midi-neu02.svg" :outfile "ast-anfang-test05" :timescale 1/4 :dynamik 0)
(svg->midi :infile #P"/tmp/test-midi-neu02.svg" :outfile "ast-anfang-test05" :timescale 1/4)

(points->svg (group-points (sort
               (svg->points :infile #P"/tmp/test-midi-neu03.svg" :timescale 1)
               (lambda (x y) (< (first x) (first y)))))
             :outfile "/tmp/test-midi-neu03-grouped.svg")

(svg->points
 :outfile "/tmp/test-midi-neu03-grouped.svg")

 
(length (svg->points :infile #P"/tmp/test-midi-neu04.svg" :timescale 1)


        )

(svg->points :infile #P"/tmp/test-midi-neu04.svg" :timescale 1)

(mapcar #'(lambda (x) (list (- (first x) 293) (second x)))
        '((293 42.0) (293 40.0) (298 43.0) (298 41.0) (295 36.0) (297 30.0) (299 26.0)
          (296 19.0) (300 15.0) (309 44.0) (309 42.0) (315 45.0) (315 43.0) (304 37.0)
          (313 38.0) (318 39.0) (307 31.0) (317 34.0) (317 32.0) (322 35.0) (322 33.0)
          (312 27.0) (319 28.0) (305 20.0) (314 21.0) (321 22.0) (308 16.0) (316 17.0)
          (323 18.0) (301 10.0) (301 8.0) (303 9.0) (310 10.0) (320 11.0) (325 44.0)
          (335 46.0) (344 47.0) (353 48.0) (361 49.0) (369 50.0) (379 51.0) (324 40.0)
          (328 41.0) (340 42.0) (348 43.0) (356 44.0) (363 45.0) (371 46.0) (382 47.0)
          (331 36.0) (331 34.0) (338 37.0) (338 35.0) (349 38.0) (349 36.0) (351 37.0)
          (359 38.0) (368 39.0) (377 40.0) (385 41.0) (326 29.0) (336 30.0) (342 31.0)
          (350 32.0) (354 33.0) (364 34.0) (372 35.0) (380 36.0) (387 37.0) (329 23.0)
          (341 26.0) (341 24.0) (346 27.0) (346 25.0) (357 28.0) (357 26.0) (362 29.0)
          (362 27.0) (373 30.0) (373 28.0) (375 29.0) (383 30.0) (333 19.0) (343 20.0)
          (352 21.0) (360 22.0) (366 23.0) (374 24.0) (378 25.0) (388 26.0) (327 12.0)
          (337 13.0) (345 14.0) (355 15.0) (365 18.0) (365 16.0) (370 19.0) (370 17.0)
          (381 20.0) (381 18.0) (386 21.0) (386 19.0) (330 8.0) (339 9.0) (347 10.0)
          (358 11.0) (367 12.0) (376 13.0) (384 14.0)))

(sort '((0 42.0) (0 40.0) (5 43.0) (5 41.0) (2 36.0) (4 30.0) (6 26.0) (3 19.0)
        (7 15.0) (16 44.0) (16 42.0) (22 45.0) (22 43.0) (11 37.0) (20 38.0) (25 39.0)
        (14 31.0) (24 34.0) (24 32.0) (29 35.0) (29 33.0) (19 27.0) (26 28.0)
        (12 20.0) (21 21.0) (28 22.0) (15 16.0) (23 17.0) (30 18.0) (8 10.0) (8 8.0)
        (10 9.0) (17 10.0) (27 11.0) (32 44.0) (42 46.0) (51 47.0) (60 48.0) (68 49.0)
        (76 50.0) (86 51.0) (31 40.0) (35 41.0) (47 42.0) (55 43.0) (63 44.0)
        (70 45.0) (78 46.0) (89 47.0) (38 36.0) (38 34.0) (45 37.0) (45 35.0)
        (56 38.0) (56 36.0) (58 37.0) (66 38.0) (75 39.0) (84 40.0) (92 41.0)
        (33 29.0) (43 30.0) (49 31.0) (57 32.0) (61 33.0) (71 34.0) (79 35.0)
        (87 36.0) (94 37.0) (36 23.0) (48 26.0) (48 24.0) (53 27.0) (53 25.0)
        (64 28.0) (64 26.0) (69 29.0) (69 27.0) (80 30.0) (80 28.0) (82 29.0)
        (90 30.0) (40 19.0) (50 20.0) (59 21.0) (67 22.0) (73 23.0) (81 24.0)
        (85 25.0) (95 26.0) (34 12.0) (44 13.0) (52 14.0) (62 15.0) (72 18.0)
        (72 16.0) (77 19.0) (77 17.0) (88 20.0) (88 18.0) (93 21.0) (93 19.0) (37 8.0)
        (46 9.0) (54 10.0) (65 11.0) (74 12.0) (83 13.0) (91 14.0))
      (lambda (x y) (or
                     (< (first x) (first y))
                     (and (= (first x) (first y))
                          (< (second x) (second y))))))

;;; modulo:
;; y-offs: 44
;; x/y-offs: 96/12

(defparameter *modulo*
  '((0 40.0) (0 42.0) (2 36.0) (3 19.0) (4 30.0) (5 41.0) (5 43.0) (6 26.0)
    (7 15.0) (8 8.0) (8 10.0) (10 9.0) (11 37.0) (12 20.0) (14 31.0) (15 16.0)
    (16 42.0) (16 44.0) (17 10.0) (19 27.0) (20 38.0) (21 21.0) (22 43.0)
    (22 45.0) (23 17.0) (24 32.0) (24 34.0) (25 39.0) (26 28.0) (27 11.0)
    (28 22.0) (29 33.0) (29 35.0) (30 18.0) (31 40.0) (32 44.0) (33 29.0)
    (34 12.0) (35 41.0) (36 23.0) (37 8.0) (38 34.0) (38 36.0) (40 19.0) (42 46.0)
    (43 30.0) (44 13.0) (45 35.0) (45 37.0) (46 9.0) (47 42.0) (48 24.0) (48 26.0)
    (49 31.0) (50 20.0) (51 47.0) (52 14.0) (53 25.0) (53 27.0) (54 10.0)
    (55 43.0) (56 36.0) (56 38.0) (57 32.0) (58 37.0) (59 21.0) (60 48.0)
    (61 33.0) (62 15.0) (63 44.0) (64 26.0) (64 28.0) (65 11.0) (66 38.0)
    (67 22.0) (68 49.0) (69 27.0) (69 29.0) (70 45.0) (71 34.0) (72 16.0)
    (72 18.0) (73 23.0) (74 12.0) (75 39.0) (76 50.0) (77 17.0) (77 19.0)
    (78 46.0) (79 35.0) (80 28.0) (80 30.0) (81 24.0) (82 29.0) (83 13.0)
    (84 40.0) (85 25.0) (86 51.0) (87 36.0) (88 18.0) (88 20.0) (89 47.0)
    (90 30.0) (91 14.0) (92 41.0) (93 19.0) (93 21.0) (94 37.0) (95 26.0)))

(points->svg
 (loop
    for x-offs from 0 by 96
    for y-offs1 from -120 by 12
    for x-tile-count below 10
    append (loop
              for y-offs2 from y-offs1 by 44
              for y-tile-count below 6
              append (mapcar (lambda (point)
                               (destructuring-bind (x y) point
                                 (list (+ x x-offs) (+ y y-offs2))))
                             *modulo*)))
 :outfile "/tmp/tile-pattern.svg")


;;; Schluss:

(points->svg '((0 18.0) (0 45.5) (0 49.0) (0 60.5) (0 66.5) (0 67.5) (1 15.5) (1 47.0) (1 49.5) (1 61.0) (1 67.0) (1 68.0) (2 7.0) (2 48.0) (2 49.5) (2 61.5) (2 68.0) (2 68.5) (3 19.0) (3 46.5) (3 50.0) (3 61.5) (3 67.5) (3 68.5) (4 12.5) (4 48.0) (4 50.0) (4 62.0) (4 68.5) (4 69.0) (5 19.5) (5 48.5) (5 51.5) (5 62.0) (5 68.5) (5 69.5) (6 17.0) (6 48.5) (6 51.0) (6 62.5) (6 68.5) (6 69.5) (7 8.5) (7 49.5) (7 51.0) (7 63.0) (7 69.5) (7 70.0) (8 20.5) (8 48.0) (8 51.5) (8 63.0) (8 69.0) (8 70.0) (9 14.0) (9 49.5) (9 51.5) (9 63.5) (9 70.0) (9 70.5) (10 21.0) (10 50.0) (10 53.0) (10 63.5) (10 70.0) (10 71.0) (11 19.0) (11 50.5) (11 53.0) (11 64.5) (11 70.5) (11 71.5) (12 22.0) (12 49.5) (12 53.0) (12 64.5) (12 70.5) (12 71.5) (13 19.5) (13 51.0) (13 53.5) (13 65.0) (13 71.0) (13 72.0) (14 11.0) (14 52.0) (14 53.5) (14 65.5) (14 72.0) (14 72.5) (15 23.0) (15 50.5) (15 54.0) (15 65.5) (15 71.5) (15 72.5) (16 16.5) (16 52.0) (16 54.0) (16 66.0) (16 72.5) (16 73.0) (17 23.5) (17 52.5) (17 55.5) (17 66.0) (17 72.5) (17 73.5) (18 21.0) (18 52.5) (18 55.0) (18 66.5) (18 72.5) (18 73.5) (19 12.5) (19 53.5) (19 55.0) (19 67.0) (19 73.5) (19 74.0) (20 24.5) (20 52.0) (20 55.5) (20 67.0) (20 73.0) (20 74.0) (21 18.0) (21 53.5) (21 55.5) (21 67.5) (21 74.0) (21 74.5) (22 25.0) (22 54.0) (22 57.0) (22 67.5) (22 74.0) (22 75.0) (23 23.0) (23 54.5) (23 57.0) (23 68.5) (23 74.5) (23 75.5) (24 25.5) (24 53.0) (24 56.5) (24 68.0) (24 74.0) (24 75.0) (25 19.0) (25 55.5) (25 57.5) (25 68.5) (25 75.0) (25 76.0) (26 26.5) (26 54.0) (26 57.5) (26 69.0) (26 75.0) (26 76.0) (27 36.5) (27 51.5) (27 57.5) (27 70.0) (27 75.0) (27 77.0) (28 27.5) (28 50.5) (28 54.5) (28 70.0) (28 75.0) (28 76.0) (29 33.0) (29 48.5) (29 54.5) (29 70.5) (29 75.0) (29 76.5) (30 28.5) (30 51.5) (30 55.5) (30 71.0) (30 76.0) (30 77.0) (31 31.5) (31 50.5) (31 55.5) (31 71.5) (31 76.0) (31 77.0) (32 28.5) (32 56.0) (32 59.5) (32 71.0) (32 77.0) (32 78.0) (33 26.0) (33 57.5) (33 60.0) (33 71.5) (33 77.5) (33 78.5) (34 17.5) (34 58.5) (34 60.0) (34 72.0) (34 78.5) (34 79.0) (35 29.5) (35 57.0) (35 60.5) (35 72.0) (35 78.0) (35 79.0) (36 23.0) (36 58.5) (36 60.5) (36 72.5) (36 79.0) (36 79.5) (37 30.0) (37 59.0) (37 62.0) (37 72.5) (37 79.0) (37 80.0) (38 28.0) (38 59.5) (38 62.0) (38 73.5) (38 79.5) (38 80.5) (39 30.5) (39 58.0) (39 61.5) (39 73.0) (39 79.0) (39 80.0) (40 24.0) (40 60.5) (40 62.5) (40 73.5) (40 80.0) (40 81.0) (41 31.5) (41 59.0) (41 62.5) (41 74.0) (41 80.0) (41 81.0) (42 41.5) (42 56.5) (42 62.5) (42 75.0) (42 80.0) (42 82.0) (43 32.5) (43 55.5) (43 59.5) (43 75.0) (43 80.0) (43 81.0) (44 38.0) (44 53.5) (44 59.5) (44 75.5) (44 80.0) (44 81.5) (45 33.5) (45 56.5) (45 60.5) (45 76.0) (45 81.0) (45 82.0) (46 36.5) (46 55.5) (46 60.5) (46 76.5) (46 81.0) (46 82.0) (47 33.5) (47 61.0) (47 64.5) (47 76.0) (47 82.0) (47 83.0) (48 31.0) (48 62.5) (48 65.0) (48 76.5) (48 82.5) (48 83.5) (49 22.5) (49 63.5) (49 65.0) (49 77.0) (49 83.5) (49 84.0) (50 34.5) (50 62.0) (50 65.5) (50 77.0) (50 83.0) (50 84.0) (51 28.0) (51 63.5) (51 65.5) (51 77.5) (51 84.0) (51 84.5) (52 35.0) (52 64.0) (52 67.0) (52 77.5) (52 84.0) (52 85.0) (53 33.0) (53 64.5) (53 67.0) (53 78.5) (53 84.5) (53 85.5) (54 35.5) (54 63.0) (54 66.5) (54 78.0) (54 84.0) (54 85.0) (55 29.0) (55 65.5) (55 67.5) (55 78.5) (55 85.0) (55 86.0) (56 36.5) (56 64.0) (56 67.5) (56 79.0) (56 85.0) (56 86.0) (57 46.5) (57 61.5) (57 67.5) (57 80.0) (57 85.0) (57 87.0) (58 37.5) (58 60.5) (58 64.5) (58 80.0) (58 85.0) (58 86.0) (59 43.0) (59 58.5) (59 64.5) (59 80.5) (59 85.0) (59 86.5) (60 38.5) (60 61.5) (60 65.5) (60 81.0) (60 86.0) (60 87.0) (61 41.5) (61 60.5) (61 65.5) (61 81.5) (61 86.0) (61 87.0) (62 38.5) (62 66.0) (62 69.5) (62 81.0) (62 87.0) (62 88.0) (63 36.0) (63 67.5) (63 70.0) (63 81.5) (63 87.5) (63 88.5) (64 27.5) (64 68.5) (64 70.0) (64 82.0) (64 88.5) (64 89.0) (65 39.5) (65 67.0) (65 70.5) (65 82.0) (65 88.0) (65 89.0) (66 33.0) (66 68.5) (66 70.5) (66 82.5) (66 89.0) (66 89.5) (67 40.0) (67 69.0) (67 72.0) (67 82.5) (67 89.0) (67 90.0) (68 38.0) (68 69.5) (68 72.0) (68 83.5) (68 89.5) (68 90.5) (69 40.5) (69 68.0) (69 71.5) (69 83.0) (69 89.0) (69 90.0) (70 34.0) (70 70.5) (70 72.5) (70 83.5) (70 90.0) (70 91.0) (71 41.5) (71 69.0) (71 72.5) (71 84.0) (71 90.0) (71 91.0) (72 51.5) (72 66.5) (72 72.5) (72 85.0) (72 90.0) (72 92.0) (73 42.5) (73 65.5) (73 69.5) (73 85.0) (73 90.0) (73 91.0) (74 48.0) (74 63.5) (74 69.5) (74 85.5) (74 90.0) (74 91.5) (75 43.5) (75 66.5) (75 70.5) (75 86.0) (75 91.0) (75 92.0) (76 46.5) (76 65.5) (76 70.5) (76 86.5) (76 91.0) (76 92.0)))

(float (/ 400 76))

(let* ((num 76)
       (sum (/ (reduce #'+ (loop for x below num collect (* 15000 (expt 1/30 (/ x (- num 1)))))) 1000)))
  (format nil "~2,'0d:~2,'0d.~3,'0d" (floor sum 60) (floor (mod sum 60)) (round (* 1000 (mod sum 1.0)))))

(let ((num 190))
  (/ (reduce #'+ (loop for x below num collect (* 6000 (expt 1/30 (/ x (- num 1)))))) 1000))

(points->svg
 (let* ((num 190)
        (times (mapcar (lambda (x) (/ x 1000))
                       (ou:integrate (loop for x below num collect (* 6000 (expt 1/60 (/ x (- num 1))))))))
        (chordlist
         '((8.644881 12.741365 17.673746 21.941282 25.741362 29.945377 31.938675
            35.945377 36.874855 39.970665 43.258636 47.69259 48.94538 49.85141 50.507847
            50.556107 51.985344 52.35934 52.97066 53.692596 55.115273 55.637978 56.25864
            57.033188 58.02887 62.531998 63.481186 64.985344 66.07615 66.6926)
           (9.058519 12.176456 23.789017 29.945377 30.938679 35.24706 39.738396 42.679886
            42.94538 47.69259 47.891727 49.458046 49.63797 49.786125 51.359344 54.564903
            57.481194 58.506165 60.692596 61.712204)
           (2.644884 9.058519 12.176456 17.741364 20.058517 23.789017 25.938675 29.945377
            30.938679 35.24706 39.738396 42.679886 42.94538 44.556114 46.35934 47.69259
            47.891727 48.258636 49.458046 49.63797 49.786125 51.359344 52.028862 53.67988
            54.564903 57.481194 58.506165 60.07616 60.692596 61.712204)
           (5.6448784 9.058519 10.176458 17.741364 20.058517 25.938675 26.368864
            30.938679 31.792694 35.24706 41.938683 42.679886 43.94538 46.247063 46.35934
            48.258636 48.378216 49.298023 49.786125 50.078373 50.914185 51.359344
            52.564903 53.67988 55.028862 58.303288 59.282055 60.78613 61.692596
            62.359337)
           (10.176458 12.741365 13.176458 15.058514 23.741364 24.789013 25.938675
            31.792694 31.938675 36.874855 40.738403 43.258636 43.94538 46.247063 46.35934
            48.378216 48.679882 48.89173 50.458046 50.507847 50.914185 52.35934 52.564903
            54.258644 55.5649 58.303288 59.506172 60.78613 61.692596 62.712204)
           (10.176458 12.741365 15.058514 23.058517 24.789013 25.938675 29.789017
            31.792694 35.945377 37.789017 39.970665 43.258636 43.94538 46.247063 46.35934
            48.378216 48.679882 48.89173 50.458046 50.914185 51.985344 52.564903
            53.692596 55.458046 56.679886 58.303288 59.506172 60.78613 61.692596
            63.45804)
           (4.644868 14.176472 15.941282 19.741362 22.058521 25.78901 29.945377 32.938683
            33.556107 36.78901 38.44724 40.938675 42.94538 43.851414 46.970665 47.69259
            49.076157 49.63797 50.25864 51.241024 51.458046 53.359344 54.02887 55.679886
            56.564907 57.481194 58.985336 60.692596 61.359344 62.45804)
           (10.058521 11.176452 18.74136 19.67375 26.938677 27.368866 27.941282 32.792694
            34.941277 35.945377 39.447243 40.938675 43.679886 43.792694 47.359344
            47.450603 49.258644 49.340446 49.378223 51.078377 52.24102 53.564903
            53.692596 55.851414 57.115273 57.667706 58.740498 60.378216 61.359344
            62.85141)
           (10.058521 15.176462 16.673738 18.74136 26.938677 27.941282 29.7384 34.55611
            35.945377 37.789017 39.447243 41.938683 43.679886 45.556107 47.359344
            48.89173 49.258644 50.076153 50.637966 51.71221 52.24102 53.692596 54.115265
            55.851414 57.564903 58.481194 59.506172 61.076157 62.359337 63.45804)
           (5.6448784 15.176462 16.941278 20.741364 23.058517 26.789011 30.945381
            33.93868 34.55611 37.789017 39.447243 41.938683 43.94538 44.85141 47.970665
            48.692596 50.076153 50.637966 51.258644 52.24102 52.45804 54.359337 55.028862
            56.679886 57.564903 58.481194 59.98534 61.692596 62.359337 63.45804)
           (-6.8623466 16.941278 19.67375 20.741364 23.058517 23.941286 30.945381
            32.792694 33.93868 39.447243 40.368862 44.85141 44.945377 45.792698 48.692596
            49.378223 49.450615 51.258644 51.85141 51.914185 52.24102 54.359337 54.507217
            56.679886 57.115273 59.303295 60.74049 62.37822 62.69259 64.07837)
           (13.176458 16.176453 18.058514 19.789015 26.741358 27.789013 30.945381
            34.938683 36.247055 38.78901 40.97066 42.93868 45.45805 45.792698 48.692596
            48.970665 50.786133 51.679886 51.914185 52.985336 53.458046 55.359344 55.5649
            57.258636 58.5649 59.303295 60.985336 62.37822 63.359344 64.458046)
           (13.673744 16.176453 17.673746 17.941292 27.789013 27.938679 28.941278
            34.938683 35.556118 38.78901 40.447235 42.93868 45.851418 46.55611 48.359344
            48.970665 51.07615 51.115273 51.637974 53.24102 53.458046 55.115273 55.359344
            56.85141 58.5649 59.481194 60.985336 62.076157 63.359344 64.458046)
           (12.176456 13.673744 17.941292 20.67374 24.941277 27.938679 28.941278
            33.792698 34.938683 40.447235 41.36887 45.851418 45.945374 48.247055
            48.359344 50.37822 50.450607 51.115273 52.85141 52.914177 53.24102 54.564903
            55.359344 56.85141 58.115265 60.30329 61.740486 62.786133 63.69258 65.07837)
           (5.6448784 15.741365 18.058514 20.67374 24.941277 26.741358 28.93868 32.945377
            34.938683 39.874855 41.36887 45.945374 46.258644 47.556118 49.35934 50.450607
            50.692596 51.679886 52.63797 52.85141 53.50785 55.028862 55.359344 57.258636
            58.115265 60.4812 61.740486 63.07615 63.69258 65.07837)
           (5.6448784 12.058519 15.176462 20.741364 23.058517 26.789011 28.93868
            32.945377 33.93868 38.24706 42.738403 45.679882 45.945374 47.556118 49.35934
            50.692596 50.891724 51.258644 52.45804 52.63797 52.786125 54.359337 55.028862
            56.679886 57.564903 60.4812 61.506165 63.07615 63.69258 64.712204)
           (-5.862336 14.741358 23.941286 25.058521 26.789011 27.741365 31.945381
            33.792698 37.94538 39.789013 41.97067 45.258636 45.945374 46.792694 49.692593
            50.37822 50.891724 51.85141 52.45804 52.914177 53.98534 55.50722 55.692593
            58.258644 58.67989 60.30329 61.506165 63.37822 63.69258 65.45805)
           (6.644886 14.741358 21.673754 23.941286 27.741365 29.36887 31.945381 35.556118
            37.94538 41.97067 42.368866 45.258636 46.94538 48.556114 49.692593 51.07615
            51.85141 52.298023 53.078377 53.98534 54.170918 55.692593 56.028866 58.258644
            59.115273 61.133675 62.282043 64.07616 64.6926 66.07837)
           (6.644886 13.058521 16.176453 21.74136 24.05852 27.789013 29.938675 33.945374
            34.938683 39.247063 43.738403 46.67989 46.94538 48.556114 50.359337 51.69259
            51.89173 52.258636 53.458046 53.63797 53.786125 55.359344 56.028866 57.679882
            58.5649 61.4812 62.50618 64.07616 64.6926 65.712204)
           (9.644863 13.058521 14.176472 21.74136 24.05852 29.938675 30.368866 34.938683
            35.79269 39.247063 45.93868 46.67989 47.945377 50.24706 50.359337 52.258636
            52.378212 53.29802 53.786125 54.078377 54.914185 55.359344 56.564907
            57.679882 59.028866 62.3033 63.28205 64.786125 65.69259 66.359344)
           (14.176472 16.741362 17.17646 19.058516 27.741365 28.789015 29.938675 35.79269
            35.938675 40.874863 44.738403 47.25864 47.945377 50.24706 50.359337 52.378212
            52.679886 52.891724 54.458046 54.50785 54.914185 56.359344 56.564907
            58.258644 59.564903 62.3033 63.506172 64.786125 65.69259 66.71221)
           (14.176472 16.741362 19.058516 27.058514 28.789015 29.938675 33.789013
            35.79269 39.94538 41.789017 43.970665 47.25864 47.945377 50.24706 50.359337
            52.378212 52.679886 52.891724 54.458046 54.914185 55.985344 56.564907
            57.692596 59.458042 60.679882 62.3033 63.506172 64.786125 65.69259 67.458046)
           (8.644881 18.176456 19.941284 23.741364 26.058517 29.789017 33.945374
            36.938675 37.55611 40.789017 42.447243 44.938683 46.94538 47.85141 50.97067
            51.69259 53.076157 53.63797 54.258644 55.241028 55.458046 57.359337 58.02887
            59.679886 60.564903 61.4812 62.985336 64.6926 65.35934 66.45805)
           (14.058517 15.176462 22.741362 23.673746 30.938679 31.368862 31.941284
            36.792694 38.941284 39.94538 43.447235 44.938683 47.679886 47.79269 51.359344
            51.450615 53.258636 53.340443 53.37822 55.07837 56.24102 57.564903 57.692596
            59.85141 61.115273 61.6677 62.740494 64.37821 65.35934 66.85142)
           (14.058517 19.176464 20.67374 22.741362 30.938679 31.941284 33.738403
            38.556107 39.94538 41.789017 43.447235 45.93868 47.679886 49.55611 51.359344
            52.891724 53.258636 54.076157 54.637974 55.71221 56.24102 57.692596 58.115265
            59.85141 61.564896 62.481194 63.506172 65.07616 66.359344 67.458046)
           (9.644863 19.176464 20.941286 24.741365 27.058514 30.78901 34.94538 37.938683
            38.556107 41.789017 43.447235 45.93868 47.945377 48.851406 51.970665
            52.692596 54.076157 54.637974 55.258636 56.24102 56.458042 58.35934 59.028866
            60.679882 61.564896 62.481194 63.98535 65.69259 66.359344 67.458046)
           (-2.8623562 20.941286 23.673746 24.741365 27.058514 27.941282 34.94538
            36.792694 37.938683 43.447235 44.368866 48.851406 48.94538 49.792686
            52.692596 53.37822 53.450607 55.258636 55.851414 55.914185 56.24102 58.35934
            58.50722 60.679882 61.115273 63.30329 64.74049 66.37822 66.6926 68.07837)
           (17.17646 20.176455 22.058521 23.789017 30.74136 31.789017 34.94538 38.938675
            40.247055 42.789013 44.97067 46.938683 49.458046 49.792686 52.692596 52.97066
            54.78613 55.679886 55.914185 56.98534 57.45805 59.359344 59.564903 61.258644
            62.564896 63.30329 64.985344 66.37822 67.359344 68.45804)
           (17.673746 20.176455 21.673754 21.941282 31.789017 31.938675 32.94128
            38.938675 39.556114 42.789013 44.447235 46.938683 49.85141 50.556107 52.35934
            52.97066 55.076164 55.115273 55.637978 57.24102 57.45805 59.115273 59.359344
            60.851406 62.564896 63.481186 64.985344 66.07615 67.359344 68.45804)
           (16.176453 17.673746 21.941282 24.673748 28.941278 31.938675 32.94128
            37.792686 38.938675 44.447235 45.368866 49.85141 49.94538 52.247055 52.35934
            54.378216 54.450607 55.115273 56.85141 56.91418 57.24102 58.5649 59.359344
            60.851406 62.115265 64.30328 65.74049 66.78613 67.6926 69.078384)
           (9.644863 19.741362 22.058521 24.673748 28.941278 30.74136 32.938683 36.94538
            38.938675 43.874855 45.368866 49.94538 50.25864 51.556114 53.359344 54.450607
            54.69259 55.679886 56.63797 56.85141 57.50785 59.028866 59.359344 61.258644
            62.115265 64.4812 65.74049 67.07616 67.6926 69.078384)
           (9.644863 16.058521 19.176464 24.741365 27.058514 30.78901 32.938683 36.94538
            37.938683 42.247063 46.738396 49.67988 49.94538 51.556114 53.359344 54.69259
            54.89173 55.258636 56.458042 56.63797 56.786125 58.35934 59.028866 60.679882
            61.564896 64.4812 65.506165 67.07616 67.6926 68.71222)
           (9.644863 16.058521 19.176464 24.741365 27.058514 30.78901 32.938683 36.94538
            37.938683 42.247063 46.738396 49.67988 49.94538 51.556114 53.359344 54.69259
            54.89173 55.258636 56.458042 56.63797 56.786125 58.35934 59.028866 60.679882
            61.564896 64.4812 65.506165 67.07616 67.6926 68.71222)
           (-1.8623571 18.74136 27.941282 29.058525 30.78901 31.741364 35.945377
            37.792686 41.945377 43.78902 45.970673 49.258644 49.94538 50.792694 53.692596
            54.378216 54.89173 55.851414 56.458042 56.91418 57.985336 59.507217 59.69259
            62.258636 62.67987 64.30328 65.506165 67.37821 67.6926 69.45805)
           (10.644891 18.74136 25.673744 27.941282 31.741364 33.368866 35.945377
            39.556114 41.945377 45.970673 46.368866 49.258644 50.945374 52.556114
            53.692596 55.076164 55.851414 56.29801 57.078377 57.985336 58.170906 59.69259
            60.028862 62.258636 63.11528 65.13368 66.28205 68.07616 68.6926 70.07838)
           (10.644891 17.058523 20.176455 25.741362 28.058521 31.789017 33.93868 37.94538
            38.938675 43.247055 47.7384 50.67988 50.945374 52.556114 54.359337 55.692593
            55.89172 56.25864 57.45805 57.63797 57.786125 59.359344 60.028862 61.67988
            62.564896 65.48119 66.50617 68.07616 68.6926 69.71221)
           (-0.8623009 19.741362 28.941278 30.058514 31.789017 32.741364 36.94538
            38.792694 42.94538 44.789017 46.970665 50.25864 50.945374 51.792694 54.69259
            55.37822 55.89172 56.85141 57.45805 57.914177 58.985336 60.507214 60.692596
            63.258636 63.679886 65.30328 66.50617 68.37822 68.6926 70.458046)
           (11.64489 19.741362 26.673746 28.941278 32.741364 34.368866 36.94538 40.556114
            42.94538 46.970665 47.368866 50.25864 51.94538 53.556114 54.69259 56.076157
            56.85141 57.29802 58.078377 58.985336 59.170914 60.692596 61.028854 63.258636
            64.11527 66.13368 67.28204 69.07616 69.6926 71.07837)
           (11.64489 18.058514 21.176462 26.741358 29.058525 32.78901 34.938683 38.945374
            39.93868 44.24706 48.738403 51.679886 51.94538 53.556114 55.359344 56.692596
            56.891727 57.258636 58.458046 58.637978 58.786133 60.359344 61.028854
            62.67987 63.564903 66.481186 67.506165 69.07616 69.6926 70.712204)
           (11.64489 18.058514 21.176462 26.741358 29.058525 32.78901 34.938683 38.945374
            39.93868 44.24706 48.738403 51.679886 51.94538 53.556114 55.359344 56.692596
            56.891727 57.258636 58.458046 58.637978 58.786133 60.359344 61.028854
            62.67987 63.564903 66.481186 67.506165 69.07616 69.6926 70.712204)
           (14.644884 18.058514 19.176464 26.741358 29.058525 34.938683 35.368866
            39.93868 40.792686 44.24706 50.938675 51.679886 52.945374 55.247055 55.359344
            57.258636 57.37822 58.29802 58.786133 59.078373 59.91418 60.359344 61.564896
            62.67987 64.02887 67.30328 68.28206 69.78613 70.6926 71.359344)
           (19.176464 21.74136 22.176458 24.05852 32.741364 33.789013 34.938683 40.792686
            40.938675 45.874847 49.738403 52.258636 52.945374 55.247055 55.359344
            57.37822 57.679882 57.891724 59.458042 59.507847 59.91418 61.359344 61.564896
            63.258636 64.564896 67.30328 68.506165 69.78613 70.6926 71.71222)
           (15.644875 21.74136 24.05852 24.673748 32.741364 34.938683 36.368874 40.938675
            42.55612 45.874847 47.368866 49.938683 52.258636 53.556114 55.359344
            56.450615 57.679882 58.076157 58.637978 59.507847 60.07837 61.359344
            62.115265 63.258636 65.02887 66.481186 67.74049 69.07616 70.359344 71.07837)
           (15.644875 19.058516 20.176455 27.741365 30.058514 35.938675 36.368874
            40.938675 41.792694 45.247055 47.368866 49.938683 52.679886 52.792686
            56.359344 56.450615 58.258644 58.34044 58.37822 59.786133 60.07837 61.359344
            62.564896 63.679886 65.02887 66.66771 67.74049 69.37822 70.359344 71.07837)
           (20.176455 22.741362 23.176455 25.058521 33.74136 34.789017 35.938675
            41.792694 41.938683 45.789013 46.874855 49.938683 52.792686 53.258636
            55.970665 56.359344 58.34044 58.37822 58.67989 60.45804 60.507854 62.359337
            62.564896 64.25864 65.56491 66.66771 67.985344 69.37822 70.359344 71.45804)
           (20.176455 22.741362 23.176455 25.058521 33.74136 34.789017 35.938675
            41.792694 41.938683 46.874855 50.7384 53.258636 53.945377 56.24706 56.359344
            58.37822 58.67989 58.891724 60.45804 60.507854 60.914177 62.359337 62.564896
            64.25864 65.56491 68.3033 69.50617 70.78613 71.69259 72.71221)
           (13.644882 23.176455 24.941277 28.741362 31.058517 34.789017 38.945374
            41.938683 42.55612 47.44724 50.7384 52.85141 53.945377 55.556114 56.692596
            58.076157 58.891724 59.25864 60.241024 60.45804 61.170914 62.359337 63.028862
            64.67988 65.56491 68.13368 69.50617 71.07616 71.69259 72.71221)
           (13.644882 20.058517 23.176455 28.741362 31.058517 34.789017 36.938675
            40.945374 41.938683 46.247063 50.7384 53.67988 53.945377 55.556114 57.359337
            58.692593 58.891724 59.25864 60.45804 60.637974 60.78613 62.359337 63.028862
            64.67988 65.56491 68.48119 69.50617 71.07616 71.69259 72.71221)
           (2.1376677 22.741362 31.941284 33.058514 34.789017 35.741364 39.94538
            41.792694 45.945374 47.789017 49.970665 53.258636 53.945377 54.792694
            57.692596 58.37822 58.891724 59.85141 60.45804 60.914177 61.985344 63.507225
            63.69258 66.25865 66.679886 68.3033 69.50617 71.37822 71.69259 73.458046)
           (14.644884 22.741362 29.673744 31.941284 35.741364 37.368866 39.94538
            43.556114 45.945374 49.970665 50.368866 53.258636 54.94538 56.556114
            57.692596 59.076153 59.85141 60.298016 61.078377 61.985344 62.170914 63.69258
            64.02887 66.25865 67.11527 69.13368 70.28204 72.07616 72.6926 74.07837)
           (14.644884 21.05852 24.176456 29.741364 32.058517 35.789017 37.938683
            41.945377 42.93868 47.24706 51.738396 54.679886 54.94538 56.556114 58.35934
            59.69259 59.891727 60.258636 61.458046 61.63797 61.786125 63.359344 64.02887
            65.679886 66.5649 69.4812 70.506165 72.07616 72.6926 73.712204)
           (14.644884 23.673746 27.941282 31.741364 34.05852 37.938683 38.941284
            41.945377 44.938683 48.78901 50.44724 54.94538 55.851414 56.556114 58.35934
            58.970665 59.69259 61.115273 61.63797 62.258636 63.241028 64.02887 65.35934
            66.85142 67.679886 69.4812 70.98534 72.07616 72.6926 74.45804)
           (14.644884 18.74136 23.673746 27.941282 31.741364 35.945377 37.938683
            41.945377 42.874855 45.970673 49.258644 53.692596 54.94538 55.851414 56.50785
            56.556114 57.985336 58.35934 58.970665 59.69259 61.115273 61.63797 62.258636
            63.033188 64.02887 68.532005 69.4812 70.98534 72.07616 72.6926)
           (15.058514 18.176456 29.789017 35.945377 36.938675 41.247055 45.738403
            48.679882 48.94538 53.692596 53.891727 55.458046 55.637978 55.786133
            57.359337 60.564903 63.481186 64.506165 66.6926 67.712204)
           (8.644881 15.058514 18.176456 23.741364 26.058517 29.789017 31.938675
            35.945377 36.938675 41.247055 45.738403 48.679882 48.94538 50.556107 52.35934
            53.692596 53.891727 54.258644 55.458046 55.637978 55.786133 57.359337
            58.02887 59.679886 60.564903 63.481186 64.506165 66.07615 66.6926 67.712204)
           (11.64489 15.058514 16.176453 23.741364 26.058517 31.938675 32.368866
            36.938675 37.792686 41.247055 47.938675 48.679882 49.94538 52.247055 52.35934
            54.258644 54.378216 55.298023 55.786133 56.078373 56.91418 57.359337 58.5649
            59.679886 61.028854 64.30328 65.28204 66.78613 67.6926 68.359344)
           (16.176453 18.74136 19.176464 21.05852 29.741364 30.78901 31.938675 37.792686
            37.938683 42.874855 46.738396 49.258644 49.94538 52.247055 52.35934 54.378216
            54.679886 54.89173 56.458042 56.50785 56.91418 58.35934 58.5649 60.258636
            61.564896 64.30328 65.506165 66.78613 67.6926 68.71222)
           (12.644875 18.74136 21.05852 21.673754 29.741364 31.938675 33.368866 37.938683
            39.556114 42.874855 44.368866 46.938683 49.258644 50.556107 52.35934
            53.450607 54.679886 55.076164 55.637978 56.50785 57.078377 58.35934 59.115273
            60.258636 62.02887 63.481186 64.74049 66.07615 67.359344 68.07837)
           (12.644875 16.058521 17.17646 24.741365 27.058514 32.938683 33.368866
            37.938683 38.792694 42.247063 44.368866 46.938683 49.67988 49.792686
            53.359344 53.450607 55.258636 55.340446 55.37822 56.786125 57.078377 58.35934
            59.564903 60.679882 62.02887 63.66771 64.74049 66.37822 67.359344 68.07837)
           (17.17646 19.741362 20.176455 22.058521 30.74136 31.789017 32.938683 38.792694
            38.938675 42.789013 43.874855 46.938683 49.792686 50.25864 52.97066 53.359344
            55.340446 55.37822 55.679886 57.45805 57.50785 59.359344 59.564903 61.258644
            62.564896 63.66771 64.985344 66.37822 67.359344 68.45804)
           (17.17646 19.741362 20.176455 22.058521 30.74136 31.789017 32.938683 38.792694
            38.938675 43.874855 47.7384 50.25864 50.945374 53.247055 53.359344 55.37822
            55.679886 55.89172 57.45805 57.50785 57.914177 59.359344 59.564903 61.258644
            62.564896 65.30328 66.50617 67.786125 68.6926 69.71221)
           (10.644891 20.176455 21.941282 25.741362 28.058521 31.789017 35.945377
            38.938675 39.556114 44.447235 47.7384 49.85141 50.945374 52.556114 53.692596
            55.076164 55.89172 56.25864 57.24102 57.45805 58.170906 59.359344 60.028862
            61.67988 62.564896 65.13368 66.50617 68.07616 68.6926 69.71221)
           (10.644891 17.058523 20.176455 25.741362 28.058521 31.789017 33.93868 37.94538
            38.938675 43.247055 47.7384 50.67988 50.945374 52.556114 54.359337 55.692593
            55.89172 56.25864 57.45805 57.63797 57.786125 59.359344 60.028862 61.67988
            62.564896 65.48119 66.50617 68.07616 68.6926 69.71221)
           (13.644882 17.058523 18.176456 25.741362 28.058521 33.93868 34.368866
            38.938675 39.792694 43.247055 49.938683 50.67988 51.94538 54.247063 54.359337
            56.25864 56.378212 57.29802 57.786125 58.078377 58.914185 59.359344 60.564903
            61.67988 63.028862 66.30329 67.28204 68.78612 69.6926 70.359344)
           (18.176456 20.741364 21.176462 23.058517 31.741364 32.78901 33.93868 39.792694
            39.93868 44.874863 48.738403 51.258644 51.94538 54.247063 54.359337 56.378212
            56.679886 56.891727 58.458046 58.50785 58.914185 60.359344 60.564903
            62.258636 63.564903 66.30329 67.506165 68.78612 69.6926 70.712204)
           (18.176456 20.741364 23.058517 31.058517 32.78901 33.93868 37.789017 39.792694
            43.94538 45.789013 47.970665 51.258644 51.94538 54.247063 54.359337 56.378212
            56.679886 56.891727 58.458046 58.914185 59.98534 60.564903 61.692596 63.45804
            64.67988 66.30329 67.506165 68.78612 69.6926 71.45804)
           (12.644875 22.176458 23.941286 27.741365 30.058514 33.789013 37.94538
            40.938675 41.556114 44.789017 46.447243 48.938675 50.945374 51.85141
            54.970665 55.692593 57.076157 57.63797 58.258644 59.24102 59.458042 61.359344
            62.02887 63.679886 64.564896 65.48119 66.98534 68.6926 69.359344 70.458046)
           (18.058514 19.176464 26.741358 27.673742 34.938683 35.368866 35.941284
            40.792686 42.941288 43.94538 47.44724 48.938675 51.679886 51.792694 55.359344
            55.450615 57.258636 57.340446 57.37822 59.078373 60.241024 61.564896
            61.692596 63.85141 65.115265 65.66771 66.7405 68.37822 69.359344 70.85141)
           (18.058514 23.176455 24.673748 26.741358 34.938683 35.941284 37.738403
            42.55612 43.94538 45.789013 47.44724 49.938683 51.679886 53.556114 55.359344
            56.891727 57.258636 58.076157 58.637978 59.712215 60.241024 61.692596
            62.115265 63.85141 65.56491 66.481186 67.506165 69.07616 70.359344 71.45804)
           (13.644882 23.176455 24.941277 28.741362 31.058517 34.789017 38.945374
            41.938683 42.55612 45.789013 47.44724 49.938683 51.94538 52.85141 55.970665
            56.692596 58.076157 58.637978 59.25864 60.241024 60.45804 62.359337 63.028862
            64.67988 65.56491 66.481186 67.985344 69.6926 70.359344 71.45804)
           (1.1376657 24.941277 27.673742 28.741362 31.058517 31.941284 38.945374
            40.792686 41.938683 47.44724 48.368874 52.85141 52.945374 53.792694 56.692596
            57.37822 57.45061 59.25864 59.85141 59.91418 60.241024 62.359337 62.507217
            64.67988 65.115265 67.30328 68.74049 70.37822 70.6926 72.07837)
           (21.176462 24.176456 26.058517 27.789013 34.741364 35.789017 38.945374
            42.93868 44.24706 46.789017 48.970665 50.938675 53.458046 53.792694 56.692596
            56.97067 58.786133 59.679886 59.91418 60.985336 61.458046 63.359344 63.564903
            65.25864 66.5649 67.30328 68.98534 70.37822 71.359344 72.45804)
           (21.673754 24.176456 25.673744 25.941284 35.789017 35.938675 36.941284
            42.93868 43.556114 46.789017 48.447235 50.938675 53.851402 54.55612 56.359344
            56.97067 59.076153 59.115273 59.637978 61.24102 61.458046 63.11528 63.359344
            64.85141 66.5649 67.4812 68.98534 70.07616 71.359344 72.45804)
           (20.176455 21.673754 25.941284 28.673738 32.94128 35.938675 36.941284
            41.792694 42.93868 48.447235 49.368866 53.851402 53.945377 56.24706 56.359344
            58.37822 58.450607 59.115273 60.851406 60.914177 61.24102 62.564896 63.359344
            64.85141 66.115265 68.3033 69.74049 70.78613 71.69259 73.07838)
           (13.644882 23.741364 26.058517 28.673738 32.94128 34.741364 36.938675
            40.945374 42.93868 47.874855 49.368866 53.945377 54.258644 55.556114
            57.359337 58.450607 58.692593 59.679886 60.637974 60.851406 61.50785
            63.028862 63.359344 65.25864 66.115265 68.48119 69.74049 71.07616 71.69259
            73.07838)
           (13.644882 20.058517 23.176455 28.741362 31.058517 34.789017 36.938675
            40.945374 41.938683 46.247063 50.7384 53.67988 53.945377 55.556114 57.359337
            58.692593 58.891724 59.25864 60.45804 60.637974 60.78613 62.359337 63.028862
            64.67988 65.56491 68.48119 69.50617 71.07616 71.69259 72.71221)
           (16.64487 20.058517 21.176462 28.741362 31.058517 36.938675 37.368866
            41.938683 42.792694 46.247063 52.938675 53.67988 54.94538 57.247055 57.359337
            59.25864 59.378212 60.298016 60.78613 61.078377 61.914185 62.359337 63.564903
            64.67988 66.02887 69.30329 70.28204 71.78613 72.6926 73.359344)
           (21.176462 23.741364 24.176456 26.058517 34.741364 35.789017 36.938675
            42.792694 42.93868 47.874855 51.738396 54.258644 54.94538 57.247055 57.359337
            59.378212 59.679886 59.891727 61.458046 61.50785 61.914185 63.359344
            63.564903 65.25864 66.5649 69.30329 70.506165 71.78613 72.6926 73.712204)
           (21.176462 23.741364 26.058517 34.05852 35.789017 36.938675 40.789017
            42.792694 46.94538 48.78901 50.97067 54.258644 54.94538 57.247055 57.359337
            59.378212 59.679886 59.891727 61.458046 61.914185 62.985336 63.564903 64.6926
            66.45805 67.679886 69.30329 70.506165 71.78613 72.6926 74.45804)
           (15.644875 25.176458 26.941286 30.74136 33.058514 36.78901 40.945374 43.938675
            44.556114 47.789017 49.447243 51.93868 53.945377 54.85141 57.970673 58.692593
            60.07616 60.637974 61.258644 62.241013 62.45804 64.359344 65.02887 66.679886
            67.564896 68.48119 69.98534 71.69259 72.359344 73.458046)
           (21.05852 22.176458 29.741364 30.673748 37.938683 38.368866 38.941284
            43.792694 45.941284 46.94538 50.44724 51.93868 54.679886 54.792694 58.35934
            58.450607 60.258636 60.34045 60.378216 62.07837 63.241028 64.564896 64.6926
            66.85142 68.115265 68.66771 69.74049 71.37822 72.359344 73.8514)
           (21.05852 26.176472 27.673742 29.741364 37.938683 38.941284 40.738403
            45.556107 46.94538 48.78901 50.44724 52.938675 54.679886 56.556114 58.35934
            59.891727 60.258636 61.076157 61.63797 62.712204 63.241028 64.6926 65.115265
            66.85142 68.56491 69.4812 70.506165 72.07616 73.359344 74.45804)
           (16.64487 26.176472 27.941282 31.741364 34.05852 37.789017 41.945377 44.938683
            45.556107 48.78901 50.44724 52.938675 54.94538 55.851414 58.970665 59.69259
            61.076157 61.63797 62.258636 63.241028 63.45804 65.35934 66.02887 67.679886
            68.56491 69.4812 70.98534 72.6926 73.359344 74.45804)
           (4.1376457 27.941282 30.673748 31.741364 34.05852 34.941277 41.945377
            43.792694 44.938683 50.44724 51.368866 55.851414 55.94538 56.79269 59.69259
            60.378216 60.45061 62.258636 62.85141 62.914185 63.241028 65.35934 65.50722
            67.679886 68.115265 70.30328 71.74049 73.37822 73.6926 75.07837)
           (24.176456 27.176462 29.058525 30.78901 37.741356 38.78901 41.945377 45.93868
            47.24706 49.789017 51.970665 53.938683 56.458042 56.79269 59.69259 59.97067
            61.786125 62.67987 62.914185 63.98535 64.458046 66.359344 66.5649 68.25864
            69.5649 70.30328 71.98534 73.37822 74.35934 75.45804)
           (24.673748 27.176462 28.673738 28.941278 38.78901 38.938675 39.941284 45.93868
            46.55611 49.789017 51.447243 53.938683 56.85141 57.556107 59.359344 59.97067
            62.076157 62.115265 62.63797 64.24102 64.458046 66.115265 66.359344 67.85141
            69.5649 70.4812 71.98534 73.07616 74.35934 75.45804)
           (23.176455 24.673748 28.941278 31.673752 35.941284 38.938675 39.941284
            44.79269 45.93868 51.447243 52.368862 56.85141 56.945377 59.24706 59.359344
            61.37822 61.450607 62.115265 63.85141 63.914185 64.24102 65.56491 66.359344
            67.85141 69.115265 71.3033 72.74049 73.786125 74.69259 76.07837)
           (16.64487 26.741358 29.058525 31.673752 35.941284 37.741356 39.93868 43.94538
            45.93868 50.87486 52.368862 56.945377 57.258636 58.55611 60.359344 61.450607
            61.692596 62.67987 63.63797 63.85141 64.50785 66.02887 66.359344 68.25864
            69.115265 71.48119 72.74049 74.07616 74.69259 76.07837)
           (16.64487 23.058517 26.176472 31.741364 34.05852 37.789017 39.93868 43.94538
            44.938683 49.247063 53.7384 56.679886 56.945377 58.55611 60.359344 61.692596
            61.891724 62.258636 63.45804 63.63797 63.786133 65.35934 66.02887 67.679886
            68.56491 71.48119 72.50617 74.07616 74.69259 75.71221)
           (16.64487 23.058517 26.176472 31.741364 34.05852 37.789017 39.93868 43.94538
            44.938683 49.247063 53.7384 56.679886 56.945377 58.55611 60.359344 61.692596
            61.891724 62.258636 63.45804 63.63797 63.786133 65.35934 66.02887 67.679886
            68.56491 71.48119 72.50617 74.07616 74.69259 75.71221)
           (16.64487 25.673744 29.941292 33.74136 36.058517 39.93868 40.941284 43.94538
            46.938683 50.78901 52.447235 56.945377 57.851418 58.55611 60.359344 60.970665
            61.692596 63.11528 63.63797 64.25864 65.24101 66.02887 67.359344 68.85141
            69.679886 71.48119 72.98534 74.07616 74.69259 76.458046)
           (16.64487 20.741364 25.673744 29.941292 33.74136 37.94538 39.93868 43.94538
            44.874863 47.970665 51.258644 55.692593 56.945377 57.851418 58.50785 58.55611
            59.98534 60.359344 60.970665 61.692596 63.11528 63.63797 64.25864 65.03319
            66.02887 70.532 71.48119 72.98534 74.07616 74.69259)
           (17.058523 20.176455 31.789017 37.94538 38.938675 43.247055 47.7384 50.67988
            50.945374 55.692593 55.89172 57.45805 57.63797 57.786125 59.359344 62.564896
            65.48119 66.50617 68.6926 69.71221)
           (10.644891 17.058523 20.176455 25.741362 28.058521 31.789017 33.93868 37.94538
            38.938675 43.247055 47.7384 50.67988 50.945374 52.556114 54.359337 55.692593
            55.89172 56.25864 57.45805 57.63797 57.786125 59.359344 60.028862 61.67988
            62.564896 65.48119 66.50617 68.07616 68.6926 69.71221)
           (13.644882 17.058523 18.176456 25.741362 28.058521 33.93868 34.368866
            38.938675 39.792694 43.247055 49.938683 50.67988 51.94538 54.247063 54.359337
            56.25864 56.378212 57.29802 57.786125 58.078377 58.914185 59.359344 60.564903
            61.67988 63.028862 66.30329 67.28204 68.78612 69.6926 70.359344)
           (18.176456 20.741364 21.176462 23.058517 31.741364 32.78901 33.93868 39.792694
            39.93868 44.874863 48.738403 51.258644 51.94538 54.247063 54.359337 56.378212
            56.679886 56.891727 58.458046 58.50785 58.914185 60.359344 60.564903
            62.258636 63.564903 66.30329 67.506165 68.78612 69.6926 70.712204)
           (18.176456 20.741364 23.058517 31.058517 32.78901 33.93868 37.789017 39.792694
            43.94538 45.789013 47.970665 51.258644 51.94538 54.247063 54.359337 56.378212
            56.679886 56.891727 58.458046 58.914185 59.98534 60.564903 61.692596 63.45804
            64.67988 66.30329 67.506165 68.78612 69.6926 71.45804)
           (12.644875 22.176458 23.941286 27.741365 30.058514 33.789013 37.94538
            40.938675 41.556114 44.789017 46.447243 48.938675 50.945374 51.85141
            54.970665 55.692593 57.076157 57.63797 58.258644 59.24102 59.458042 61.359344
            62.02887 63.679886 64.564896 65.48119 66.98534 68.6926 69.359344 70.458046)
           (18.058514 19.176464 26.741358 27.673742 34.938683 35.368866 35.941284
            40.792686 42.941288 43.94538 47.44724 48.938675 51.679886 51.792694 55.359344
            55.450615 57.258636 57.340446 57.37822 59.078373 60.241024 61.564896
            61.692596 63.85141 65.115265 65.66771 66.7405 68.37822 69.359344 70.85141)
           (18.058514 23.176455 24.673748 26.741358 34.938683 35.941284 37.738403
            42.55612 43.94538 45.789013 47.44724 49.938683 51.679886 53.556114 55.359344
            56.891727 57.258636 58.076157 58.637978 59.712215 60.241024 61.692596
            62.115265 63.85141 65.56491 66.481186 67.506165 69.07616 70.359344 71.45804)
           (13.644882 23.176455 24.941277 28.741362 31.058517 34.789017 38.945374
            41.938683 42.55612 45.789013 47.44724 49.938683 51.94538 52.85141 55.970665
            56.692596 58.076157 58.637978 59.25864 60.241024 60.45804 62.359337 63.028862
            64.67988 65.56491 66.481186 67.985344 69.6926 70.359344 71.45804)
           (1.1376657 24.941277 27.673742 28.741362 31.058517 31.941284 38.945374
            40.792686 41.938683 47.44724 48.368874 52.85141 52.945374 53.792694 56.692596
            57.37822 57.45061 59.25864 59.85141 59.91418 60.241024 62.359337 62.507217
            64.67988 65.115265 67.30328 68.74049 70.37822 70.6926 72.07837)
           (21.176462 24.176456 26.058517 27.789013 34.741364 35.789017 38.945374
            42.93868 44.24706 46.789017 48.970665 50.938675 53.458046 53.792694 56.692596
            56.97067 58.786133 59.679886 59.91418 60.985336 61.458046 63.359344 63.564903
            65.25864 66.5649 67.30328 68.98534 70.37822 71.359344 72.45804)
           (21.673754 24.176456 25.673744 25.941284 35.789017 35.938675 36.941284
            42.93868 43.556114 46.789017 48.447235 50.938675 53.851402 54.55612 56.359344
            56.97067 59.076153 59.115273 59.637978 61.24102 61.458046 63.11528 63.359344
            64.85141 66.5649 67.4812 68.98534 70.07616 71.359344 72.45804)
           (20.176455 21.673754 25.941284 28.673738 32.94128 35.938675 36.941284
            41.792694 42.93868 48.447235 49.368866 53.851402 53.945377 56.24706 56.359344
            58.37822 58.450607 59.115273 60.851406 60.914177 61.24102 62.564896 63.359344
            64.85141 66.115265 68.3033 69.74049 70.78613 71.69259 73.07838)
           (13.644882 23.741364 26.058517 28.673738 32.94128 34.741364 36.938675
            40.945374 42.93868 47.874855 49.368866 53.945377 54.258644 55.556114
            57.359337 58.450607 58.692593 59.679886 60.637974 60.851406 61.50785
            63.028862 63.359344 65.25864 66.115265 68.48119 69.74049 71.07616 71.69259
            73.07838)
           (13.644882 20.058517 23.176455 28.741362 31.058517 34.789017 36.938675
            40.945374 41.938683 46.247063 50.7384 53.67988 53.945377 55.556114 57.359337
            58.692593 58.891724 59.25864 60.45804 60.637974 60.78613 62.359337 63.028862
            64.67988 65.56491 68.48119 69.50617 71.07616 71.69259 72.71221)
           (13.644882 22.673738 26.941286 30.74136 33.058514 36.938675 37.941277
            40.945374 43.938675 47.789017 49.447243 53.945377 54.85141 55.556114
            57.359337 57.970673 58.692593 60.115276 60.637974 61.258644 62.241013
            63.028862 64.359344 65.85141 66.679886 68.48119 69.98534 71.07616 71.69259
            73.458046)
           (13.644882 17.741364 22.673738 26.941286 30.74136 34.94538 36.938675 40.945374
            41.87486 44.97067 48.258636 52.692596 53.945377 54.85141 55.507843 55.556114
            56.98534 57.359337 57.970673 58.692593 60.115276 60.637974 61.258644 62.03318
            63.028862 67.532 68.48119 69.98534 71.07616 71.69259)
           (14.058517 17.17646 28.789015 34.94538 35.938675 40.247055 44.738403 47.679886
            47.945377 52.692596 52.891724 54.458046 54.637974 54.78613 56.359344
            59.564903 62.481194 63.506172 65.69259 66.71221)
           (7.644891 14.058517 17.17646 22.741362 25.058521 28.789015 30.938679 34.94538
            35.938675 40.247055 44.738403 47.679886 47.945377 49.55611 51.359344
            52.692596 52.891724 53.258636 54.458046 54.637974 54.78613 56.359344 57.02887
            58.67989 59.564903 62.481194 63.506172 65.07616 65.69259 66.71221)
           (10.644891 14.058517 15.176462 22.741362 25.058521 30.938679 31.368862
            35.938675 36.792694 40.247055 46.938683 47.679886 48.94538 51.247063
            51.359344 53.258636 53.37822 54.29802 54.78613 55.07837 55.914185 56.359344
            57.564903 58.67989 60.028862 63.30329 64.28204 65.78612 66.6926 67.359344)
           (15.176462 17.741364 18.176456 20.058517 28.741362 29.789017 30.938679
            36.792694 36.938675 41.87486 45.738403 48.258636 48.94538 51.247063 51.359344
            53.37822 53.67988 53.891727 55.458046 55.507843 55.914185 57.359337 57.564903
            59.25864 60.564903 63.30329 64.506165 65.78612 66.6926 67.712204)
           (11.64489 17.741364 20.058517 20.67374 28.741362 30.938679 32.368866 36.938675
            38.556107 41.87486 43.368862 45.93868 48.258636 49.55611 51.359344 52.450615
            53.67988 54.076157 54.637974 55.507843 56.078373 57.359337 58.115265 59.25864
            61.028854 62.481194 63.740494 65.07616 66.359344 67.07838)
           (11.64489 15.058514 16.176453 23.741364 26.058517 31.938675 32.368866
            36.938675 37.792686 41.247055 43.368862 45.93868 48.679882 48.792694 52.35934
            52.450615 54.258644 54.340446 54.378216 55.786133 56.078373 57.359337 58.5649
            59.679886 61.028854 62.667694 63.740494 65.37822 66.359344 67.07838)
           (16.176453 18.74136 19.176464 21.05852 29.741364 30.78901 31.938675 37.792686
            37.938683 41.789017 42.874855 45.93868 48.792694 49.258644 51.970665 52.35934
            54.340446 54.378216 54.679886 56.458042 56.50785 58.35934 58.5649 60.258636
            61.564896 62.667694 63.98535 65.37822 66.359344 67.458046)
           (16.176453 18.74136 19.176464 21.05852 29.741364 30.78901 31.938675 37.792686
            37.938683 42.874855 46.738396 49.258644 49.94538 52.247055 52.35934 54.378216
            54.679886 54.89173 56.458042 56.50785 56.91418 58.35934 58.5649 60.258636
            61.564896 64.30328 65.506165 66.78613 67.6926 68.71222)
           (9.644863 19.176464 20.941286 24.741365 27.058514 30.78901 34.94538 37.938683
            38.556107 43.447235 46.738396 48.851406 49.94538 51.556114 52.692596
            54.076157 54.89173 55.258636 56.24102 56.458042 57.170906 58.35934 59.028866
            60.679882 61.564896 64.133675 65.506165 67.07616 67.6926 68.71222)
           (9.644863 16.058521 19.176464 24.741365 27.058514 30.78901 32.938683 36.94538
            37.938683 42.247063 46.738396 49.67988 49.94538 51.556114 53.359344 54.69259
            54.89173 55.258636 56.458042 56.63797 56.786125 58.35934 59.028866 60.679882
            61.564896 64.4812 65.506165 67.07616 67.6926 68.71222)
           (9.644863 18.673748 22.941284 26.741358 29.058525 32.938683 33.941284 36.94538
            39.93868 43.78902 45.447243 49.94538 50.851418 51.556114 53.359344 53.97067
            54.69259 56.11527 56.63797 57.258636 58.241028 59.028866 60.359344 61.851402
            62.67987 64.4812 65.98534 67.07616 67.6926 69.45805)
           (9.644863 13.741362 18.673748 22.941284 26.741358 30.945381 32.938683 36.94538
            37.874855 40.97066 44.25864 48.692596 49.94538 50.851418 51.50785 51.556114
            52.985336 53.359344 53.97067 54.69259 56.11527 56.63797 57.258636 58.033184
            59.028866 63.531998 64.4812 65.98534 67.07616 67.6926)
           (10.058521 13.176458 24.789013 30.945381 31.938675 36.247055 40.738403
            43.679886 43.94538 48.692596 48.89173 50.458046 50.637966 50.786133 52.35934
            55.5649 58.481194 59.506172 61.692596 62.712204)
           (3.6448746 10.058521 13.176458 18.74136 21.05852 24.789013 26.938677 30.945381
            31.938675 36.247055 40.738403 43.679886 43.94538 45.556107 47.359344
            48.692596 48.89173 49.258644 50.458046 50.637966 50.786133 52.35934 53.028862
            54.679886 55.5649 58.481194 59.506172 61.076157 61.692596 62.712204)
           (6.644886 10.058521 11.176452 18.74136 21.05852 26.938677 27.368866 31.938675
            32.792694 36.247055 42.93868 43.679886 44.945377 47.24706 47.359344 49.258644
            49.378223 50.29802 50.786133 51.078377 51.914185 52.35934 53.564903 54.679886
            56.028866 59.303295 60.28205 61.786125 62.69259 63.359344)
           (11.176452 13.741362 14.176472 16.058521 24.741365 25.78901 26.938677
            32.792694 32.938683 37.874855 41.7384 44.25864 44.945377 47.24706 47.359344
            49.378223 49.67988 49.891724 51.458046 51.50785 51.914185 53.359344 53.564903
            55.258636 56.564907 59.303295 60.506172 61.786125 62.69259 63.71221)
           (11.176452 13.741362 16.058521 24.05852 25.78901 26.938677 30.78901 32.792694
            36.94538 38.78901 40.97066 44.25864 44.945377 47.24706 47.359344 49.378223
            49.67988 49.891724 51.458046 51.914185 52.985336 53.564903 54.69259 56.458042
            57.679882 59.303295 60.506172 61.786125 62.69259 64.458046)
           (5.6448784 15.176462 16.941278 20.741364 23.058517 26.789011 30.945381
            33.93868 34.55611 37.789017 39.447243 41.938683 43.94538 44.85141 47.970665
            48.692596 50.076153 50.637966 51.258644 52.24102 52.45804 54.359337 55.028862
            56.679886 57.564903 58.481194 59.98534 61.692596 62.359337 63.45804)
           (11.058517 12.176456 19.741362 20.67374 27.938679 28.368862 28.941278
            33.792698 35.941284 36.94538 40.447235 41.938683 44.679886 44.79269 48.359344
            48.45061 50.25864 50.340443 50.37822 52.07837 53.24102 54.564903 54.69259
            56.85141 58.115265 58.6677 59.740494 61.37822 62.359337 63.85141)
           (11.058517 16.176453 17.673746 19.741362 27.938679 28.941278 30.738403
            35.556118 36.94538 38.78901 40.447235 42.93868 44.679886 46.55611 48.359344
            49.891724 50.25864 51.07615 51.637974 52.712208 53.24102 54.69259 55.115273
            56.85141 58.5649 59.481194 60.506172 62.076157 63.359344 64.458046)
           (6.644886 16.176453 17.941292 21.74136 24.05852 27.789013 31.945381 34.938683
            35.556118 38.78901 40.447235 42.93868 44.945377 45.851418 48.970665 49.692593
            51.07615 51.637974 52.258636 53.24102 53.458046 55.359344 56.028866 57.679882
            58.5649 59.481194 60.985336 62.69259 63.359344 64.458046)
           (-5.862336 17.941292 20.67374 21.74136 24.05852 24.941277 31.945381 33.792698
            34.938683 40.447235 41.36887 45.851418 45.945374 46.792694 49.692593 50.37822
            50.450607 52.258636 52.85141 52.914177 53.24102 55.359344 55.50722 57.679882
            58.115265 60.30329 61.740486 63.37822 63.69258 65.07837)
           (14.176472 17.17646 19.058516 20.789011 27.741365 28.789015 31.945381
            35.938675 37.247063 39.789013 41.97067 43.938675 46.458046 46.792694
            49.692593 49.970665 51.78613 52.679886 52.914177 53.98534 54.458046 56.359344
            56.564907 58.258644 59.564903 60.30329 61.985344 63.37822 64.359344 65.45805)
           (14.673746 17.17646 18.673748 18.941282 28.789015 28.93868 29.941292 35.938675
            36.556114 39.789013 41.447243 43.938675 46.851414 47.556118 49.35934
            49.970665 52.076157 52.115273 52.63797 54.241024 54.458046 56.11527 56.359344
            57.851418 59.564903 60.4812 61.985344 63.07615 64.359344 65.45805)
           (13.176458 14.673746 18.941282 21.673754 25.941284 28.93868 29.941292
            34.792694 35.938675 41.447243 42.368866 46.851414 46.94538 49.247063 49.35934
            51.378216 51.450615 52.115273 53.851402 53.914185 54.241024 55.5649 56.359344
            57.851418 59.115273 61.303284 62.740494 63.786133 64.6926 66.07837)
           (6.644886 16.741362 19.058516 21.673754 25.941284 27.741365 29.938675
            33.945374 35.938675 40.874863 42.368866 46.94538 47.25864 48.556114 50.359337
            51.450615 51.69259 52.679886 53.63797 53.851402 54.50785 56.028866 56.359344
            58.258644 59.115273 61.4812 62.740494 64.07616 64.6926 66.07837)
           (6.644886 13.058521 16.176453 21.74136 24.05852 27.789013 29.938675 33.945374
            34.938683 39.247063 43.738403 46.67989 46.94538 48.556114 50.359337 51.69259
            51.89173 52.258636 53.458046 53.63797 53.786125 55.359344 56.028866 57.679882
            58.5649 61.4812 62.50618 64.07616 64.6926 65.712204)
           (-4.8623343 15.741365 24.941277 26.058517 27.789013 28.741362 32.945377
            34.792694 38.945374 40.789017 42.970665 46.258644 46.94538 47.79269 50.692596
            51.378216 51.89173 52.85141 53.458046 53.914185 54.985336 56.507217 56.692596
            59.25864 59.679886 61.303284 62.50618 64.37821 64.6926 66.45805)
           (7.644891 15.741365 22.673738 24.941277 28.741362 30.368866 32.945377
            36.556114 38.945374 42.970665 43.368862 46.258644 47.945377 49.55611
            50.692596 52.076157 52.85141 53.29802 54.078377 54.985336 55.170914 56.692596
            57.02887 59.25864 60.115276 62.133667 63.28205 65.07616 65.69259 67.07838)
           (7.644891 14.058517 17.17646 22.741362 25.058521 28.789015 30.938679 34.94538
            35.938675 40.247055 44.738403 47.679886 47.945377 49.55611 51.359344
            52.692596 52.891724 53.258636 54.458046 54.637974 54.78613 56.359344 57.02887
            58.67989 59.564903 62.481194 63.506172 65.07616 65.69259 66.71221)
           (10.644891 14.058517 15.176462 22.741362 25.058521 30.938679 31.368862
            35.938675 36.792694 40.247055 46.938683 47.679886 48.94538 51.247063
            51.359344 53.258636 53.37822 54.29802 54.78613 55.07837 55.914185 56.359344
            57.564903 58.67989 60.028862 63.30329 64.28204 65.78612 66.6926 67.359344)
           (15.176462 17.741364 18.176456 20.058517 28.741362 29.789017 30.938679
            36.792694 36.938675 41.87486 45.738403 48.258636 48.94538 51.247063 51.359344
            53.37822 53.67988 53.891727 55.458046 55.507843 55.914185 57.359337 57.564903
            59.25864 60.564903 63.30329 64.506165 65.78612 66.6926 67.712204)
           (15.176462 17.741364 20.058517 28.058521 29.789017 30.938679 34.789017
            36.792694 40.945374 42.789013 44.97067 48.258636 48.94538 51.247063 51.359344
            53.37822 53.67988 53.891727 55.458046 55.914185 56.98534 57.564903 58.692593
            60.45804 61.67988 63.30329 64.506165 65.78612 66.6926 68.45804)
           (9.644863 19.176464 20.941286 24.741365 27.058514 30.78901 34.94538 37.938683
            38.556107 41.789017 43.447235 45.93868 47.945377 48.851406 51.970665
            52.692596 54.076157 54.637974 55.258636 56.24102 56.458042 58.35934 59.028866
            60.679882 61.564896 62.481194 63.98535 65.69259 66.359344 67.458046)
           (15.058514 16.176453 23.741364 24.673748 31.938675 32.368866 32.94128
            37.792686 39.941284 40.945374 44.447235 45.93868 48.679882 48.792694 52.35934
            52.450615 54.258644 54.340446 54.378216 56.078373 57.24102 58.5649 58.692593
            60.851406 62.115265 62.667694 63.740494 65.37822 66.359344 67.85141)
           (15.058514 20.176455 21.673754 23.741364 31.938675 32.94128 34.738396
            39.556114 40.945374 42.789013 44.447235 46.938683 48.679882 50.556107
            52.35934 53.891727 54.258644 55.076164 55.637978 56.71221 57.24102 58.692593
            59.115273 60.851406 62.564896 63.481186 64.506165 66.07615 67.359344
            68.45804)
           (10.644891 20.176455 21.941282 25.741362 28.058521 31.789017 35.945377
            38.938675 39.556114 42.789013 44.447235 46.938683 48.94538 49.85141 52.97066
            53.692596 55.076164 55.637978 56.25864 57.24102 57.45805 59.359344 60.028862
            61.67988 62.564896 63.481186 64.985344 66.6926 67.359344 68.45804)
           (-1.8623571 21.941282 24.673748 25.741362 28.058521 28.941278 35.945377
            37.792686 38.938675 44.447235 45.368866 49.85141 49.94538 50.792694 53.692596
            54.378216 54.450607 56.25864 56.85141 56.91418 57.24102 59.359344 59.507217
            61.67988 62.115265 64.30328 65.74049 67.37821 67.6926 69.078384)
           (18.176456 21.176462 23.058517 24.789013 31.741364 32.78901 35.945377 39.93868
            41.247055 43.78902 45.970673 47.938675 50.458046 50.792694 53.692596 53.97067
            55.786133 56.679886 56.91418 57.985336 58.458046 60.359344 60.564903
            62.258636 63.564903 64.30328 65.98534 67.37821 68.359344 69.45805)
           (18.673748 21.176462 22.673738 22.941284 32.78901 32.938683 33.941284 39.93868
            40.556114 43.78902 45.447243 47.938675 50.851418 51.556114 53.359344 53.97067
            56.076157 56.11527 56.63797 58.241028 58.458046 60.115276 60.359344 61.851402
            63.564903 64.4812 65.98534 67.07616 68.359344 69.45805)
           (17.17646 18.673748 22.941284 25.673744 29.941292 32.938683 33.941284
            38.792694 39.93868 45.447243 46.368866 50.851418 50.945374 53.247055
            53.359344 55.37822 55.450615 56.11527 57.851418 57.914177 58.241028 59.564903
            60.359344 61.851402 63.11528 65.30328 66.7405 67.786125 68.6926 70.07838)
           (10.644891 20.741364 23.058517 25.673744 29.941292 31.741364 33.93868 37.94538
            39.93868 44.874863 46.368866 50.945374 51.258644 52.556114 54.359337
            55.450615 55.692593 56.679886 57.63797 57.851418 58.50785 60.028862 60.359344
            62.258636 63.11528 65.48119 66.7405 68.07616 68.6926 70.07838)
           (10.644891 17.058523 20.176455 25.741362 28.058521 31.789017 33.93868 37.94538
            38.938675 43.247055 47.7384 50.67988 50.945374 52.556114 54.359337 55.692593
            55.89172 56.25864 57.45805 57.63797 57.786125 59.359344 60.028862 61.67988
            62.564896 65.48119 66.50617 68.07616 68.6926 69.71221)
           (13.644882 17.058523 18.176456 25.741362 28.058521 33.93868 34.368866
            38.938675 39.792694 43.247055 49.938683 50.67988 51.94538 54.247063 54.359337
            56.25864 56.378212 57.29802 57.786125 58.078377 58.914185 59.359344 60.564903
            61.67988 63.028862 66.30329 67.28204 68.78612 69.6926 70.359344)
           (18.176456 20.741364 21.176462 23.058517 31.741364 32.78901 33.93868 39.792694
            39.93868 44.874863 48.738403 51.258644 51.94538 54.247063 54.359337 56.378212
            56.679886 56.891727 58.458046 58.50785 58.914185 60.359344 60.564903
            62.258636 63.564903 66.30329 67.506165 68.78612 69.6926 70.712204)
           (18.176456 20.741364 23.058517 31.058517 32.78901 33.93868 37.789017 39.792694
            43.94538 45.789013 47.970665 51.258644 51.94538 54.247063 54.359337 56.378212
            56.679886 56.891727 58.458046 58.914185 59.98534 60.564903 61.692596 63.45804
            64.67988 66.30329 67.506165 68.78612 69.6926 71.45804)
           (12.644875 22.176458 23.941286 27.741365 30.058514 33.789013 37.94538
            40.938675 41.556114 44.789017 46.447243 48.938675 50.945374 51.85141
            54.970665 55.692593 57.076157 57.63797 58.258644 59.24102 59.458042 61.359344
            62.02887 63.679886 64.564896 65.48119 66.98534 68.6926 69.359344 70.458046)
           (18.058514 19.176464 26.741358 27.673742 34.938683 35.368866 35.941284
            40.792686 42.941288 43.94538 47.44724 48.938675 51.679886 51.792694 55.359344
            55.450615 57.258636 57.340446 57.37822 59.078373 60.241024 61.564896
            61.692596 63.85141 65.115265 65.66771 66.7405 68.37822 69.359344 70.85141)
           (18.058514 23.176455 24.673748 26.741358 34.938683 35.941284 37.738403
            42.55612 43.94538 45.789013 47.44724 49.938683 51.679886 53.556114 55.359344
            56.891727 57.258636 58.076157 58.637978 59.712215 60.241024 61.692596
            62.115265 63.85141 65.56491 66.481186 67.506165 69.07616 70.359344 71.45804)
           (13.644882 23.176455 24.941277 28.741362 31.058517 34.789017 38.945374
            41.938683 42.55612 45.789013 47.44724 49.938683 51.94538 52.85141 55.970665
            56.692596 58.076157 58.637978 59.25864 60.241024 60.45804 62.359337 63.028862
            64.67988 65.56491 66.481186 67.985344 69.6926 70.359344 71.45804)
           (1.1376657 24.941277 27.673742 28.741362 31.058517 31.941284 38.945374
            40.792686 41.938683 47.44724 48.368874 52.85141 52.945374 53.792694 56.692596
            57.37822 57.45061 59.25864 59.85141 59.91418 60.241024 62.359337 62.507217
            64.67988 65.115265 67.30328 68.74049 70.37822 70.6926 72.07837)
           (21.176462 24.176456 26.058517 27.789013 34.741364 35.789017 38.945374
            42.93868 44.24706 46.789017 48.970665 50.938675 53.458046 53.792694 56.692596
            56.97067 58.786133 59.679886 59.91418 60.985336 61.458046 63.359344 63.564903
            65.25864 66.5649 67.30328 68.98534 70.37822 71.359344 72.45804)
           (21.673754 24.176456 25.673744 25.941284 35.789017 35.938675 36.941284
            42.93868 43.556114 46.789017 48.447235 50.938675 53.851402 54.55612 56.359344
            56.97067 59.076153 59.115273 59.637978 61.24102 61.458046 63.11528 63.359344
            64.85141 66.5649 67.4812 68.98534 70.07616 71.359344 72.45804)
           (20.176455 21.673754 25.941284 28.673738 32.94128 35.938675 36.941284
            41.792694 42.93868 48.447235 49.368866 53.851402 53.945377 56.24706 56.359344
            58.37822 58.450607 59.115273 60.851406 60.914177 61.24102 62.564896 63.359344
            64.85141 66.115265 68.3033 69.74049 70.78613 71.69259 73.07838)
           (13.644882 23.741364 26.058517 28.673738 32.94128 34.741364 36.938675
            40.945374 42.93868 47.874855 49.368866 53.945377 54.258644 55.556114
            57.359337 58.450607 58.692593 59.679886 60.637974 60.851406 61.50785
            63.028862 63.359344 65.25864 66.115265 68.48119 69.74049 71.07616 71.69259
            73.07838)
           (13.644882 20.058517 23.176455 28.741362 31.058517 34.789017 36.938675
            40.945374 41.938683 46.247063 50.7384 53.67988 53.945377 55.556114 57.359337
            58.692593 58.891724 59.25864 60.45804 60.637974 60.78613 62.359337 63.028862
            64.67988 65.56491 68.48119 69.50617 71.07616 71.69259 72.71221)
           (2.1376677 22.741362 31.941284 33.058514 34.789017 35.741364 39.94538
            41.792694 45.945374 47.789017 49.970665 53.258636 53.945377 54.792694
            57.692596 58.37822 58.891724 59.85141 60.45804 60.914177 61.985344 63.507225
            63.69258 66.25865 66.679886 68.3033 69.50617 71.37822 71.69259 73.458046)
           (14.644884 22.741362 29.673744 31.941284 35.741364 37.368866 39.94538
            43.556114 45.945374 49.970665 50.368866 53.258636 54.94538 56.556114
            57.692596 59.076153 59.85141 60.298016 61.078377 61.985344 62.170914 63.69258
            64.02887 66.25865 67.11527 69.13368 70.28204 72.07616 72.6926 74.07837)
           (14.644884 21.05852 24.176456 29.741364 32.058517 35.789017 37.938683
            41.945377 42.93868 47.24706 51.738396 54.679886 54.94538 56.556114 58.35934
            59.69259 59.891727 60.258636 61.458046 61.63797 61.786125 63.359344 64.02887
            65.679886 66.5649 69.4812 70.506165 72.07616 72.6926 73.712204)
           (17.644878 21.05852 22.176458 29.741364 32.058517 37.938683 38.368866 42.93868
            43.792694 47.24706 53.938683 54.679886 55.94538 58.247063 58.35934 60.258636
            60.378216 61.29802 61.786125 62.07837 62.914185 63.359344 64.564896 65.679886
            67.02887 70.30328 71.28206 72.78613 73.6926 74.35934)
           (22.176458 24.741365 25.176458 27.058514 35.741364 36.78901 37.938683
            43.792694 43.938675 48.874855 52.738403 55.258636 55.94538 58.247063 58.35934
            60.378216 60.679882 60.891724 62.45804 62.507843 62.914185 64.359344
            64.564896 66.25865 67.564896 70.30328 71.50618 72.78613 73.6926 74.712204)
           (18.644886 24.741365 27.058514 27.673742 35.741364 37.938683 39.368866
            43.938675 45.556107 48.874855 50.368866 52.938675 55.258636 56.556114
            58.35934 59.450603 60.679882 61.076157 61.63797 62.507843 63.07837 64.359344
            65.115265 66.25865 68.02887 69.4812 70.74049 72.07616 73.359344 74.07837)
           (18.644886 22.058521 23.176455 30.74136 33.058514 38.938675 39.368866
            43.938675 44.79269 48.247055 50.368866 52.938675 55.679886 55.792694
            59.359344 59.450603 61.258644 61.340446 61.37822 62.786133 63.07837 64.359344
            65.56491 66.679886 68.02887 69.66771 70.74049 72.37822 73.359344 74.07837)
           (23.176455 25.741362 26.176472 28.058521 36.74136 37.789017 38.938675 44.79269
            44.938683 48.78901 49.874855 52.938675 55.792694 56.25864 58.970665 59.359344
            61.340446 61.37822 61.67988 63.45804 63.507843 65.35934 65.56491 67.258644
            68.56491 69.66771 70.98534 72.37822 73.359344 74.45804)
           (23.176455 25.741362 26.176472 28.058521 36.74136 37.789017 38.938675 44.79269
            44.938683 49.874855 53.7384 56.25864 56.945377 59.24706 59.359344 61.37822
            61.67988 61.891724 63.45804 63.507843 63.914185 65.35934 65.56491 67.258644
            68.56491 71.3033 72.50617 73.786125 74.69259 75.71221)
           (16.64487 26.176472 27.941282 31.741364 34.05852 37.789017 41.945377 44.938683
            45.556107 50.44724 53.7384 55.851414 56.945377 58.55611 59.69259 61.076157
            61.891724 62.258636 63.241028 63.45804 64.17091 65.35934 66.02887 67.679886
            68.56491 71.13367 72.50617 74.07616 74.69259 75.71221)
           (16.64487 23.058517 26.176472 31.741364 34.05852 37.789017 39.93868 43.94538
            44.938683 49.247063 53.7384 56.679886 56.945377 58.55611 60.359344 61.692596
            61.891724 62.258636 63.45804 63.63797 63.786133 65.35934 66.02887 67.679886
            68.56491 71.48119 72.50617 74.07616 74.69259 75.71221)
           (19.644894 23.058517 24.176456 31.741364 34.05852 39.93868 40.368862 44.938683
            45.792698 49.247063 55.938675 56.679886 57.945374 60.247055 60.359344
            62.258636 62.37822 63.29802 63.786133 64.07837 64.914185 65.35934 66.5649
            67.679886 69.02887 72.30329 73.28204 74.78613 75.69258 76.359344)
           (24.176456 26.741358 27.176462 29.058525 37.741356 38.78901 39.93868 45.792698
            45.93868 50.87486 54.738403 57.258636 57.945374 60.247055 60.359344 62.37822
            62.67987 62.891724 64.458046 64.50785 64.914185 66.359344 66.5649 68.25864
            69.5649 72.30329 73.506165 74.78613 75.69258 76.712204)
           (24.176456 26.741358 29.058525 37.05852 38.78901 39.93868 43.78902 45.792698
            49.94538 51.789013 53.97067 57.258636 57.945374 60.247055 60.359344 62.37822
            62.67987 62.891724 64.458046 64.914185 65.98534 66.5649 67.6926 69.45805
            70.679886 72.30329 73.506165 74.78613 75.69258 77.45805)
           (18.644886 28.176453 29.941292 33.74136 36.058517 39.789013 43.94538 46.938683
            47.556118 50.78901 52.447235 54.93868 56.945377 57.851418 60.970665 61.692596
            63.07615 63.63797 64.25864 65.24101 65.45805 67.359344 68.02887 69.679886
            70.564896 71.48119 72.98534 74.69259 75.359344 76.458046)
           (24.05852 25.176458 32.741364 33.673748 40.938675 41.36887 41.94129 46.792694
            48.941284 49.94538 53.447243 54.93868 57.679882 57.792698 61.359344 61.450607
            63.258636 63.340454 63.37822 65.07837 66.24103 67.564896 67.6926 69.85142
            71.115265 71.667694 72.74049 74.37822 75.359344 76.85141)
           (24.05852 29.17646 30.673748 32.741364 40.938675 41.94129 43.738403 48.556114
            49.94538 51.789013 53.447243 55.938675 57.679882 59.556118 61.359344
            62.891724 63.258636 64.07616 64.63797 65.712204 66.24103 67.6926 68.115265
            69.85142 71.564896 72.4812 73.506165 75.07615 76.359344 77.45805)
           (19.644894 29.17646 30.941284 34.741364 37.05852 40.789017 44.945377 47.938675
            48.556114 51.789013 53.447243 55.938675 57.945374 58.851414 61.970673
            62.69259 64.07616 64.63797 65.25864 66.24103 66.45805 68.359344 69.02887
            70.679886 71.564896 72.4812 73.985344 75.69258 76.359344 77.45805)
           (7.1376686 30.941284 33.673748 34.741364 37.05852 37.941277 44.945377
            46.792694 47.938675 53.447243 54.368866 58.851414 58.94538 59.79269 62.69259
            63.37822 63.450607 65.25864 65.85141 65.91419 66.24103 68.359344 68.50722
            70.679886 71.115265 73.30328 74.74049 76.37821 76.6926 78.07837)
           (27.176462 30.176456 32.058517 33.789013 40.741364 41.789017 44.945377
            48.938675 50.24706 52.789017 54.970665 56.938683 59.458042 59.79269 62.69259
            62.970673 64.786125 65.679886 65.91419 66.98534 67.458046 69.359344 69.5649
            71.25864 72.5649 73.30328 74.98534 76.37821 77.35934 78.45805)
           (27.673742 30.176456 31.673752 31.941284 41.789017 41.938683 42.941288
            48.938675 49.55611 52.789017 54.447243 56.938683 59.85141 60.556114 62.359337
            62.970673 65.07616 65.115265 65.63797 67.24102 67.458046 69.115265 69.359344
            70.85141 72.5649 73.4812 74.98534 76.07616 77.35934 78.45805)
           (26.176472 27.673742 31.941284 34.673737 38.941284 41.938683 42.941288
            47.79269 48.938675 54.447243 55.368862 59.85141 59.945377 62.247055 62.359337
            64.37821 64.45061 65.115265 66.85142 66.914185 67.24102 68.56491 69.359344
            70.85141 72.11528 74.3033 75.74049 76.786125 77.69259 79.07838)
           (19.644894 29.741364 32.058517 34.673737 38.941284 40.741364 42.93868 46.94538
            48.938675 53.87486 55.368862 59.945377 60.258636 61.556107 63.359344 64.45061
            64.6926 65.679886 66.63797 66.85142 67.50785 69.02887 69.359344 71.25864
            72.11528 74.48119 75.74049 77.07616 77.69259 79.07838)
           (19.644894 26.058517 29.17646 34.741364 37.05852 40.789017 42.93868 46.94538
            47.938675 52.247055 56.738403 59.679886 59.945377 61.556107 63.359344 64.6926
            64.89173 65.25864 66.45805 66.63797 66.78613 68.359344 69.02887 70.679886
            71.564896 74.48119 75.50617 77.07616 77.69259 78.71221)
           (8.137679 28.741362 37.941277 39.058517 40.789017 41.741356 45.945374 47.79269
            51.94538 53.789017 55.970665 59.25864 59.945377 60.792686 63.69258 64.37821
            64.89173 65.85141 66.45805 66.914185 67.985344 69.507225 69.6926 72.25864
            72.679886 74.3033 75.50617 77.37822 77.69259 79.458046)
           (20.644878 28.741362 35.673744 37.941277 41.741356 43.368862 45.945374
            49.55611 51.94538 55.970665 56.368866 59.25864 60.945374 62.556107 63.69258
            65.07616 65.85141 66.29802 67.07838 67.985344 68.17091 69.6926 70.02887
            72.25864 73.11527 75.13368 76.28204 78.07615 78.6926 80.07837)
           (20.644878 27.058514 30.176456 35.741364 38.058525 41.789017 43.938675
            47.945377 48.938675 53.247055 57.738403 60.679882 60.945374 62.556107
            64.359344 65.69259 65.89172 66.25865 67.458046 67.63797 67.786125 69.359344
            70.02887 71.679886 72.5649 75.481186 76.506165 78.07615 78.6926 79.712204)
           (20.644878 29.673744 33.941284 37.741356 40.058517 43.938675 44.94128
            47.945377 50.938675 54.789013 56.447235 60.945374 61.851402 62.556107
            64.359344 64.97066 65.69259 67.11527 67.63797 68.25864 69.24103 70.02887
            71.359344 72.85141 73.67988 75.481186 76.985344 78.07615 78.6926 80.45804)
           (20.644878 24.741365 29.673744 33.941284 37.741356 41.945377 43.938675
            47.945377 48.874855 51.970665 55.258636 59.69259 60.945374 61.851402
            62.507843 62.556107 63.98535 64.359344 64.97066 65.69259 67.11527 67.63797
            68.25864 69.03319 70.02887 74.532 75.481186 76.985344 78.07615 78.6926)
           (21.05852 24.176456 35.789017 41.945377 42.93868 47.24706 51.738396 54.679886
            54.94538 59.69259 59.891727 61.458046 61.63797 61.786125 63.359344 66.5649
            69.4812 70.506165 72.6926 73.712204)
           (14.644884 21.05852 24.176456 29.741364 32.058517 35.789017 37.938683
            41.945377 42.93868 47.24706 51.738396 54.679886 54.94538 56.556114 58.35934
            59.69259 59.891727 60.258636 61.458046 61.63797 61.786125 63.359344 64.02887
            65.679886 66.5649 69.4812 70.506165 72.07616 72.6926 73.712204)
           (17.644878 21.05852 22.176458 29.741364 32.058517 37.938683 38.368866 42.93868
            43.792694 47.24706 53.938683 54.679886 55.94538 58.247063 58.35934 60.258636
            60.378216 61.29802 61.786125 62.07837 62.914185 63.359344 64.564896 65.679886
            67.02887 70.30328 71.28206 72.78613 73.6926 74.35934)
           (22.176458 24.741365 25.176458 27.058514 35.741364 36.78901 37.938683
            43.792694 43.938675 48.874855 52.738403 55.258636 55.94538 58.247063 58.35934
            60.378216 60.679882 60.891724 62.45804 62.507843 62.914185 64.359344
            64.564896 66.25865 67.564896 70.30328 71.50618 72.78613 73.6926 74.712204)
           (18.644886 24.741365 27.058514 27.673742 35.741364 37.938683 39.368866
            43.938675 45.556107 48.874855 50.368866 52.938675 55.258636 56.556114
            58.35934 59.450603 60.679882 61.076157 61.63797 62.507843 63.07837 64.359344
            65.115265 66.25865 68.02887 69.4812 70.74049 72.07616 73.359344 74.07837)
           (18.644886 22.058521 23.176455 30.74136 33.058514 38.938675 39.368866
            43.938675 44.79269 48.247055 50.368866 52.938675 55.679886 55.792694
            59.359344 59.450603 61.258644 61.340446 61.37822 62.786133 63.07837 64.359344
            65.56491 66.679886 68.02887 69.66771 70.74049 72.37822 73.359344 74.07837))))
   (loop
      for x in times
for next in (cdr times)
for chord in chordlist
append (loop for p in chord collect (list x p (- next x))))))


(length
  (group-to-chords (svg->points :infile #P"/tmp/test2.svg" :quantize t)))

(points->svg
 (let* ((num 255)
        (times (mapcar (lambda (x) (/ x 1000))
                       (ou:integrate (loop for x below num collect (* 6000 (expt 1/60 (/ x (- num 1))))))))
        (chordlist
         (group-to-chords (svg->points :infile #P"/tmp/test2.svg" :quantize t))))
   (loop
      for x in times
      for next in (cdr times)
      for chord in chordlist
      append (loop for p in chord collect (list x p))))
 :outfile "/tmp/test4.svg")

(svg->points :infile #P"/tmp/test4.svg" :quantize t)


(loop for x below 100 collect (lilypond::create-ts))

(defun mysquare (x) (progn (break "x: ~a" x) (* x x)))

(mysquare 24)

(svg->lwav :infile #P"/tmp/test4.svg" :outfile "test-schluss02" :db -20 :quantize nil)

(let* ((num 190)
       (times (mapcar (lambda (x) (/ x 1000))
                      (ou:integrate (loop for x below num collect (* 6000 (expt 1/60 (/ x (- num 1))))))))
       (chordlist
        '((8.644881 12.741365 17.673746 21.941282 25.741362 29.945377 31.938675
           35.945377 36.874855 39.970665 43.258636 47.69259 48.94538 49.85141 50.507847
           50.556107 51.985344 52.35934 52.97066 53.692596 55.115273 55.637978 56.25864
           57.033188 58.02887 62.531998 63.481186 64.985344 66.07615 66.6926)
          (9.058519 12.176456 23.789017 29.945377 30.938679 35.24706 39.738396 42.679886
           42.94538 47.69259 47.891727 49.458046 49.63797 49.786125 51.359344 54.564903
           57.481194 58.506165 60.692596 61.712204)
          (2.644884 9.058519 12.176456 17.741364 20.058517 23.789017 25.938675 29.945377
           30.938679 35.24706 39.738396 42.679886 42.94538 44.556114 46.35934 47.69259
           47.891727 48.258636 49.458046 49.63797 49.786125 51.359344 52.028862 53.67988
           54.564903 57.481194 58.506165 60.07616 60.692596 61.712204)
          (5.6448784 9.058519 10.176458 17.741364 20.058517 25.938675 26.368864
           30.938679 31.792694 35.24706 41.938683 42.679886 43.94538 46.247063 46.35934
           48.258636 48.378216 49.298023 49.786125 50.078373 50.914185 51.359344
           52.564903 53.67988 55.028862 58.303288 59.282055 60.78613 61.692596
           62.359337)
          (10.176458 12.741365 13.176458 15.058514 23.741364 24.789013 25.938675
           31.792694 31.938675 36.874855 40.738403 43.258636 43.94538 46.247063 46.35934
           48.378216 48.679882 48.89173 50.458046 50.507847 50.914185 52.35934 52.564903
           54.258644 55.5649 58.303288 59.506172 60.78613 61.692596 62.712204)
          (10.176458 12.741365 15.058514 23.058517 24.789013 25.938675 29.789017
           31.792694 35.945377 37.789017 39.970665 43.258636 43.94538 46.247063 46.35934
           48.378216 48.679882 48.89173 50.458046 50.914185 51.985344 52.564903
           53.692596 55.458046 56.679886 58.303288 59.506172 60.78613 61.692596
           63.45804)
          (4.644868 14.176472 15.941282 19.741362 22.058521 25.78901 29.945377 32.938683
           33.556107 36.78901 38.44724 40.938675 42.94538 43.851414 46.970665 47.69259
           49.076157 49.63797 50.25864 51.241024 51.458046 53.359344 54.02887 55.679886
           56.564907 57.481194 58.985336 60.692596 61.359344 62.45804)
          (10.058521 11.176452 18.74136 19.67375 26.938677 27.368866 27.941282 32.792694
           34.941277 35.945377 39.447243 40.938675 43.679886 43.792694 47.359344
           47.450603 49.258644 49.340446 49.378223 51.078377 52.24102 53.564903
           53.692596 55.851414 57.115273 57.667706 58.740498 60.378216 61.359344
           62.85141)
          (10.058521 15.176462 16.673738 18.74136 26.938677 27.941282 29.7384 34.55611
           35.945377 37.789017 39.447243 41.938683 43.679886 45.556107 47.359344
           48.89173 49.258644 50.076153 50.637966 51.71221 52.24102 53.692596 54.115265
           55.851414 57.564903 58.481194 59.506172 61.076157 62.359337 63.45804)
          (5.6448784 15.176462 16.941278 20.741364 23.058517 26.789011 30.945381
           33.93868 34.55611 37.789017 39.447243 41.938683 43.94538 44.85141 47.970665
           48.692596 50.076153 50.637966 51.258644 52.24102 52.45804 54.359337 55.028862
           56.679886 57.564903 58.481194 59.98534 61.692596 62.359337 63.45804)
          (-6.8623466 16.941278 19.67375 20.741364 23.058517 23.941286 30.945381
           32.792694 33.93868 39.447243 40.368862 44.85141 44.945377 45.792698 48.692596
           49.378223 49.450615 51.258644 51.85141 51.914185 52.24102 54.359337 54.507217
           56.679886 57.115273 59.303295 60.74049 62.37822 62.69259 64.07837)
          (13.176458 16.176453 18.058514 19.789015 26.741358 27.789013 30.945381
           34.938683 36.247055 38.78901 40.97066 42.93868 45.45805 45.792698 48.692596
           48.970665 50.786133 51.679886 51.914185 52.985336 53.458046 55.359344 55.5649
           57.258636 58.5649 59.303295 60.985336 62.37822 63.359344 64.458046)
          (13.673744 16.176453 17.673746 17.941292 27.789013 27.938679 28.941278
           34.938683 35.556118 38.78901 40.447235 42.93868 45.851418 46.55611 48.359344
           48.970665 51.07615 51.115273 51.637974 53.24102 53.458046 55.115273 55.359344
           56.85141 58.5649 59.481194 60.985336 62.076157 63.359344 64.458046)
          (12.176456 13.673744 17.941292 20.67374 24.941277 27.938679 28.941278
           33.792698 34.938683 40.447235 41.36887 45.851418 45.945374 48.247055
           48.359344 50.37822 50.450607 51.115273 52.85141 52.914177 53.24102 54.564903
           55.359344 56.85141 58.115265 60.30329 61.740486 62.786133 63.69258 65.07837)
          (5.6448784 15.741365 18.058514 20.67374 24.941277 26.741358 28.93868 32.945377
           34.938683 39.874855 41.36887 45.945374 46.258644 47.556118 49.35934 50.450607
           50.692596 51.679886 52.63797 52.85141 53.50785 55.028862 55.359344 57.258636
           58.115265 60.4812 61.740486 63.07615 63.69258 65.07837)
          (5.6448784 12.058519 15.176462 20.741364 23.058517 26.789011 28.93868
           32.945377 33.93868 38.24706 42.738403 45.679882 45.945374 47.556118 49.35934
           50.692596 50.891724 51.258644 52.45804 52.63797 52.786125 54.359337 55.028862
           56.679886 57.564903 60.4812 61.506165 63.07615 63.69258 64.712204)
          (-5.862336 14.741358 23.941286 25.058521 26.789011 27.741365 31.945381
           33.792698 37.94538 39.789013 41.97067 45.258636 45.945374 46.792694 49.692593
           50.37822 50.891724 51.85141 52.45804 52.914177 53.98534 55.50722 55.692593
           58.258644 58.67989 60.30329 61.506165 63.37822 63.69258 65.45805)
          (6.644886 14.741358 21.673754 23.941286 27.741365 29.36887 31.945381 35.556118
           37.94538 41.97067 42.368866 45.258636 46.94538 48.556114 49.692593 51.07615
           51.85141 52.298023 53.078377 53.98534 54.170918 55.692593 56.028866 58.258644
           59.115273 61.133675 62.282043 64.07616 64.6926 66.07837)
          (6.644886 13.058521 16.176453 21.74136 24.05852 27.789013 29.938675 33.945374
           34.938683 39.247063 43.738403 46.67989 46.94538 48.556114 50.359337 51.69259
           51.89173 52.258636 53.458046 53.63797 53.786125 55.359344 56.028866 57.679882
           58.5649 61.4812 62.50618 64.07616 64.6926 65.712204)
          (9.644863 13.058521 14.176472 21.74136 24.05852 29.938675 30.368866 34.938683
           35.79269 39.247063 45.93868 46.67989 47.945377 50.24706 50.359337 52.258636
           52.378212 53.29802 53.786125 54.078377 54.914185 55.359344 56.564907
           57.679882 59.028866 62.3033 63.28205 64.786125 65.69259 66.359344)
          (14.176472 16.741362 17.17646 19.058516 27.741365 28.789015 29.938675 35.79269
           35.938675 40.874863 44.738403 47.25864 47.945377 50.24706 50.359337 52.378212
           52.679886 52.891724 54.458046 54.50785 54.914185 56.359344 56.564907
           58.258644 59.564903 62.3033 63.506172 64.786125 65.69259 66.71221)
          (14.176472 16.741362 19.058516 27.058514 28.789015 29.938675 33.789013
           35.79269 39.94538 41.789017 43.970665 47.25864 47.945377 50.24706 50.359337
           52.378212 52.679886 52.891724 54.458046 54.914185 55.985344 56.564907
           57.692596 59.458042 60.679882 62.3033 63.506172 64.786125 65.69259 67.458046)
          (8.644881 18.176456 19.941284 23.741364 26.058517 29.789017 33.945374
           36.938675 37.55611 40.789017 42.447243 44.938683 46.94538 47.85141 50.97067
           51.69259 53.076157 53.63797 54.258644 55.241028 55.458046 57.359337 58.02887
           59.679886 60.564903 61.4812 62.985336 64.6926 65.35934 66.45805)
          (14.058517 15.176462 22.741362 23.673746 30.938679 31.368862 31.941284
           36.792694 38.941284 39.94538 43.447235 44.938683 47.679886 47.79269 51.359344
           51.450615 53.258636 53.340443 53.37822 55.07837 56.24102 57.564903 57.692596
           59.85141 61.115273 61.6677 62.740494 64.37821 65.35934 66.85142)
          (14.058517 19.176464 20.67374 22.741362 30.938679 31.941284 33.738403
           38.556107 39.94538 41.789017 43.447235 45.93868 47.679886 49.55611 51.359344
           52.891724 53.258636 54.076157 54.637974 55.71221 56.24102 57.692596 58.115265
           59.85141 61.564896 62.481194 63.506172 65.07616 66.359344 67.458046)
          (9.644863 19.176464 20.941286 24.741365 27.058514 30.78901 34.94538 37.938683
           38.556107 41.789017 43.447235 45.93868 47.945377 48.851406 51.970665
           52.692596 54.076157 54.637974 55.258636 56.24102 56.458042 58.35934 59.028866
           60.679882 61.564896 62.481194 63.98535 65.69259 66.359344 67.458046)
          (-2.8623562 20.941286 23.673746 24.741365 27.058514 27.941282 34.94538
           36.792694 37.938683 43.447235 44.368866 48.851406 48.94538 49.792686
           52.692596 53.37822 53.450607 55.258636 55.851414 55.914185 56.24102 58.35934
           58.50722 60.679882 61.115273 63.30329 64.74049 66.37822 66.6926 68.07837)
          (17.17646 20.176455 22.058521 23.789017 30.74136 31.789017 34.94538 38.938675
           40.247055 42.789013 44.97067 46.938683 49.458046 49.792686 52.692596 52.97066
           54.78613 55.679886 55.914185 56.98534 57.45805 59.359344 59.564903 61.258644
           62.564896 63.30329 64.985344 66.37822 67.359344 68.45804)
          (17.673746 20.176455 21.673754 21.941282 31.789017 31.938675 32.94128
           38.938675 39.556114 42.789013 44.447235 46.938683 49.85141 50.556107 52.35934
           52.97066 55.076164 55.115273 55.637978 57.24102 57.45805 59.115273 59.359344
           60.851406 62.564896 63.481186 64.985344 66.07615 67.359344 68.45804)
          (16.176453 17.673746 21.941282 24.673748 28.941278 31.938675 32.94128
           37.792686 38.938675 44.447235 45.368866 49.85141 49.94538 52.247055 52.35934
           54.378216 54.450607 55.115273 56.85141 56.91418 57.24102 58.5649 59.359344
           60.851406 62.115265 64.30328 65.74049 66.78613 67.6926 69.078384)
          (9.644863 19.741362 22.058521 24.673748 28.941278 30.74136 32.938683 36.94538
           38.938675 43.874855 45.368866 49.94538 50.25864 51.556114 53.359344 54.450607
           54.69259 55.679886 56.63797 56.85141 57.50785 59.028866 59.359344 61.258644
           62.115265 64.4812 65.74049 67.07616 67.6926 69.078384)
          (9.644863 16.058521 19.176464 24.741365 27.058514 30.78901 32.938683 36.94538
           37.938683 42.247063 46.738396 49.67988 49.94538 51.556114 53.359344 54.69259
           54.89173 55.258636 56.458042 56.63797 56.786125 58.35934 59.028866 60.679882
           61.564896 64.4812 65.506165 67.07616 67.6926 68.71222)
          (9.644863 16.058521 19.176464 24.741365 27.058514 30.78901 32.938683 36.94538
           37.938683 42.247063 46.738396 49.67988 49.94538 51.556114 53.359344 54.69259
           54.89173 55.258636 56.458042 56.63797 56.786125 58.35934 59.028866 60.679882
           61.564896 64.4812 65.506165 67.07616 67.6926 68.71222)
          (-1.8623571 18.74136 27.941282 29.058525 30.78901 31.741364 35.945377
           37.792686 41.945377 43.78902 45.970673 49.258644 49.94538 50.792694 53.692596
           54.378216 54.89173 55.851414 56.458042 56.91418 57.985336 59.507217 59.69259
           62.258636 62.67987 64.30328 65.506165 67.37821 67.6926 69.45805)
          (10.644891 18.74136 25.673744 27.941282 31.741364 33.368866 35.945377
           39.556114 41.945377 45.970673 46.368866 49.258644 50.945374 52.556114
           53.692596 55.076164 55.851414 56.29801 57.078377 57.985336 58.170906 59.69259
           60.028862 62.258636 63.11528 65.13368 66.28205 68.07616 68.6926 70.07838)
          (10.644891 17.058523 20.176455 25.741362 28.058521 31.789017 33.93868 37.94538
           38.938675 43.247055 47.7384 50.67988 50.945374 52.556114 54.359337 55.692593
           55.89172 56.25864 57.45805 57.63797 57.786125 59.359344 60.028862 61.67988
           62.564896 65.48119 66.50617 68.07616 68.6926 69.71221)
          (-0.8623009 19.741362 28.941278 30.058514 31.789017 32.741364 36.94538
           38.792694 42.94538 44.789017 46.970665 50.25864 50.945374 51.792694 54.69259
           55.37822 55.89172 56.85141 57.45805 57.914177 58.985336 60.507214 60.692596
           63.258636 63.679886 65.30328 66.50617 68.37822 68.6926 70.458046)
          (11.64489 19.741362 26.673746 28.941278 32.741364 34.368866 36.94538 40.556114
           42.94538 46.970665 47.368866 50.25864 51.94538 53.556114 54.69259 56.076157
           56.85141 57.29802 58.078377 58.985336 59.170914 60.692596 61.028854 63.258636
           64.11527 66.13368 67.28204 69.07616 69.6926 71.07837)
          (11.64489 18.058514 21.176462 26.741358 29.058525 32.78901 34.938683 38.945374
           39.93868 44.24706 48.738403 51.679886 51.94538 53.556114 55.359344 56.692596
           56.891727 57.258636 58.458046 58.637978 58.786133 60.359344 61.028854
           62.67987 63.564903 66.481186 67.506165 69.07616 69.6926 70.712204)
          (11.64489 18.058514 21.176462 26.741358 29.058525 32.78901 34.938683 38.945374
           39.93868 44.24706 48.738403 51.679886 51.94538 53.556114 55.359344 56.692596
           56.891727 57.258636 58.458046 58.637978 58.786133 60.359344 61.028854
           62.67987 63.564903 66.481186 67.506165 69.07616 69.6926 70.712204)
          (14.644884 18.058514 19.176464 26.741358 29.058525 34.938683 35.368866
           39.93868 40.792686 44.24706 50.938675 51.679886 52.945374 55.247055 55.359344
           57.258636 57.37822 58.29802 58.786133 59.078373 59.91418 60.359344 61.564896
           62.67987 64.02887 67.30328 68.28206 69.78613 70.6926 71.359344)
          (19.176464 21.74136 22.176458 24.05852 32.741364 33.789013 34.938683 40.792686
           40.938675 45.874847 49.738403 52.258636 52.945374 55.247055 55.359344
           57.37822 57.679882 57.891724 59.458042 59.507847 59.91418 61.359344 61.564896
           63.258636 64.564896 67.30328 68.506165 69.78613 70.6926 71.71222)
          (15.644875 21.74136 24.05852 24.673748 32.741364 34.938683 36.368874 40.938675
           42.55612 45.874847 47.368866 49.938683 52.258636 53.556114 55.359344
           56.450615 57.679882 58.076157 58.637978 59.507847 60.07837 61.359344
           62.115265 63.258636 65.02887 66.481186 67.74049 69.07616 70.359344 71.07837)
          (15.644875 19.058516 20.176455 27.741365 30.058514 35.938675 36.368874
           40.938675 41.792694 45.247055 47.368866 49.938683 52.679886 52.792686
           56.359344 56.450615 58.258644 58.34044 58.37822 59.786133 60.07837 61.359344
           62.564896 63.679886 65.02887 66.66771 67.74049 69.37822 70.359344 71.07837)
          (20.176455 22.741362 23.176455 25.058521 33.74136 34.789017 35.938675
           41.792694 41.938683 45.789013 46.874855 49.938683 52.792686 53.258636
           55.970665 56.359344 58.34044 58.37822 58.67989 60.45804 60.507854 62.359337
           62.564896 64.25864 65.56491 66.66771 67.985344 69.37822 70.359344 71.45804)
          (20.176455 22.741362 23.176455 25.058521 33.74136 34.789017 35.938675
           41.792694 41.938683 46.874855 50.7384 53.258636 53.945377 56.24706 56.359344
           58.37822 58.67989 58.891724 60.45804 60.507854 60.914177 62.359337 62.564896
           64.25864 65.56491 68.3033 69.50617 70.78613 71.69259 72.71221)
          (13.644882 23.176455 24.941277 28.741362 31.058517 34.789017 38.945374
           41.938683 42.55612 47.44724 50.7384 52.85141 53.945377 55.556114 56.692596
           58.076157 58.891724 59.25864 60.241024 60.45804 61.170914 62.359337 63.028862
           64.67988 65.56491 68.13368 69.50617 71.07616 71.69259 72.71221)
          (13.644882 20.058517 23.176455 28.741362 31.058517 34.789017 36.938675
           40.945374 41.938683 46.247063 50.7384 53.67988 53.945377 55.556114 57.359337
           58.692593 58.891724 59.25864 60.45804 60.637974 60.78613 62.359337 63.028862
           64.67988 65.56491 68.48119 69.50617 71.07616 71.69259 72.71221)
          (2.1376677 22.741362 31.941284 33.058514 34.789017 35.741364 39.94538
           41.792694 45.945374 47.789017 49.970665 53.258636 53.945377 54.792694
           57.692596 58.37822 58.891724 59.85141 60.45804 60.914177 61.985344 63.507225
           63.69258 66.25865 66.679886 68.3033 69.50617 71.37822 71.69259 73.458046)
          (14.644884 22.741362 29.673744 31.941284 35.741364 37.368866 39.94538
           43.556114 45.945374 49.970665 50.368866 53.258636 54.94538 56.556114
           57.692596 59.076153 59.85141 60.298016 61.078377 61.985344 62.170914 63.69258
           64.02887 66.25865 67.11527 69.13368 70.28204 72.07616 72.6926 74.07837)
          (14.644884 21.05852 24.176456 29.741364 32.058517 35.789017 37.938683
           41.945377 42.93868 47.24706 51.738396 54.679886 54.94538 56.556114 58.35934
           59.69259 59.891727 60.258636 61.458046 61.63797 61.786125 63.359344 64.02887
           65.679886 66.5649 69.4812 70.506165 72.07616 72.6926 73.712204)
          (14.644884 23.673746 27.941282 31.741364 34.05852 37.938683 38.941284
           41.945377 44.938683 48.78901 50.44724 54.94538 55.851414 56.556114 58.35934
           58.970665 59.69259 61.115273 61.63797 62.258636 63.241028 64.02887 65.35934
           66.85142 67.679886 69.4812 70.98534 72.07616 72.6926 74.45804)
          (14.644884 18.74136 23.673746 27.941282 31.741364 35.945377 37.938683
           41.945377 42.874855 45.970673 49.258644 53.692596 54.94538 55.851414 56.50785
           56.556114 57.985336 58.35934 58.970665 59.69259 61.115273 61.63797 62.258636
           63.033188 64.02887 68.532005 69.4812 70.98534 72.07616 72.6926)
          (15.058514 18.176456 29.789017 35.945377 36.938675 41.247055 45.738403
           48.679882 48.94538 53.692596 53.891727 55.458046 55.637978 55.786133
           57.359337 60.564903 63.481186 64.506165 66.6926 67.712204)
          (8.644881 15.058514 18.176456 23.741364 26.058517 29.789017 31.938675
           35.945377 36.938675 41.247055 45.738403 48.679882 48.94538 50.556107 52.35934
           53.692596 53.891727 54.258644 55.458046 55.637978 55.786133 57.359337
           58.02887 59.679886 60.564903 63.481186 64.506165 66.07615 66.6926 67.712204)
          (11.64489 15.058514 16.176453 23.741364 26.058517 31.938675 32.368866
           36.938675 37.792686 41.247055 47.938675 48.679882 49.94538 52.247055 52.35934
           54.258644 54.378216 55.298023 55.786133 56.078373 56.91418 57.359337 58.5649
           59.679886 61.028854 64.30328 65.28204 66.78613 67.6926 68.359344)
          (16.176453 18.74136 19.176464 21.05852 29.741364 30.78901 31.938675 37.792686
           37.938683 42.874855 46.738396 49.258644 49.94538 52.247055 52.35934 54.378216
           54.679886 54.89173 56.458042 56.50785 56.91418 58.35934 58.5649 60.258636
           61.564896 64.30328 65.506165 66.78613 67.6926 68.71222)
          (12.644875 18.74136 21.05852 21.673754 29.741364 31.938675 33.368866 37.938683
           39.556114 42.874855 44.368866 46.938683 49.258644 50.556107 52.35934
           53.450607 54.679886 55.076164 55.637978 56.50785 57.078377 58.35934 59.115273
           60.258636 62.02887 63.481186 64.74049 66.07615 67.359344 68.07837)
          (12.644875 16.058521 17.17646 24.741365 27.058514 32.938683 33.368866
           37.938683 38.792694 42.247063 44.368866 46.938683 49.67988 49.792686
           53.359344 53.450607 55.258636 55.340446 55.37822 56.786125 57.078377 58.35934
           59.564903 60.679882 62.02887 63.66771 64.74049 66.37822 67.359344 68.07837)
          (17.17646 19.741362 20.176455 22.058521 30.74136 31.789017 32.938683 38.792694
           38.938675 42.789013 43.874855 46.938683 49.792686 50.25864 52.97066 53.359344
           55.340446 55.37822 55.679886 57.45805 57.50785 59.359344 59.564903 61.258644
           62.564896 63.66771 64.985344 66.37822 67.359344 68.45804)
          (17.17646 19.741362 20.176455 22.058521 30.74136 31.789017 32.938683 38.792694
           38.938675 43.874855 47.7384 50.25864 50.945374 53.247055 53.359344 55.37822
           55.679886 55.89172 57.45805 57.50785 57.914177 59.359344 59.564903 61.258644
           62.564896 65.30328 66.50617 67.786125 68.6926 69.71221)
          (10.644891 20.176455 21.941282 25.741362 28.058521 31.789017 35.945377
           38.938675 39.556114 44.447235 47.7384 49.85141 50.945374 52.556114 53.692596
           55.076164 55.89172 56.25864 57.24102 57.45805 58.170906 59.359344 60.028862
           61.67988 62.564896 65.13368 66.50617 68.07616 68.6926 69.71221)
          (10.644891 17.058523 20.176455 25.741362 28.058521 31.789017 33.93868 37.94538
           38.938675 43.247055 47.7384 50.67988 50.945374 52.556114 54.359337 55.692593
           55.89172 56.25864 57.45805 57.63797 57.786125 59.359344 60.028862 61.67988
           62.564896 65.48119 66.50617 68.07616 68.6926 69.71221)
          (13.644882 17.058523 18.176456 25.741362 28.058521 33.93868 34.368866
           38.938675 39.792694 43.247055 49.938683 50.67988 51.94538 54.247063 54.359337
           56.25864 56.378212 57.29802 57.786125 58.078377 58.914185 59.359344 60.564903
           61.67988 63.028862 66.30329 67.28204 68.78612 69.6926 70.359344)
          (18.176456 20.741364 21.176462 23.058517 31.741364 32.78901 33.93868 39.792694
           39.93868 44.874863 48.738403 51.258644 51.94538 54.247063 54.359337 56.378212
           56.679886 56.891727 58.458046 58.50785 58.914185 60.359344 60.564903
           62.258636 63.564903 66.30329 67.506165 68.78612 69.6926 70.712204)
          (18.176456 20.741364 23.058517 31.058517 32.78901 33.93868 37.789017 39.792694
           43.94538 45.789013 47.970665 51.258644 51.94538 54.247063 54.359337 56.378212
           56.679886 56.891727 58.458046 58.914185 59.98534 60.564903 61.692596 63.45804
           64.67988 66.30329 67.506165 68.78612 69.6926 71.45804)
          (12.644875 22.176458 23.941286 27.741365 30.058514 33.789013 37.94538
           40.938675 41.556114 44.789017 46.447243 48.938675 50.945374 51.85141
           54.970665 55.692593 57.076157 57.63797 58.258644 59.24102 59.458042 61.359344
           62.02887 63.679886 64.564896 65.48119 66.98534 68.6926 69.359344 70.458046)
          (18.058514 19.176464 26.741358 27.673742 34.938683 35.368866 35.941284
           40.792686 42.941288 43.94538 47.44724 48.938675 51.679886 51.792694 55.359344
           55.450615 57.258636 57.340446 57.37822 59.078373 60.241024 61.564896
           61.692596 63.85141 65.115265 65.66771 66.7405 68.37822 69.359344 70.85141)
          (18.058514 23.176455 24.673748 26.741358 34.938683 35.941284 37.738403
           42.55612 43.94538 45.789013 47.44724 49.938683 51.679886 53.556114 55.359344
           56.891727 57.258636 58.076157 58.637978 59.712215 60.241024 61.692596
           62.115265 63.85141 65.56491 66.481186 67.506165 69.07616 70.359344 71.45804)
          (13.644882 23.176455 24.941277 28.741362 31.058517 34.789017 38.945374
           41.938683 42.55612 45.789013 47.44724 49.938683 51.94538 52.85141 55.970665
           56.692596 58.076157 58.637978 59.25864 60.241024 60.45804 62.359337 63.028862
           64.67988 65.56491 66.481186 67.985344 69.6926 70.359344 71.45804)
          (1.1376657 24.941277 27.673742 28.741362 31.058517 31.941284 38.945374
           40.792686 41.938683 47.44724 48.368874 52.85141 52.945374 53.792694 56.692596
           57.37822 57.45061 59.25864 59.85141 59.91418 60.241024 62.359337 62.507217
           64.67988 65.115265 67.30328 68.74049 70.37822 70.6926 72.07837)
          (21.176462 24.176456 26.058517 27.789013 34.741364 35.789017 38.945374
           42.93868 44.24706 46.789017 48.970665 50.938675 53.458046 53.792694 56.692596
           56.97067 58.786133 59.679886 59.91418 60.985336 61.458046 63.359344 63.564903
           65.25864 66.5649 67.30328 68.98534 70.37822 71.359344 72.45804)
          (21.673754 24.176456 25.673744 25.941284 35.789017 35.938675 36.941284
           42.93868 43.556114 46.789017 48.447235 50.938675 53.851402 54.55612 56.359344
           56.97067 59.076153 59.115273 59.637978 61.24102 61.458046 63.11528 63.359344
           64.85141 66.5649 67.4812 68.98534 70.07616 71.359344 72.45804)
          (20.176455 21.673754 25.941284 28.673738 32.94128 35.938675 36.941284
           41.792694 42.93868 48.447235 49.368866 53.851402 53.945377 56.24706 56.359344
           58.37822 58.450607 59.115273 60.851406 60.914177 61.24102 62.564896 63.359344
           64.85141 66.115265 68.3033 69.74049 70.78613 71.69259 73.07838)
          (13.644882 23.741364 26.058517 28.673738 32.94128 34.741364 36.938675
           40.945374 42.93868 47.874855 49.368866 53.945377 54.258644 55.556114
           57.359337 58.450607 58.692593 59.679886 60.637974 60.851406 61.50785
           63.028862 63.359344 65.25864 66.115265 68.48119 69.74049 71.07616 71.69259
           73.07838)
          (13.644882 20.058517 23.176455 28.741362 31.058517 34.789017 36.938675
           40.945374 41.938683 46.247063 50.7384 53.67988 53.945377 55.556114 57.359337
           58.692593 58.891724 59.25864 60.45804 60.637974 60.78613 62.359337 63.028862
           64.67988 65.56491 68.48119 69.50617 71.07616 71.69259 72.71221)
          (16.64487 20.058517 21.176462 28.741362 31.058517 36.938675 37.368866
           41.938683 42.792694 46.247063 52.938675 53.67988 54.94538 57.247055 57.359337
           59.25864 59.378212 60.298016 60.78613 61.078377 61.914185 62.359337 63.564903
           64.67988 66.02887 69.30329 70.28204 71.78613 72.6926 73.359344)
          (21.176462 23.741364 24.176456 26.058517 34.741364 35.789017 36.938675
           42.792694 42.93868 47.874855 51.738396 54.258644 54.94538 57.247055 57.359337
           59.378212 59.679886 59.891727 61.458046 61.50785 61.914185 63.359344
           63.564903 65.25864 66.5649 69.30329 70.506165 71.78613 72.6926 73.712204)
          (21.176462 23.741364 26.058517 34.05852 35.789017 36.938675 40.789017
           42.792694 46.94538 48.78901 50.97067 54.258644 54.94538 57.247055 57.359337
           59.378212 59.679886 59.891727 61.458046 61.914185 62.985336 63.564903 64.6926
           66.45805 67.679886 69.30329 70.506165 71.78613 72.6926 74.45804)
          (15.644875 25.176458 26.941286 30.74136 33.058514 36.78901 40.945374 43.938675
           44.556114 47.789017 49.447243 51.93868 53.945377 54.85141 57.970673 58.692593
           60.07616 60.637974 61.258644 62.241013 62.45804 64.359344 65.02887 66.679886
           67.564896 68.48119 69.98534 71.69259 72.359344 73.458046)
          (21.05852 22.176458 29.741364 30.673748 37.938683 38.368866 38.941284
           43.792694 45.941284 46.94538 50.44724 51.93868 54.679886 54.792694 58.35934
           58.450607 60.258636 60.34045 60.378216 62.07837 63.241028 64.564896 64.6926
           66.85142 68.115265 68.66771 69.74049 71.37822 72.359344 73.8514)
          (21.05852 26.176472 27.673742 29.741364 37.938683 38.941284 40.738403
           45.556107 46.94538 48.78901 50.44724 52.938675 54.679886 56.556114 58.35934
           59.891727 60.258636 61.076157 61.63797 62.712204 63.241028 64.6926 65.115265
           66.85142 68.56491 69.4812 70.506165 72.07616 73.359344 74.45804)
          (16.64487 26.176472 27.941282 31.741364 34.05852 37.789017 41.945377 44.938683
           45.556107 48.78901 50.44724 52.938675 54.94538 55.851414 58.970665 59.69259
           61.076157 61.63797 62.258636 63.241028 63.45804 65.35934 66.02887 67.679886
           68.56491 69.4812 70.98534 72.6926 73.359344 74.45804)
          (4.1376457 27.941282 30.673748 31.741364 34.05852 34.941277 41.945377
           43.792694 44.938683 50.44724 51.368866 55.851414 55.94538 56.79269 59.69259
           60.378216 60.45061 62.258636 62.85141 62.914185 63.241028 65.35934 65.50722
           67.679886 68.115265 70.30328 71.74049 73.37822 73.6926 75.07837)
          (24.176456 27.176462 29.058525 30.78901 37.741356 38.78901 41.945377 45.93868
           47.24706 49.789017 51.970665 53.938683 56.458042 56.79269 59.69259 59.97067
           61.786125 62.67987 62.914185 63.98535 64.458046 66.359344 66.5649 68.25864
           69.5649 70.30328 71.98534 73.37822 74.35934 75.45804)
          (24.673748 27.176462 28.673738 28.941278 38.78901 38.938675 39.941284 45.93868
           46.55611 49.789017 51.447243 53.938683 56.85141 57.556107 59.359344 59.97067
           62.076157 62.115265 62.63797 64.24102 64.458046 66.115265 66.359344 67.85141
           69.5649 70.4812 71.98534 73.07616 74.35934 75.45804)
          (23.176455 24.673748 28.941278 31.673752 35.941284 38.938675 39.941284
           44.79269 45.93868 51.447243 52.368862 56.85141 56.945377 59.24706 59.359344
           61.37822 61.450607 62.115265 63.85141 63.914185 64.24102 65.56491 66.359344
           67.85141 69.115265 71.3033 72.74049 73.786125 74.69259 76.07837)
          (16.64487 26.741358 29.058525 31.673752 35.941284 37.741356 39.93868 43.94538
           45.93868 50.87486 52.368862 56.945377 57.258636 58.55611 60.359344 61.450607
           61.692596 62.67987 63.63797 63.85141 64.50785 66.02887 66.359344 68.25864
           69.115265 71.48119 72.74049 74.07616 74.69259 76.07837)
          (16.64487 23.058517 26.176472 31.741364 34.05852 37.789017 39.93868 43.94538
           44.938683 49.247063 53.7384 56.679886 56.945377 58.55611 60.359344 61.692596
           61.891724 62.258636 63.45804 63.63797 63.786133 65.35934 66.02887 67.679886
           68.56491 71.48119 72.50617 74.07616 74.69259 75.71221)
          (16.64487 23.058517 26.176472 31.741364 34.05852 37.789017 39.93868 43.94538
           44.938683 49.247063 53.7384 56.679886 56.945377 58.55611 60.359344 61.692596
           61.891724 62.258636 63.45804 63.63797 63.786133 65.35934 66.02887 67.679886
           68.56491 71.48119 72.50617 74.07616 74.69259 75.71221)
          (16.64487 25.673744 29.941292 33.74136 36.058517 39.93868 40.941284 43.94538
           46.938683 50.78901 52.447235 56.945377 57.851418 58.55611 60.359344 60.970665
           61.692596 63.11528 63.63797 64.25864 65.24101 66.02887 67.359344 68.85141
           69.679886 71.48119 72.98534 74.07616 74.69259 76.458046)
          (16.64487 20.741364 25.673744 29.941292 33.74136 37.94538 39.93868 43.94538
           44.874863 47.970665 51.258644 55.692593 56.945377 57.851418 58.50785 58.55611
           59.98534 60.359344 60.970665 61.692596 63.11528 63.63797 64.25864 65.03319
           66.02887 70.532 71.48119 72.98534 74.07616 74.69259)
          (17.058523 20.176455 31.789017 37.94538 38.938675 43.247055 47.7384 50.67988
           50.945374 55.692593 55.89172 57.45805 57.63797 57.786125 59.359344 62.564896
           65.48119 66.50617 68.6926 69.71221)
          (10.644891 17.058523 20.176455 25.741362 28.058521 31.789017 33.93868 37.94538
           38.938675 43.247055 47.7384 50.67988 50.945374 52.556114 54.359337 55.692593
           55.89172 56.25864 57.45805 57.63797 57.786125 59.359344 60.028862 61.67988
           62.564896 65.48119 66.50617 68.07616 68.6926 69.71221)
          (13.644882 17.058523 18.176456 25.741362 28.058521 33.93868 34.368866
           38.938675 39.792694 43.247055 49.938683 50.67988 51.94538 54.247063 54.359337
           56.25864 56.378212 57.29802 57.786125 58.078377 58.914185 59.359344 60.564903
           61.67988 63.028862 66.30329 67.28204 68.78612 69.6926 70.359344)
          (18.176456 20.741364 21.176462 23.058517 31.741364 32.78901 33.93868 39.792694
           39.93868 44.874863 48.738403 51.258644 51.94538 54.247063 54.359337 56.378212
           56.679886 56.891727 58.458046 58.50785 58.914185 60.359344 60.564903
           62.258636 63.564903 66.30329 67.506165 68.78612 69.6926 70.712204)
          (18.176456 20.741364 23.058517 31.058517 32.78901 33.93868 37.789017 39.792694
           43.94538 45.789013 47.970665 51.258644 51.94538 54.247063 54.359337 56.378212
           56.679886 56.891727 58.458046 58.914185 59.98534 60.564903 61.692596 63.45804
           64.67988 66.30329 67.506165 68.78612 69.6926 71.45804)
          (12.644875 22.176458 23.941286 27.741365 30.058514 33.789013 37.94538
           40.938675 41.556114 44.789017 46.447243 48.938675 50.945374 51.85141
           54.970665 55.692593 57.076157 57.63797 58.258644 59.24102 59.458042 61.359344
           62.02887 63.679886 64.564896 65.48119 66.98534 68.6926 69.359344 70.458046)
          (18.058514 19.176464 26.741358 27.673742 34.938683 35.368866 35.941284
           40.792686 42.941288 43.94538 47.44724 48.938675 51.679886 51.792694 55.359344
           55.450615 57.258636 57.340446 57.37822 59.078373 60.241024 61.564896
           61.692596 63.85141 65.115265 65.66771 66.7405 68.37822 69.359344 70.85141)
          (18.058514 23.176455 24.673748 26.741358 34.938683 35.941284 37.738403
           42.55612 43.94538 45.789013 47.44724 49.938683 51.679886 53.556114 55.359344
           56.891727 57.258636 58.076157 58.637978 59.712215 60.241024 61.692596
           62.115265 63.85141 65.56491 66.481186 67.506165 69.07616 70.359344 71.45804)
          (13.644882 23.176455 24.941277 28.741362 31.058517 34.789017 38.945374
           41.938683 42.55612 45.789013 47.44724 49.938683 51.94538 52.85141 55.970665
           56.692596 58.076157 58.637978 59.25864 60.241024 60.45804 62.359337 63.028862
           64.67988 65.56491 66.481186 67.985344 69.6926 70.359344 71.45804)
          (1.1376657 24.941277 27.673742 28.741362 31.058517 31.941284 38.945374
           40.792686 41.938683 47.44724 48.368874 52.85141 52.945374 53.792694 56.692596
           57.37822 57.45061 59.25864 59.85141 59.91418 60.241024 62.359337 62.507217
           64.67988 65.115265 67.30328 68.74049 70.37822 70.6926 72.07837)
          (21.176462 24.176456 26.058517 27.789013 34.741364 35.789017 38.945374
           42.93868 44.24706 46.789017 48.970665 50.938675 53.458046 53.792694 56.692596
           56.97067 58.786133 59.679886 59.91418 60.985336 61.458046 63.359344 63.564903
           65.25864 66.5649 67.30328 68.98534 70.37822 71.359344 72.45804)
          (21.673754 24.176456 25.673744 25.941284 35.789017 35.938675 36.941284
           42.93868 43.556114 46.789017 48.447235 50.938675 53.851402 54.55612 56.359344
           56.97067 59.076153 59.115273 59.637978 61.24102 61.458046 63.11528 63.359344
           64.85141 66.5649 67.4812 68.98534 70.07616 71.359344 72.45804)
          (20.176455 21.673754 25.941284 28.673738 32.94128 35.938675 36.941284
           41.792694 42.93868 48.447235 49.368866 53.851402 53.945377 56.24706 56.359344
           58.37822 58.450607 59.115273 60.851406 60.914177 61.24102 62.564896 63.359344
           64.85141 66.115265 68.3033 69.74049 70.78613 71.69259 73.07838)
          (13.644882 23.741364 26.058517 28.673738 32.94128 34.741364 36.938675
           40.945374 42.93868 47.874855 49.368866 53.945377 54.258644 55.556114
           57.359337 58.450607 58.692593 59.679886 60.637974 60.851406 61.50785
           63.028862 63.359344 65.25864 66.115265 68.48119 69.74049 71.07616 71.69259
           73.07838)
          (13.644882 20.058517 23.176455 28.741362 31.058517 34.789017 36.938675
           40.945374 41.938683 46.247063 50.7384 53.67988 53.945377 55.556114 57.359337
           58.692593 58.891724 59.25864 60.45804 60.637974 60.78613 62.359337 63.028862
           64.67988 65.56491 68.48119 69.50617 71.07616 71.69259 72.71221)
          (13.644882 22.673738 26.941286 30.74136 33.058514 36.938675 37.941277
           40.945374 43.938675 47.789017 49.447243 53.945377 54.85141 55.556114
           57.359337 57.970673 58.692593 60.115276 60.637974 61.258644 62.241013
           63.028862 64.359344 65.85141 66.679886 68.48119 69.98534 71.07616 71.69259
           73.458046)
          (13.644882 17.741364 22.673738 26.941286 30.74136 34.94538 36.938675 40.945374
           41.87486 44.97067 48.258636 52.692596 53.945377 54.85141 55.507843 55.556114
           56.98534 57.359337 57.970673 58.692593 60.115276 60.637974 61.258644 62.03318
           63.028862 67.532 68.48119 69.98534 71.07616 71.69259)
          (14.058517 17.17646 28.789015 34.94538 35.938675 40.247055 44.738403 47.679886
           47.945377 52.692596 52.891724 54.458046 54.637974 54.78613 56.359344
           59.564903 62.481194 63.506172 65.69259 66.71221)
          (7.644891 14.058517 17.17646 22.741362 25.058521 28.789015 30.938679 34.94538
           35.938675 40.247055 44.738403 47.679886 47.945377 49.55611 51.359344
           52.692596 52.891724 53.258636 54.458046 54.637974 54.78613 56.359344 57.02887
           58.67989 59.564903 62.481194 63.506172 65.07616 65.69259 66.71221)
          (10.644891 14.058517 15.176462 22.741362 25.058521 30.938679 31.368862
           35.938675 36.792694 40.247055 46.938683 47.679886 48.94538 51.247063
           51.359344 53.258636 53.37822 54.29802 54.78613 55.07837 55.914185 56.359344
           57.564903 58.67989 60.028862 63.30329 64.28204 65.78612 66.6926 67.359344)
          (15.176462 17.741364 18.176456 20.058517 28.741362 29.789017 30.938679
           36.792694 36.938675 41.87486 45.738403 48.258636 48.94538 51.247063 51.359344
           53.37822 53.67988 53.891727 55.458046 55.507843 55.914185 57.359337 57.564903
           59.25864 60.564903 63.30329 64.506165 65.78612 66.6926 67.712204)
          (11.64489 17.741364 20.058517 20.67374 28.741362 30.938679 32.368866 36.938675
           38.556107 41.87486 43.368862 45.93868 48.258636 49.55611 51.359344 52.450615
           53.67988 54.076157 54.637974 55.507843 56.078373 57.359337 58.115265 59.25864
           61.028854 62.481194 63.740494 65.07616 66.359344 67.07838)
          (11.64489 15.058514 16.176453 23.741364 26.058517 31.938675 32.368866
           36.938675 37.792686 41.247055 43.368862 45.93868 48.679882 48.792694 52.35934
           52.450615 54.258644 54.340446 54.378216 55.786133 56.078373 57.359337 58.5649
           59.679886 61.028854 62.667694 63.740494 65.37822 66.359344 67.07838)
          (16.176453 18.74136 19.176464 21.05852 29.741364 30.78901 31.938675 37.792686
           37.938683 41.789017 42.874855 45.93868 48.792694 49.258644 51.970665 52.35934
           54.340446 54.378216 54.679886 56.458042 56.50785 58.35934 58.5649 60.258636
           61.564896 62.667694 63.98535 65.37822 66.359344 67.458046)
          (16.176453 18.74136 19.176464 21.05852 29.741364 30.78901 31.938675 37.792686
           37.938683 42.874855 46.738396 49.258644 49.94538 52.247055 52.35934 54.378216
           54.679886 54.89173 56.458042 56.50785 56.91418 58.35934 58.5649 60.258636
           61.564896 64.30328 65.506165 66.78613 67.6926 68.71222)
          (9.644863 19.176464 20.941286 24.741365 27.058514 30.78901 34.94538 37.938683
           38.556107 43.447235 46.738396 48.851406 49.94538 51.556114 52.692596
           54.076157 54.89173 55.258636 56.24102 56.458042 57.170906 58.35934 59.028866
           60.679882 61.564896 64.133675 65.506165 67.07616 67.6926 68.71222)
          (9.644863 16.058521 19.176464 24.741365 27.058514 30.78901 32.938683 36.94538
           37.938683 42.247063 46.738396 49.67988 49.94538 51.556114 53.359344 54.69259
           54.89173 55.258636 56.458042 56.63797 56.786125 58.35934 59.028866 60.679882
           61.564896 64.4812 65.506165 67.07616 67.6926 68.71222)
          (9.644863 18.673748 22.941284 26.741358 29.058525 32.938683 33.941284 36.94538
           39.93868 43.78902 45.447243 49.94538 50.851418 51.556114 53.359344 53.97067
           54.69259 56.11527 56.63797 57.258636 58.241028 59.028866 60.359344 61.851402
           62.67987 64.4812 65.98534 67.07616 67.6926 69.45805)
          (9.644863 13.741362 18.673748 22.941284 26.741358 30.945381 32.938683 36.94538
           37.874855 40.97066 44.25864 48.692596 49.94538 50.851418 51.50785 51.556114
           52.985336 53.359344 53.97067 54.69259 56.11527 56.63797 57.258636 58.033184
           59.028866 63.531998 64.4812 65.98534 67.07616 67.6926)
          (10.058521 13.176458 24.789013 30.945381 31.938675 36.247055 40.738403
           43.679886 43.94538 48.692596 48.89173 50.458046 50.637966 50.786133 52.35934
           55.5649 58.481194 59.506172 61.692596 62.712204)
          (3.6448746 10.058521 13.176458 18.74136 21.05852 24.789013 26.938677 30.945381
           31.938675 36.247055 40.738403 43.679886 43.94538 45.556107 47.359344
           48.692596 48.89173 49.258644 50.458046 50.637966 50.786133 52.35934 53.028862
           54.679886 55.5649 58.481194 59.506172 61.076157 61.692596 62.712204)
          (6.644886 10.058521 11.176452 18.74136 21.05852 26.938677 27.368866 31.938675
           32.792694 36.247055 42.93868 43.679886 44.945377 47.24706 47.359344 49.258644
           49.378223 50.29802 50.786133 51.078377 51.914185 52.35934 53.564903 54.679886
           56.028866 59.303295 60.28205 61.786125 62.69259 63.359344)
          (11.176452 13.741362 14.176472 16.058521 24.741365 25.78901 26.938677
           32.792694 32.938683 37.874855 41.7384 44.25864 44.945377 47.24706 47.359344
           49.378223 49.67988 49.891724 51.458046 51.50785 51.914185 53.359344 53.564903
           55.258636 56.564907 59.303295 60.506172 61.786125 62.69259 63.71221)
          (11.176452 13.741362 16.058521 24.05852 25.78901 26.938677 30.78901 32.792694
           36.94538 38.78901 40.97066 44.25864 44.945377 47.24706 47.359344 49.378223
           49.67988 49.891724 51.458046 51.914185 52.985336 53.564903 54.69259 56.458042
           57.679882 59.303295 60.506172 61.786125 62.69259 64.458046)
          (5.6448784 15.176462 16.941278 20.741364 23.058517 26.789011 30.945381
           33.93868 34.55611 37.789017 39.447243 41.938683 43.94538 44.85141 47.970665
           48.692596 50.076153 50.637966 51.258644 52.24102 52.45804 54.359337 55.028862
           56.679886 57.564903 58.481194 59.98534 61.692596 62.359337 63.45804)
          (11.058517 12.176456 19.741362 20.67374 27.938679 28.368862 28.941278
           33.792698 35.941284 36.94538 40.447235 41.938683 44.679886 44.79269 48.359344
           48.45061 50.25864 50.340443 50.37822 52.07837 53.24102 54.564903 54.69259
           56.85141 58.115265 58.6677 59.740494 61.37822 62.359337 63.85141)
          (11.058517 16.176453 17.673746 19.741362 27.938679 28.941278 30.738403
           35.556118 36.94538 38.78901 40.447235 42.93868 44.679886 46.55611 48.359344
           49.891724 50.25864 51.07615 51.637974 52.712208 53.24102 54.69259 55.115273
           56.85141 58.5649 59.481194 60.506172 62.076157 63.359344 64.458046)
          (6.644886 16.176453 17.941292 21.74136 24.05852 27.789013 31.945381 34.938683
           35.556118 38.78901 40.447235 42.93868 44.945377 45.851418 48.970665 49.692593
           51.07615 51.637974 52.258636 53.24102 53.458046 55.359344 56.028866 57.679882
           58.5649 59.481194 60.985336 62.69259 63.359344 64.458046)
          (-5.862336 17.941292 20.67374 21.74136 24.05852 24.941277 31.945381 33.792698
           34.938683 40.447235 41.36887 45.851418 45.945374 46.792694 49.692593 50.37822
           50.450607 52.258636 52.85141 52.914177 53.24102 55.359344 55.50722 57.679882
           58.115265 60.30329 61.740486 63.37822 63.69258 65.07837)
          (14.176472 17.17646 19.058516 20.789011 27.741365 28.789015 31.945381
           35.938675 37.247063 39.789013 41.97067 43.938675 46.458046 46.792694
           49.692593 49.970665 51.78613 52.679886 52.914177 53.98534 54.458046 56.359344
           56.564907 58.258644 59.564903 60.30329 61.985344 63.37822 64.359344 65.45805)
          (14.673746 17.17646 18.673748 18.941282 28.789015 28.93868 29.941292 35.938675
           36.556114 39.789013 41.447243 43.938675 46.851414 47.556118 49.35934
           49.970665 52.076157 52.115273 52.63797 54.241024 54.458046 56.11527 56.359344
           57.851418 59.564903 60.4812 61.985344 63.07615 64.359344 65.45805)
          (13.176458 14.673746 18.941282 21.673754 25.941284 28.93868 29.941292
           34.792694 35.938675 41.447243 42.368866 46.851414 46.94538 49.247063 49.35934
           51.378216 51.450615 52.115273 53.851402 53.914185 54.241024 55.5649 56.359344
           57.851418 59.115273 61.303284 62.740494 63.786133 64.6926 66.07837)
          (6.644886 16.741362 19.058516 21.673754 25.941284 27.741365 29.938675
           33.945374 35.938675 40.874863 42.368866 46.94538 47.25864 48.556114 50.359337
           51.450615 51.69259 52.679886 53.63797 53.851402 54.50785 56.028866 56.359344
           58.258644 59.115273 61.4812 62.740494 64.07616 64.6926 66.07837)
          (6.644886 13.058521 16.176453 21.74136 24.05852 27.789013 29.938675 33.945374
           34.938683 39.247063 43.738403 46.67989 46.94538 48.556114 50.359337 51.69259
           51.89173 52.258636 53.458046 53.63797 53.786125 55.359344 56.028866 57.679882
           58.5649 61.4812 62.50618 64.07616 64.6926 65.712204)
          (-4.8623343 15.741365 24.941277 26.058517 27.789013 28.741362 32.945377
           34.792694 38.945374 40.789017 42.970665 46.258644 46.94538 47.79269 50.692596
           51.378216 51.89173 52.85141 53.458046 53.914185 54.985336 56.507217 56.692596
           59.25864 59.679886 61.303284 62.50618 64.37821 64.6926 66.45805)
          (7.644891 15.741365 22.673738 24.941277 28.741362 30.368866 32.945377
           36.556114 38.945374 42.970665 43.368862 46.258644 47.945377 49.55611
           50.692596 52.076157 52.85141 53.29802 54.078377 54.985336 55.170914 56.692596
           57.02887 59.25864 60.115276 62.133667 63.28205 65.07616 65.69259 67.07838)
          (7.644891 14.058517 17.17646 22.741362 25.058521 28.789015 30.938679 34.94538
           35.938675 40.247055 44.738403 47.679886 47.945377 49.55611 51.359344
           52.692596 52.891724 53.258636 54.458046 54.637974 54.78613 56.359344 57.02887
           58.67989 59.564903 62.481194 63.506172 65.07616 65.69259 66.71221)
          (10.644891 14.058517 15.176462 22.741362 25.058521 30.938679 31.368862
           35.938675 36.792694 40.247055 46.938683 47.679886 48.94538 51.247063
           51.359344 53.258636 53.37822 54.29802 54.78613 55.07837 55.914185 56.359344
           57.564903 58.67989 60.028862 63.30329 64.28204 65.78612 66.6926 67.359344)
          (15.176462 17.741364 18.176456 20.058517 28.741362 29.789017 30.938679
           36.792694 36.938675 41.87486 45.738403 48.258636 48.94538 51.247063 51.359344
           53.37822 53.67988 53.891727 55.458046 55.507843 55.914185 57.359337 57.564903
           59.25864 60.564903 63.30329 64.506165 65.78612 66.6926 67.712204)
          (15.176462 17.741364 20.058517 28.058521 29.789017 30.938679 34.789017
           36.792694 40.945374 42.789013 44.97067 48.258636 48.94538 51.247063 51.359344
           53.37822 53.67988 53.891727 55.458046 55.914185 56.98534 57.564903 58.692593
           60.45804 61.67988 63.30329 64.506165 65.78612 66.6926 68.45804)
          (9.644863 19.176464 20.941286 24.741365 27.058514 30.78901 34.94538 37.938683
           38.556107 41.789017 43.447235 45.93868 47.945377 48.851406 51.970665
           52.692596 54.076157 54.637974 55.258636 56.24102 56.458042 58.35934 59.028866
           60.679882 61.564896 62.481194 63.98535 65.69259 66.359344 67.458046)
          (15.058514 16.176453 23.741364 24.673748 31.938675 32.368866 32.94128
           37.792686 39.941284 40.945374 44.447235 45.93868 48.679882 48.792694 52.35934
           52.450615 54.258644 54.340446 54.378216 56.078373 57.24102 58.5649 58.692593
           60.851406 62.115265 62.667694 63.740494 65.37822 66.359344 67.85141)
          (15.058514 20.176455 21.673754 23.741364 31.938675 32.94128 34.738396
           39.556114 40.945374 42.789013 44.447235 46.938683 48.679882 50.556107
           52.35934 53.891727 54.258644 55.076164 55.637978 56.71221 57.24102 58.692593
           59.115273 60.851406 62.564896 63.481186 64.506165 66.07615 67.359344
           68.45804)
          (10.644891 20.176455 21.941282 25.741362 28.058521 31.789017 35.945377
           38.938675 39.556114 42.789013 44.447235 46.938683 48.94538 49.85141 52.97066
           53.692596 55.076164 55.637978 56.25864 57.24102 57.45805 59.359344 60.028862
           61.67988 62.564896 63.481186 64.985344 66.6926 67.359344 68.45804)
          (-1.8623571 21.941282 24.673748 25.741362 28.058521 28.941278 35.945377
           37.792686 38.938675 44.447235 45.368866 49.85141 49.94538 50.792694 53.692596
           54.378216 54.450607 56.25864 56.85141 56.91418 57.24102 59.359344 59.507217
           61.67988 62.115265 64.30328 65.74049 67.37821 67.6926 69.078384)
          (18.176456 21.176462 23.058517 24.789013 31.741364 32.78901 35.945377 39.93868
           41.247055 43.78902 45.970673 47.938675 50.458046 50.792694 53.692596 53.97067
           55.786133 56.679886 56.91418 57.985336 58.458046 60.359344 60.564903
           62.258636 63.564903 64.30328 65.98534 67.37821 68.359344 69.45805)
          (18.673748 21.176462 22.673738 22.941284 32.78901 32.938683 33.941284 39.93868
           40.556114 43.78902 45.447243 47.938675 50.851418 51.556114 53.359344 53.97067
           56.076157 56.11527 56.63797 58.241028 58.458046 60.115276 60.359344 61.851402
           63.564903 64.4812 65.98534 67.07616 68.359344 69.45805)
          (17.17646 18.673748 22.941284 25.673744 29.941292 32.938683 33.941284
           38.792694 39.93868 45.447243 46.368866 50.851418 50.945374 53.247055
           53.359344 55.37822 55.450615 56.11527 57.851418 57.914177 58.241028 59.564903
           60.359344 61.851402 63.11528 65.30328 66.7405 67.786125 68.6926 70.07838)
          (10.644891 20.741364 23.058517 25.673744 29.941292 31.741364 33.93868 37.94538
           39.93868 44.874863 46.368866 50.945374 51.258644 52.556114 54.359337
           55.450615 55.692593 56.679886 57.63797 57.851418 58.50785 60.028862 60.359344
           62.258636 63.11528 65.48119 66.7405 68.07616 68.6926 70.07838)
          (10.644891 17.058523 20.176455 25.741362 28.058521 31.789017 33.93868 37.94538
           38.938675 43.247055 47.7384 50.67988 50.945374 52.556114 54.359337 55.692593
           55.89172 56.25864 57.45805 57.63797 57.786125 59.359344 60.028862 61.67988
           62.564896 65.48119 66.50617 68.07616 68.6926 69.71221)
          (13.644882 17.058523 18.176456 25.741362 28.058521 33.93868 34.368866
           38.938675 39.792694 43.247055 49.938683 50.67988 51.94538 54.247063 54.359337
           56.25864 56.378212 57.29802 57.786125 58.078377 58.914185 59.359344 60.564903
           61.67988 63.028862 66.30329 67.28204 68.78612 69.6926 70.359344)
          (18.176456 20.741364 21.176462 23.058517 31.741364 32.78901 33.93868 39.792694
           39.93868 44.874863 48.738403 51.258644 51.94538 54.247063 54.359337 56.378212
           56.679886 56.891727 58.458046 58.50785 58.914185 60.359344 60.564903
           62.258636 63.564903 66.30329 67.506165 68.78612 69.6926 70.712204)
          (18.176456 20.741364 23.058517 31.058517 32.78901 33.93868 37.789017 39.792694
           43.94538 45.789013 47.970665 51.258644 51.94538 54.247063 54.359337 56.378212
           56.679886 56.891727 58.458046 58.914185 59.98534 60.564903 61.692596 63.45804
           64.67988 66.30329 67.506165 68.78612 69.6926 71.45804)
          (12.644875 22.176458 23.941286 27.741365 30.058514 33.789013 37.94538
           40.938675 41.556114 44.789017 46.447243 48.938675 50.945374 51.85141
           54.970665 55.692593 57.076157 57.63797 58.258644 59.24102 59.458042 61.359344
           62.02887 63.679886 64.564896 65.48119 66.98534 68.6926 69.359344 70.458046)
          (18.058514 19.176464 26.741358 27.673742 34.938683 35.368866 35.941284
           40.792686 42.941288 43.94538 47.44724 48.938675 51.679886 51.792694 55.359344
           55.450615 57.258636 57.340446 57.37822 59.078373 60.241024 61.564896
           61.692596 63.85141 65.115265 65.66771 66.7405 68.37822 69.359344 70.85141)
          (18.058514 23.176455 24.673748 26.741358 34.938683 35.941284 37.738403
           42.55612 43.94538 45.789013 47.44724 49.938683 51.679886 53.556114 55.359344
           56.891727 57.258636 58.076157 58.637978 59.712215 60.241024 61.692596
           62.115265 63.85141 65.56491 66.481186 67.506165 69.07616 70.359344 71.45804)
          (13.644882 23.176455 24.941277 28.741362 31.058517 34.789017 38.945374
           41.938683 42.55612 45.789013 47.44724 49.938683 51.94538 52.85141 55.970665
           56.692596 58.076157 58.637978 59.25864 60.241024 60.45804 62.359337 63.028862
           64.67988 65.56491 66.481186 67.985344 69.6926 70.359344 71.45804)
          (1.1376657 24.941277 27.673742 28.741362 31.058517 31.941284 38.945374
           40.792686 41.938683 47.44724 48.368874 52.85141 52.945374 53.792694 56.692596
           57.37822 57.45061 59.25864 59.85141 59.91418 60.241024 62.359337 62.507217
           64.67988 65.115265 67.30328 68.74049 70.37822 70.6926 72.07837)
          (21.176462 24.176456 26.058517 27.789013 34.741364 35.789017 38.945374
           42.93868 44.24706 46.789017 48.970665 50.938675 53.458046 53.792694 56.692596
           56.97067 58.786133 59.679886 59.91418 60.985336 61.458046 63.359344 63.564903
           65.25864 66.5649 67.30328 68.98534 70.37822 71.359344 72.45804)
          (21.673754 24.176456 25.673744 25.941284 35.789017 35.938675 36.941284
           42.93868 43.556114 46.789017 48.447235 50.938675 53.851402 54.55612 56.359344
           56.97067 59.076153 59.115273 59.637978 61.24102 61.458046 63.11528 63.359344
           64.85141 66.5649 67.4812 68.98534 70.07616 71.359344 72.45804)
          (20.176455 21.673754 25.941284 28.673738 32.94128 35.938675 36.941284
           41.792694 42.93868 48.447235 49.368866 53.851402 53.945377 56.24706 56.359344
           58.37822 58.450607 59.115273 60.851406 60.914177 61.24102 62.564896 63.359344
           64.85141 66.115265 68.3033 69.74049 70.78613 71.69259 73.07838)
          (13.644882 23.741364 26.058517 28.673738 32.94128 34.741364 36.938675
           40.945374 42.93868 47.874855 49.368866 53.945377 54.258644 55.556114
           57.359337 58.450607 58.692593 59.679886 60.637974 60.851406 61.50785
           63.028862 63.359344 65.25864 66.115265 68.48119 69.74049 71.07616 71.69259
           73.07838)
          (13.644882 20.058517 23.176455 28.741362 31.058517 34.789017 36.938675
           40.945374 41.938683 46.247063 50.7384 53.67988 53.945377 55.556114 57.359337
           58.692593 58.891724 59.25864 60.45804 60.637974 60.78613 62.359337 63.028862
           64.67988 65.56491 68.48119 69.50617 71.07616 71.69259 72.71221)
          (2.1376677 22.741362 31.941284 33.058514 34.789017 35.741364 39.94538
           41.792694 45.945374 47.789017 49.970665 53.258636 53.945377 54.792694
           57.692596 58.37822 58.891724 59.85141 60.45804 60.914177 61.985344 63.507225
           63.69258 66.25865 66.679886 68.3033 69.50617 71.37822 71.69259 73.458046)
          (14.644884 22.741362 29.673744 31.941284 35.741364 37.368866 39.94538
           43.556114 45.945374 49.970665 50.368866 53.258636 54.94538 56.556114
           57.692596 59.076153 59.85141 60.298016 61.078377 61.985344 62.170914 63.69258
           64.02887 66.25865 67.11527 69.13368 70.28204 72.07616 72.6926 74.07837)
          (14.644884 21.05852 24.176456 29.741364 32.058517 35.789017 37.938683
           41.945377 42.93868 47.24706 51.738396 54.679886 54.94538 56.556114 58.35934
           59.69259 59.891727 60.258636 61.458046 61.63797 61.786125 63.359344 64.02887
           65.679886 66.5649 69.4812 70.506165 72.07616 72.6926 73.712204)
          (17.644878 21.05852 22.176458 29.741364 32.058517 37.938683 38.368866 42.93868
           43.792694 47.24706 53.938683 54.679886 55.94538 58.247063 58.35934 60.258636
           60.378216 61.29802 61.786125 62.07837 62.914185 63.359344 64.564896 65.679886
           67.02887 70.30328 71.28206 72.78613 73.6926 74.35934)
          (22.176458 24.741365 25.176458 27.058514 35.741364 36.78901 37.938683
           43.792694 43.938675 48.874855 52.738403 55.258636 55.94538 58.247063 58.35934
           60.378216 60.679882 60.891724 62.45804 62.507843 62.914185 64.359344
           64.564896 66.25865 67.564896 70.30328 71.50618 72.78613 73.6926 74.712204)
          (18.644886 24.741365 27.058514 27.673742 35.741364 37.938683 39.368866
           43.938675 45.556107 48.874855 50.368866 52.938675 55.258636 56.556114
           58.35934 59.450603 60.679882 61.076157 61.63797 62.507843 63.07837 64.359344
           65.115265 66.25865 68.02887 69.4812 70.74049 72.07616 73.359344 74.07837)
          (18.644886 22.058521 23.176455 30.74136 33.058514 38.938675 39.368866
           43.938675 44.79269 48.247055 50.368866 52.938675 55.679886 55.792694
           59.359344 59.450603 61.258644 61.340446 61.37822 62.786133 63.07837 64.359344
           65.56491 66.679886 68.02887 69.66771 70.74049 72.37822 73.359344 74.07837)
          (23.176455 25.741362 26.176472 28.058521 36.74136 37.789017 38.938675 44.79269
           44.938683 48.78901 49.874855 52.938675 55.792694 56.25864 58.970665 59.359344
           61.340446 61.37822 61.67988 63.45804 63.507843 65.35934 65.56491 67.258644
           68.56491 69.66771 70.98534 72.37822 73.359344 74.45804)
          (23.176455 25.741362 26.176472 28.058521 36.74136 37.789017 38.938675 44.79269
           44.938683 49.874855 53.7384 56.25864 56.945377 59.24706 59.359344 61.37822
           61.67988 61.891724 63.45804 63.507843 63.914185 65.35934 65.56491 67.258644
           68.56491 71.3033 72.50617 73.786125 74.69259 75.71221)
          (16.64487 26.176472 27.941282 31.741364 34.05852 37.789017 41.945377 44.938683
           45.556107 50.44724 53.7384 55.851414 56.945377 58.55611 59.69259 61.076157
           61.891724 62.258636 63.241028 63.45804 64.17091 65.35934 66.02887 67.679886
           68.56491 71.13367 72.50617 74.07616 74.69259 75.71221)
          (16.64487 23.058517 26.176472 31.741364 34.05852 37.789017 39.93868 43.94538
           44.938683 49.247063 53.7384 56.679886 56.945377 58.55611 60.359344 61.692596
           61.891724 62.258636 63.45804 63.63797 63.786133 65.35934 66.02887 67.679886
           68.56491 71.48119 72.50617 74.07616 74.69259 75.71221)
          (19.644894 23.058517 24.176456 31.741364 34.05852 39.93868 40.368862 44.938683
           45.792698 49.247063 55.938675 56.679886 57.945374 60.247055 60.359344
           62.258636 62.37822 63.29802 63.786133 64.07837 64.914185 65.35934 66.5649
           67.679886 69.02887 72.30329 73.28204 74.78613 75.69258 76.359344)
          (24.176456 26.741358 27.176462 29.058525 37.741356 38.78901 39.93868 45.792698
           45.93868 50.87486 54.738403 57.258636 57.945374 60.247055 60.359344 62.37822
           62.67987 62.891724 64.458046 64.50785 64.914185 66.359344 66.5649 68.25864
           69.5649 72.30329 73.506165 74.78613 75.69258 76.712204)
          (24.176456 26.741358 29.058525 37.05852 38.78901 39.93868 43.78902 45.792698
           49.94538 51.789013 53.97067 57.258636 57.945374 60.247055 60.359344 62.37822
           62.67987 62.891724 64.458046 64.914185 65.98534 66.5649 67.6926 69.45805
           70.679886 72.30329 73.506165 74.78613 75.69258 77.45805)
          (18.644886 28.176453 29.941292 33.74136 36.058517 39.789013 43.94538 46.938683
           47.556118 50.78901 52.447235 54.93868 56.945377 57.851418 60.970665 61.692596
           63.07615 63.63797 64.25864 65.24101 65.45805 67.359344 68.02887 69.679886
           70.564896 71.48119 72.98534 74.69259 75.359344 76.458046)
          (24.05852 25.176458 32.741364 33.673748 40.938675 41.36887 41.94129 46.792694
           48.941284 49.94538 53.447243 54.93868 57.679882 57.792698 61.359344 61.450607
           63.258636 63.340454 63.37822 65.07837 66.24103 67.564896 67.6926 69.85142
           71.115265 71.667694 72.74049 74.37822 75.359344 76.85141)
          (24.05852 29.17646 30.673748 32.741364 40.938675 41.94129 43.738403 48.556114
           49.94538 51.789013 53.447243 55.938675 57.679882 59.556118 61.359344
           62.891724 63.258636 64.07616 64.63797 65.712204 66.24103 67.6926 68.115265
           69.85142 71.564896 72.4812 73.506165 75.07615 76.359344 77.45805)
          (19.644894 29.17646 30.941284 34.741364 37.05852 40.789017 44.945377 47.938675
           48.556114 51.789013 53.447243 55.938675 57.945374 58.851414 61.970673
           62.69259 64.07616 64.63797 65.25864 66.24103 66.45805 68.359344 69.02887
           70.679886 71.564896 72.4812 73.985344 75.69258 76.359344 77.45805)
          (7.1376686 30.941284 33.673748 34.741364 37.05852 37.941277 44.945377
           46.792694 47.938675 53.447243 54.368866 58.851414 58.94538 59.79269 62.69259
           63.37822 63.450607 65.25864 65.85141 65.91419 66.24103 68.359344 68.50722
           70.679886 71.115265 73.30328 74.74049 76.37821 76.6926 78.07837)
          (27.176462 30.176456 32.058517 33.789013 40.741364 41.789017 44.945377
           48.938675 50.24706 52.789017 54.970665 56.938683 59.458042 59.79269 62.69259
           62.970673 64.786125 65.679886 65.91419 66.98534 67.458046 69.359344 69.5649
           71.25864 72.5649 73.30328 74.98534 76.37821 77.35934 78.45805)
          (27.673742 30.176456 31.673752 31.941284 41.789017 41.938683 42.941288
           48.938675 49.55611 52.789017 54.447243 56.938683 59.85141 60.556114 62.359337
           62.970673 65.07616 65.115265 65.63797 67.24102 67.458046 69.115265 69.359344
           70.85141 72.5649 73.4812 74.98534 76.07616 77.35934 78.45805)
          (26.176472 27.673742 31.941284 34.673737 38.941284 41.938683 42.941288
           47.79269 48.938675 54.447243 55.368862 59.85141 59.945377 62.247055 62.359337
           64.37821 64.45061 65.115265 66.85142 66.914185 67.24102 68.56491 69.359344
           70.85141 72.11528 74.3033 75.74049 76.786125 77.69259 79.07838)
          (19.644894 29.741364 32.058517 34.673737 38.941284 40.741364 42.93868 46.94538
           48.938675 53.87486 55.368862 59.945377 60.258636 61.556107 63.359344 64.45061
           64.6926 65.679886 66.63797 66.85142 67.50785 69.02887 69.359344 71.25864
           72.11528 74.48119 75.74049 77.07616 77.69259 79.07838)
          (19.644894 26.058517 29.17646 34.741364 37.05852 40.789017 42.93868 46.94538
           47.938675 52.247055 56.738403 59.679886 59.945377 61.556107 63.359344 64.6926
           64.89173 65.25864 66.45805 66.63797 66.78613 68.359344 69.02887 70.679886
           71.564896 74.48119 75.50617 77.07616 77.69259 78.71221)
          (8.137679 28.741362 37.941277 39.058517 40.789017 41.741356 45.945374 47.79269
           51.94538 53.789017 55.970665 59.25864 59.945377 60.792686 63.69258 64.37821
           64.89173 65.85141 66.45805 66.914185 67.985344 69.507225 69.6926 72.25864
           72.679886 74.3033 75.50617 77.37822 77.69259 79.458046)
          (20.644878 28.741362 35.673744 37.941277 41.741356 43.368862 45.945374
           49.55611 51.94538 55.970665 56.368866 59.25864 60.945374 62.556107 63.69258
           65.07616 65.85141 66.29802 67.07838 67.985344 68.17091 69.6926 70.02887
           72.25864 73.11527 75.13368 76.28204 78.07615 78.6926 80.07837)
          (20.644878 27.058514 30.176456 35.741364 38.058525 41.789017 43.938675
           47.945377 48.938675 53.247055 57.738403 60.679882 60.945374 62.556107
           64.359344 65.69259 65.89172 66.25865 67.458046 67.63797 67.786125 69.359344
           70.02887 71.679886 72.5649 75.481186 76.506165 78.07615 78.6926 79.712204)
          (20.644878 29.673744 33.941284 37.741356 40.058517 43.938675 44.94128
           47.945377 50.938675 54.789013 56.447235 60.945374 61.851402 62.556107
           64.359344 64.97066 65.69259 67.11527 67.63797 68.25864 69.24103 70.02887
           71.359344 72.85141 73.67988 75.481186 76.985344 78.07615 78.6926 80.45804)
          (20.644878 24.741365 29.673744 33.941284 37.741356 41.945377 43.938675
           47.945377 48.874855 51.970665 55.258636 59.69259 60.945374 61.851402
           62.507843 62.556107 63.98535 64.359344 64.97066 65.69259 67.11527 67.63797
           68.25864 69.03319 70.02887 74.532 75.481186 76.985344 78.07615 78.6926)
          (21.05852 24.176456 35.789017 41.945377 42.93868 47.24706 51.738396 54.679886
           54.94538 59.69259 59.891727 61.458046 61.63797 61.786125 63.359344 66.5649
           69.4812 70.506165 72.6926 73.712204)
          (14.644884 21.05852 24.176456 29.741364 32.058517 35.789017 37.938683
           41.945377 42.93868 47.24706 51.738396 54.679886 54.94538 56.556114 58.35934
           59.69259 59.891727 60.258636 61.458046 61.63797 61.786125 63.359344 64.02887
           65.679886 66.5649 69.4812 70.506165 72.07616 72.6926 73.712204)
          (17.644878 21.05852 22.176458 29.741364 32.058517 37.938683 38.368866 42.93868
           43.792694 47.24706 53.938683 54.679886 55.94538 58.247063 58.35934 60.258636
           60.378216 61.29802 61.786125 62.07837 62.914185 63.359344 64.564896 65.679886
           67.02887 70.30328 71.28206 72.78613 73.6926 74.35934)
          (22.176458 24.741365 25.176458 27.058514 35.741364 36.78901 37.938683
           43.792694 43.938675 48.874855 52.738403 55.258636 55.94538 58.247063 58.35934
           60.378216 60.679882 60.891724 62.45804 62.507843 62.914185 64.359344
           64.564896 66.25865 67.564896 70.30328 71.50618 72.78613 73.6926 74.712204)
          (18.644886 24.741365 27.058514 27.673742 35.741364 37.938683 39.368866
           43.938675 45.556107 48.874855 50.368866 52.938675 55.258636 56.556114
           58.35934 59.450603 60.679882 61.076157 61.63797 62.507843 63.07837 64.359344
           65.115265 66.25865 68.02887 69.4812 70.74049 72.07616 73.359344 74.07837)
          (18.644886 22.058521 23.176455 30.74136 33.058514 38.938675 39.368866
           43.938675 44.79269 48.247055 50.368866 52.938675 55.679886 55.792694
           59.359344 59.450603 61.258644 61.340446 61.37822 62.786133 63.07837 64.359344
           65.56491 66.679886 68.02887 69.66771 70.74049 72.37822 73.359344 74.07837))))
  (loop
     for x in times
for next in (cdr times)
for chord in chordlist
append (loop for p in chord collect (list x (+ p 48)))))

(svg->lwav :infile #P"/tmp/test2.svg" :db -20 :quantize nil)

(svg->lwav :infile #P"/tmp/test2.svg" :db -20 :quantize nil)








(points->svg
 (let* ((num 90)
        (lookup (cons 0 (ou:integrate (loop for x below num collect (* 15 (expt 1/150 (/ x (- num 1)))))))))
   (mapcar (lambda (x) (cons (elt lookup (first x)) (rest x)))
           '((0 18.0) (0 45.5) (0 49.0) (0 60.5) (0 66.5) (0 67.5) (1 15.5) (1 47.0) (1 49.5) (1 61.0) (1 67.0) (1 68.0) (2 7.0) (2 48.0) (2 49.5) (2 61.5) (2 68.0) (2 68.5) (3 19.0) (3 46.5) (3 50.0) (3 61.5) (3 67.5) (3 68.5) (4 12.5) (4 48.0) (4 50.0) (4 62.0) (4 68.5) (4 69.0) (5 19.5) (5 48.5) (5 51.5) (5 62.0) (5 68.5) (5 69.5) (6 17.0) (6 48.5) (6 51.0) (6 62.5) (6 68.5) (6 69.5) (7 8.5) (7 49.5) (7 51.0) (7 63.0) (7 69.5) (7 70.0) (8 20.5) (8 48.0) (8 51.5) (8 63.0) (8 69.0) (8 70.0) (9 14.0) (9 49.5) (9 51.5) (9 63.5) (9 70.0) (9 70.5) (10 21.0) (10 50.0) (10 53.0) (10 63.5) (10 70.0) (10 71.0) (11 19.0) (11 50.5) (11 53.0) (11 64.5) (11 70.5) (11 71.5) (12 22.0) (12 49.5) (12 53.0) (12 64.5) (12 70.5) (12 71.5) (13 19.5) (13 51.0) (13 53.5) (13 65.0) (13 71.0) (13 72.0) (14 11.0) (14 52.0) (14 53.5) (14 65.5) (14 72.0) (14 72.5) (15 23.0) (15 50.5) (15 54.0) (15 65.5) (15 71.5) (15 72.5) (16 16.5) (16 52.0) (16 54.0) (16 66.0) (16 72.5) (16 73.0) (17 23.5) (17 52.5) (17 55.5) (17 66.0) (17 72.5) (17 73.5) (18 21.0) (18 52.5) (18 55.0) (18 66.5) (18 72.5) (18 73.5) (19 12.5) (19 53.5) (19 55.0) (19 67.0) (19 73.5) (19 74.0) (20 24.5) (20 52.0) (20 55.5) (20 67.0) (20 73.0) (20 74.0) (21 18.0) (21 53.5) (21 55.5) (21 67.5) (21 74.0) (21 74.5) (22 25.0) (22 54.0) (22 57.0) (22 67.5) (22 74.0) (22 75.0) (23 23.0) (23 54.5) (23 57.0) (23 68.5) (23 74.5) (23 75.5) (24 25.5) (24 53.0) (24 56.5) (24 68.0) (24 74.0) (24 75.0) (25 19.0) (25 55.5) (25 57.5) (25 68.5) (25 75.0) (25 76.0) (26 26.5) (26 54.0) (26 57.5) (26 69.0) (26 75.0) (26 76.0) (27 36.5) (27 51.5) (27 57.5) (27 70.0) (27 75.0) (27 77.0) (28 27.5) (28 50.5) (28 54.5) (28 70.0) (28 75.0) (28 76.0) (29 33.0) (29 48.5) (29 54.5) (29 70.5) (29 75.0) (29 76.5) (30 28.5) (30 51.5) (30 55.5) (30 71.0) (30 76.0) (30 77.0) (31 31.5) (31 50.5) (31 55.5) (31 71.5) (31 76.0) (31 77.0) (32 28.5) (32 56.0) (32 59.5) (32 71.0) (32 77.0) (32 78.0) (33 26.0) (33 57.5) (33 60.0) (33 71.5) (33 77.5) (33 78.5) (34 17.5) (34 58.5) (34 60.0) (34 72.0) (34 78.5) (34 79.0) (35 29.5) (35 57.0) (35 60.5) (35 72.0) (35 78.0) (35 79.0) (36 23.0) (36 58.5) (36 60.5) (36 72.5) (36 79.0) (36 79.5) (37 30.0) (37 59.0) (37 62.0) (37 72.5) (37 79.0) (37 80.0) (38 28.0) (38 59.5) (38 62.0) (38 73.5) (38 79.5) (38 80.5) (39 30.5) (39 58.0) (39 61.5) (39 73.0) (39 79.0) (39 80.0) (40 24.0) (40 60.5) (40 62.5) (40 73.5) (40 80.0) (40 81.0) (41 31.5) (41 59.0) (41 62.5) (41 74.0) (41 80.0) (41 81.0) (42 41.5) (42 56.5) (42 62.5) (42 75.0) (42 80.0) (42 82.0) (43 32.5) (43 55.5) (43 59.5) (43 75.0) (43 80.0) (43 81.0) (44 38.0) (44 53.5) (44 59.5) (44 75.5) (44 80.0) (44 81.5) (45 33.5) (45 56.5) (45 60.5) (45 76.0) (45 81.0) (45 82.0) (46 36.5) (46 55.5) (46 60.5) (46 76.5) (46 81.0) (46 82.0) (47 33.5) (47 61.0) (47 64.5) (47 76.0) (47 82.0) (47 83.0) (48 31.0) (48 62.5) (48 65.0) (48 76.5) (48 82.5) (48 83.5) (49 22.5) (49 63.5) (49 65.0) (49 77.0) (49 83.5) (49 84.0) (50 34.5) (50 62.0) (50 65.5) (50 77.0) (50 83.0) (50 84.0) (51 28.0) (51 63.5) (51 65.5) (51 77.5) (51 84.0) (51 84.5) (52 35.0) (52 64.0) (52 67.0) (52 77.5) (52 84.0) (52 85.0) (53 33.0) (53 64.5) (53 67.0) (53 78.5) (53 84.5) (53 85.5) (54 35.5) (54 63.0) (54 66.5) (54 78.0) (54 84.0) (54 85.0) (55 29.0) (55 65.5) (55 67.5) (55 78.5) (55 85.0) (55 86.0) (56 36.5) (56 64.0) (56 67.5) (56 79.0) (56 85.0) (56 86.0) (57 46.5) (57 61.5) (57 67.5) (57 80.0) (57 85.0) (57 87.0) (58 37.5) (58 60.5) (58 64.5) (58 80.0) (58 85.0) (58 86.0) (59 43.0) (59 58.5) (59 64.5) (59 80.5) (59 85.0) (59 86.5) (60 38.5) (60 61.5) (60 65.5) (60 81.0) (60 86.0) (60 87.0) (61 41.5) (61 60.5) (61 65.5) (61 81.5) (61 86.0) (61 87.0) (62 38.5) (62 66.0) (62 69.5) (62 81.0) (62 87.0) (62 88.0) (63 36.0) (63 67.5) (63 70.0) (63 81.5) (63 87.5) (63 88.5) (64 27.5) (64 68.5) (64 70.0) (64 82.0) (64 88.5) (64 89.0) (65 39.5) (65 67.0) (65 70.5) (65 82.0) (65 88.0) (65 89.0) (66 33.0) (66 68.5) (66 70.5) (66 82.5) (66 89.0) (66 89.5) (67 40.0) (67 69.0) (67 72.0) (67 82.5) (67 89.0) (67 90.0) (68 38.0) (68 69.5) (68 72.0) (68 83.5) (68 89.5) (68 90.5) (69 40.5) (69 68.0) (69 71.5) (69 83.0) (69 89.0) (69 90.0) (70 34.0) (70 70.5) (70 72.5) (70 83.5) (70 90.0) (70 91.0) (71 41.5) (71 69.0) (71 72.5) (71 84.0) (71 90.0) (71 91.0) (72 51.5) (72 66.5) (72 72.5) (72 85.0) (72 90.0) (72 92.0) (73 42.5) (73 65.5) (73 69.5) (73 85.0) (73 90.0) (73 91.0) (74 48.0) (74 63.5) (74 69.5) (74 85.5) (74 90.0) (74 91.5) (75 43.5) (75 66.5) (75 70.5) (75 86.0) (75 91.0) (75 92.0) (76 46.5) (76 65.5) (76 70.5) (76 86.5) (76 91.0) (76 92.0))))
 :outfile "/tmp/test5.svg")

(svg->wav :infile #P"/tmp/test5.svg")

(* 32 4 0.3 0.3)

König und Meyer 26740

|#

(defun svg->pd-text (&key (infile #P"/tmp/test.svg") (outfile "/tmp/chords.txt") (xquantize t) (yquantize t))
  (with-open-file (out outfile :direction :output
                       :if-exists :supersede)
    (let ((chords (group-to-chords (svg->points :infile infile :xquantize xquantize :yquantize yquantize))))
      (format out "~{~{~a~^ ~}~%~}~%"
              (mapcar #'(lambda (x) (remove-duplicates (sort x #'<))) chords))
      (length chords))))

(defun points->pd-text (points &key (outfile "/tmp/chords.txt"))
  (with-open-file (out outfile :direction :output
                       :if-exists :supersede)
    (let ((chords (group-to-chords points)))
      (format out "~{~{~a~^ ~}~%~}~%"
              (mapcar #'(lambda (x) (remove-duplicates (sort x #'<))) chords))
      (length chords))))


;;; (svg->pd-text :infile #P"/tmp/test2.svg" :quantize t)

(defun translate-point (point &key (x 0) (y 0))
  (cons (+ (first point) x)
        (cons (+ (second point) y)
              (nthcdr 2 point))))

#|
(points->svg
 (loop for p in (xscale-points *points-anfang* 9)
    append
      (destructuring-bind (x y) p
        (mapcar (lambda (pt) (translate-point pt :x (- x 60) :y y))
                *gliss-single*)))
 :timescale 1 :outfile "/tmp/schlussteil01.svg")

|#

(defun svg->clj (&key (infile #P"/tmp/test.svg") (outfile "testchords") (timescale 1) (xquantize t) (yquantize t))
  (let ((points (svg->points :infile infile :timescale timescale :xquantize xquantize :yquantize yquantize)))
    (with-open-file (out (pathname (format nil "/home/orm/work/programmieren/clojure/vbap-test/src/vbap_test/~a.clj" outfile))
                         :direction :output :if-exists :supersede)
      (format out "(ns vbap-test.~a)~%(def ~a '~a)~%"
              outfile outfile
              (loop
                 for pt in points
                 with curr-x = (caar points)
                 with curr-chd = '()
                 append (if (= (first pt) curr-x)
                            (progn
                              (push (second pt) curr-chd)
                              nil)
                            (prog1
                                (list (sort curr-chd #'<))
                              (setf curr-chd (list (second pt)))
                              (setf curr-x (first pt))))
                 into result
                 finally (return (if curr-chd
                                     (append result (list (sort curr-chd #'<)))
                                     result)))))))

;;; (pathname (format nil "/home/orm/work/programmieren/clojure/vbap-test/src/vbap_test/~a.clj" outfile))

(defun svglines->clj (&key (infile #P"/tmp/test.svg") (outfile (pathname (format nil "/tmp/testchords.clj"))) (timescale 1) (xquantize t) (yquantize t))
  (let ((lines (sort (svg->lines :infile infile :timescale timescale :xquantize xquantize :yquantize yquantize)
                     (lambda (x y) (< (first x) (first y))))))
    (with-open-file (out outfile
                         :direction :output :if-exists :supersede)
      (format out "~s~%" lines))))

#|
               (mapcar (lambda (x y) (cons y (cdr x)))
                      lines
                      (ou:differentiate (mapcar #'first lines)))
|#

(defun points->clj (points &key (outfile "testchords"))
  (with-open-file (out (pathname (format nil "/home/orm/work/programmieren/clojure/vbap-test/src/vbap_test/~a.clj" outfile))
                       :direction :output :if-exists :supersede)
    (format out "(ns vbap-test.~a)~%(def ~a '~a)~%"
            outfile outfile
            (loop
               for pt in points
               with curr-x = (caar points)
               with curr-chd = '()
               append (if (= (first pt) curr-x)
                          (progn
                            (push (second pt) curr-chd)
                            nil)
                          (prog1
                              (list (sort curr-chd #'<))
                            (setf curr-chd (list (second pt)))
                            (setf curr-x (first pt))))
               into result
               finally (return (if curr-chd
                                   (append result (list (sort curr-chd #'<)))
                                   result))))))