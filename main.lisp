(load "binary.lisp")

;; (write-binary-array-as-file
;;  (build-8bit-array
;;   `(255
;;     #xAB #xCD
;;     ,(make-8bit-array 5)
;;     ,(vector-8bit #(9 8 7 6 5 4 3 2 1 0))
;;     ,(make-8bit-array 5)
;;     #xff #xff
;;     (:empty ,(+ 10 10))
;;     #\a "abcdef"))
;;  "test.dat")

(defparameter *nbt-head*
  (build-8bit-array
   `(#x0A 00 00
          #x0A 00 04 "data"
          03 00 07 "zCenter" 00 #xFF #xFF #xFF
          03 00 07 "xCenter" 00 #xFF #xFF #xFF
          01 00 #x11 "unlimitedTracking" 00
          01 00 #x10 "trackingPosition" 00
          02 00 05 "width" 00 #x80
          02 00 06 "height" 00 #x80
          01 00 05 "scale" 00
          01 00 09 "dimension" 00
          07 00 06 "colors" 00 00 #x40 00)))

(defparameter *nbt-tail* (build-8bit-array '(00 00)))

;; Colors[widthOffset + heightOffset * width]
(defparameter *colors-array* (let ((array (make-8bit-array (* 128 128))))
                               (dotimes (y 128)
                                 (dotimes (x 128)
                                   (setf (aref array (+ x (* y 128)))
                                         (if (= 0 (mod x 2))
                                             4;; red
                                             18;; yellow
                                             ))))
                               array))

(defparameter *mc-map*
  (build-8bit-array
   `(,*nbt-head*
     ,*colors-array*
     ,*nbt-tail*)))


(write-binary-array-as-file *mc-map* "testmap.dat")
;; $ cat testmap.dat | gzip - > map_20002.dat
