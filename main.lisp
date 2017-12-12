(use-package :sb-alien)

(load-shared-object "./libusestb.so" :dont-save t)
(define-alien-routine "read_image" (* unsigned-char) (filepath c-string))
(define-alien-routine "free_img" void (data-ptr (* unsigned-char)))

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

(defparameter *lenna* (with-alien ((pathname c-string (make-alien-string "./Lenna.png")))
                        (read-image pathname)))

(defstruct pixel
  (r 0 :type (unsigned-byte 8))
  (g 0 :type (unsigned-byte 8))
  (b 0 :type (unsigned-byte 8)))
(defun mpi (r g b)
  (make-pixel :r r :g g :b b))

(defun color-distance (r g b rr gg bb)
  (sqrt (+ (expt (- r rr) 2)
           (expt (- g gg) 2)
           (expt (- b bb) 2))))

(defparameter *color-array*
  (vector (mpi 128 128 128) ;; 0
          (mpi 127 178 56)  ;; 1
          (mpi 247 233 163) ;; 2
          (mpi 199 199 199) ;; 3
          (mpi 255 0   0)   ;; 4
          (mpi 160 160 255) ;; 5
          (mpi 167 167 167) ;; 6
          (mpi 0   124 0)   ;; 7
          (mpi 255 255 255) ;; 8
          (mpi 164 168 184) ;; 9
          (mpi 151 109 77)  ;; 10
          (mpi 112 112 112) ;; 11
          (mpi 64  64  255) ;; 12
          (mpi 143 119 72)  ;; 13
          (mpi 255 252 245) ;; 14
          (mpi 216 127 51)  ;; 15
          (mpi 178 76  216) ;; 16
          (mpi 102 153 216) ;; 17
          (mpi 229 229 51)  ;; 18
          (mpi 127 204 25)  ;; 19
          (mpi 242 127 165) ;; 20
          (mpi 76  76  76)  ;; 21
          (mpi 153 153 153) ;; 22
          (mpi 76  127 153) ;; 23
          (mpi 127 63  178) ;; 24
          (mpi 51  76  178) ;; 25
          (mpi 102 76  51)  ;; 26
          (mpi 102 127 51)  ;; 27
          (mpi 153 51  51)  ;; 28
          (mpi 25  25  25)  ;; 29
          (mpi 250 238 77)  ;; 30
          (mpi 92  219 213) ;; 31
          (mpi 74 128 255)  ;; 32
          (mpi 0  217  58)  ;; 33
          (mpi 129 86 49)   ;; 34
          (mpi 112 2  0)    ;; 35
          (mpi 209 177 161) ;; 36
          (mpi 159 82 36)   ;; 37
          (mpi 149 87 108)  ;; 38
          (mpi 112 108 138) ;; 39
          (mpi 186 133 36)  ;; 40
          (mpi 103 117 53)  ;; 41
          (mpi 160 77  78)  ;; 42
          (mpi 57  47  35)  ;; 43
          (mpi 135 107 98)  ;; 44
          (mpi 87  92  92)  ;; 45
          (mpi 122 73  88)  ;; 46
          (mpi 76  62  92)  ;; 47
          (mpi 76  50  35)  ;; 48
          (mpi 76  82  42)  ;; 49
          (mpi 142 60  46)  ;; 50
          (mpi 37  22  16))) ;; 51

(defun array-min-index (array)
  (let ((min-num (aref array 0))
        (min-index 0))
    (dotimes (i (length array))
      (if (< (aref array i) min-num)
          (progn (setf min-num (aref array i))
                 (setf min-index i))))
    min-index))

(defun array-map (array function)
  (let ((a (make-array (length array))))
    (dotimes (i (length array))
      (setf (aref a i) (funcall function (aref array i))))
    a))

(defun calculate-color (r g b)
  (array-min-index
   (array-map *color-array* (lambda (pixel)
                              (color-distance r g b
                                              (pixel-r pixel)
                                              (pixel-g pixel)
                                              (pixel-b pixel))))))



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
(defparameter *colors-buffer*
  (let ((array (make-8bit-array (* 128 128))))
    (dotimes (y 128)
      (dotimes (x 128)
        (setf (aref array (+ x (* y 128)))
              (let* ((pixel-offset (* 3 (+ x (* y 128))))
                     (r (deref *lenna* (+ pixel-offset 0)))
                     (g (deref *lenna* (+ pixel-offset 1)))
                     (b (deref *lenna* (+ pixel-offset 2))))
                (calculate-color r g b)))))
    array))

(aref *colors-buffer* (+ 125 (* 0 128)))


(defparameter *mc-map*
  (build-8bit-array
   `(,*nbt-head*
     ,*colors-buffer*
     ,*nbt-tail*)))

(write-binary-array-as-file *mc-map* "testmap.dat")
;; $ cat testmap.dat | gzip - > map_20002.dat
;; /give player minecraft:filled_map 1 20002
