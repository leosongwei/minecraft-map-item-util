(use-package :sb-alien)

(load-shared-object "./libusestb.so" :dont-save t)
(define-alien-routine "read_image" (* unsigned-char) (filepath c-string))
(define-alien-routine "free_img" void (data-ptr (* unsigned-char)))
(define-alien-routine "write_img" void (data-ptr (* unsigned-char)))

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
(load "color.lisp")

;; (defparameter *lenna* (with-alien ((pathname c-string (make-alien-string "./Lenna.png")))
;;                         (read-image pathname)))

(defparameter *lenna* (with-alien ((pathname c-string (make-alien-string "./Lenna.png")))
                        (read-image pathname)))

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

(defparameter *image*
  (let ((a (make-array '(128 128))))
    (dotimes (y 128)
      (dotimes (x 128)
        (let* ((pixel-offset (* 3 (+ x (* y 128))))
               (r (deref *lenna* (+ pixel-offset 0)))
               (g (deref *lenna* (+ pixel-offset 1)))
               (b (deref *lenna* (+ pixel-offset 2))))
          (setf (aref a x y) (mpi r g b)))))
    a))
(fs-dithering-f *image*)

(defun output-image (image)
  (let ((a (make-8bit-array (* 128 128))))
    (dotimes (x 128)
      (dotimes (y 128)
        (let* ((pixel (aref image x y))
               (r (pixel-r pixel))
               (g (pixel-g pixel))
               (b (pixel-b pixel)))
          (setf (aref a (+ x (* y 128)))
                (calculate-color r g b)))))
    a))

;; Colors[widthOffset + heightOffset * width]
;; (defparameter *colors-buffer*
;;   (let ((array (make-8bit-array (* 128 128))))
;;     (dotimes (y 128)
;;       (dotimes (x 128)
;;         (setf (aref array (+ x (* y 128)))
;;               (let* ((pixel-offset (* 3 (+ x (* y 128))))
;;                      (r (deref *lenna* (+ pixel-offset 0)))
;;                      (g (deref *lenna* (+ pixel-offset 1)))
;;                      (b (deref *lenna* (+ pixel-offset 2))))
;;                 (calculate-color r g b)))))
;;     array))
(defparameter *colors-buffer* (output-image *image*))

(aref *colors-buffer* (+ 125 (* 0 128)))

(defparameter *mc-map*
  (build-8bit-array
   `(,*nbt-head*
     ,*colors-buffer*
     ,*nbt-tail*)))

(write-binary-array-as-file *mc-map* "testmap.dat")
;; $ cat testmap.dat | gzip - > map_20002.dat
;; /give player minecraft:filled_map 1 20002

(with-alien ((img-buffer (* unsigned-char) (make-alien unsigned-char (* 128 128 3))))
  (dotimes (y 128)
    (dotimes (x 128)
      (let* ((pixel-offset (+ x (* y 128)))
             (id (aref *colors-buffer* pixel-offset))
             (location (* 3 pixel-offset))
             (r (pixel-r (aref *full-color* (- id 4))))
             (g (pixel-g (aref *full-color* (- id 4))))
             (b (pixel-b (aref *full-color* (- id 4)))))
        (setf (deref img-buffer (+ location 0)) r)
        (setf (deref img-buffer (+ location 1)) g)
        (setf (deref img-buffer (+ location 2)) b))))
  (write-img img-buffer))
