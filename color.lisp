;; (defstruct pixel
;;   (r 0 :type (unsigned-byte 8))
;;   (g 0 :type (unsigned-byte 8))
;;   (b 0 :type (unsigned-byte 8)))
(defstruct pixel
  (r 0 :type integer)
  (g 0 :type integer)
  (b 0 :type integer))
(defun mpi (r g b)
  (make-pixel :r r :g g :b b))

(defparameter *base-color-array*
  (vector ;(mpi 128 128 128) ;; 0
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
          (mpi 37 22 16))) ;; 51

(defparameter *full-color*
  (let ((a (make-array (* 4 (length *base-color-array*)))))
    (dotimes (i (length *base-color-array*))
      (let* ((rgb (aref *base-color-array* i))
             (r (pixel-r rgb))
             (g (pixel-g rgb))
             (b (pixel-b rgb)))
        (dotimes (j 4)
          (let* ((v (case j
                      (0 180)
                      (1 220)
                      (2 255)
                      (3 135))))
            (setf (aref a (+ j (* 4 i)))
                  (mpi (floor (/ (* r v) 255))
                       (floor (/ (* g v) 255))
                       (floor (/ (* b v) 255))))))))
    a))

(defun color-distance (r g b rr gg bb)
  (sqrt (+ (expt (- r rr) 2)
           (expt (- g gg) 2)
           (expt (- b bb) 2))))

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
  (+ 4
     (array-min-index
      (array-map *full-color* (lambda (pixel)
                                (color-distance r g b
                                                (pixel-r pixel)
                                                (pixel-g pixel)
                                                (pixel-b pixel)))))))
;; ---------------------------------------------------------------
;; FS dithering

(defun color+ (p0 p1)
  (mpi (+ (pixel-r p0) (pixel-r p1))
       (+ (pixel-g p0) (pixel-g p1))
       (+ (pixel-b p0) (pixel-b p1))))

(defun color- (p0 p1)
  (mpi (- (pixel-r p0) (pixel-r p1))
       (- (pixel-g p0) (pixel-g p1))
       (- (pixel-b p0) (pixel-b p1))))

(defun color* (p0 ratio)
  (mpi (floor (* (pixel-r p0) ratio))
       (floor (* (pixel-g p0) ratio))
       (floor (* (pixel-b p0) ratio))))

(defun find-closest-palette-color (pixel)
  (let ((index (array-min-index
                (array-map *full-color*
                           (lambda (p)
                             (color-distance (pixel-r pixel)
                                             (pixel-g pixel)
                                             (pixel-b pixel)
                                             (pixel-r p)
                                             (pixel-g p)
                                             (pixel-b p)))))))
    (aref *full-color* index)))
;;(find-closest-palette-color (mpi 102 54 68))

(defun fs-dithering-f (image)
  (do ((x 1 (+ x 1))) ((= x 127))
    (do ((y 0 (+ y 1))) ((= y 127))
      (let* ((old (aref image x y))
             (new (find-closest-palette-color old))
             (quant-error (color- old new)))
        (setf (aref image x y) new)
        (setf (aref image (1+ x) y)
              (color+ (aref image (1+ x) y)
                      (color* quant-error (/ 7 16))))
        (setf (aref image (1- x) y)
              (color+ (aref image (1- x) y)
                      (color* quant-error (/ 3 16))))
        (setf (aref image x (1+ y))
              (color+ (aref image x (1+ y))
                      (color* quant-error (/ 5 16))))
        (setf (aref image (1+ x) (1+ y))
              (color+ (aref image (1+ x) (1+ y))
                      (color* quant-error (/ 1 16)))))))
  image)

;; ------------------------------------------------------------------------------
(defmacro rgbf= (a b)
  ;; color float equal
  (let ((delta (float (/ 1 (* 8 255)))))
    `(< (abs (- ,a ,b)) ,delta)))

(defun rgb-2-hsl (rb gb bb)
  ;; RRGGBB: 0-255 int
  (let* ((r (float (/ rb 255)))
         (g (float (/ gb 255)))
         (b (float (/ bb 255)))
         (max (max r g b))
         (min (min r g b))
         (delta (- max min))
         (l (float (* 0.5 (+ max min))))
         (s (cond ((rgbf= delta 0.0) 0.0)
                  (t (float (/ delta (- 1 (abs (- (* 2 l) 1))))))))
         (h (cond ((rgbf= delta 0.0) 0.0)
                  ((rgbf= r max)
                   (* 60.0 (mod (float (/ (- g b) delta)) 6.0)))
                  ((rgbf= g max)
                   (* 60.0 (+ (float (/ (- b r) delta)) 2)))
                  ((rgbf= b max)
                   (* 60.0 (+ (float (/ (- r g) delta)) 4))))))
    (setf h (/ (* pi (/ (mod h 360.0) 180)) 2))
    (values h s l)))

(rgb-2-hsl 0 0 128)

(defstruct hsl
  (h 0.0 :type single-float)
  (s 0.0 :type single-float)
  (l 0.0 :type single-float))
(defmacro mhsl (h s l)
  `(make-hsl :h ,h :s ,s :l ,l))

(defun hsl-distance (h s l hh ss ll)
  (sqrt (+ (expt (- h hh) 2)
           (expt (- s ss) 2)
           (expt (- l ll) 2))))

;; -----------------------------------------------------
