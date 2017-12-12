(defmacro colorf= (a b)
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
         (s (cond ((colorf= delta 0.0) 0.0)
                  (t (float (/ delta (- 1 (abs (- (* 2 l) 1))))))))
         (h (cond ((colorf= delta 0.0) 0.0)
                  ((colorf= r max)
                   (* 60.0 (mod (float (/ (- g b) delta)) 6.0)))
                  ((colorf= g max)
                   (* 60.0 (+ (float (/ (- b r) delta)) 2)))
                  ((colorf= b max)
                   (* 60.0 (+ (float (/ (- r g) delta)) 4))))))
    (setf h (mod h 360.0))
    (values h s l)))

