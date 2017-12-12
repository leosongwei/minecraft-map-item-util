(defun is-8bit-array (array)
  (and (arrayp array)
       (equal '(unsigned-byte 8) (array-element-type array))))

(defun make-8bit-array (length)
  (make-array length :element-type '(unsigned-byte 8)))

(defun vector-8bit (vector)
  (let* ((length (length vector))
         (array (make-8bit-array length)))
    (dotimes (i length)
      (let ((num (aref vector i)))
        (setf (aref array i) num)))
    array))

(defun copy-array (from to &optional (to-offset 0))
  (let* ((from-len (length from))
         (to-len (length to)))
    (if (< (- to-len to-offset) from-len)
        (error "COPY-ARRAY: destination too sall"))
    (dotimes (i from-len)
      (setf (aref to (+ to-offset i))
            (aref from i)))
    to))

(defun is-ascii-char (char)
  (and (characterp char)
       (< (char-code char) 256)))

(defun is-ascii-string (string)
  (and (stringp string)
       (let ((flag t))
         (dotimes (i (length string))
           (if (not (is-ascii-char (aref string i)))
               (progn (setf flag nil)
                      (return))))
         flag)))

(defun binary-length (macro-list)
  (let ((length 0))
    (dolist (e macro-list)
      (cond ((and (integerp e) (<= 0 e) (< e 256)) (incf length))
            ((is-ascii-char e) (incf length))
            ((is-8bit-array e) (incf length (length e)))
            ((is-ascii-string e) (incf length (length e)))
            ((and (listp e)
                  (equal :empty (car e)))
             (incf length (cadr e)))
            (t (error "BINARY-LENGTH: unsupported notation"))))
    length))

(defun build-8bit-array (macro-list)
  (let* ((length (binary-length macro-list))
         (array (make-8bit-array length))
         (index 0))
    (dolist (e macro-list)
      (cond ((integerp e)
             (progn (setf (aref array index) e)
                    (incf index)))
            ((characterp e)
             (progn (setf (aref array index) (char-code e))
                    (incf index)))
            ((is-8bit-array e)
             (progn (copy-array e array index)
                    (incf index (length e))))
            ((stringp e)
             (progn (dotimes (i (length e))
                      (setf (aref array (+ i index)) (char-code (aref e i))))
                    (incf index (length e))))
            ((and (listp e)
                  (equal :empty (car e)))
             (incf index (cadr e)))
            (t (error "build-8bit-array: WTF???"))))
    array))

(defun write-binary-array-as-file (array filepath)
  (if (not (is-8bit-array array))
      (error "write-binary-array-as-file: not 8 bit array"))
  (with-open-file (stream filepath
                          :direction :output
                          :element-type '(unsigned-byte 8)
                          :if-does-not-exist :create
                          :if-exists :supersede)
    (write-sequence array stream))
  t)

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
