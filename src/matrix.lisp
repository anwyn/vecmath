;;; matrix.lisp --- Simple vector and matrix math for 3d graphics
;;;                  _        _      
;;;  _ __ ___   __ _| |_ _ __(_)_  __
;;; | '_ ` _ \ / _` | __| '__| \ \/ /
;;; | | | | | | (_| | |_| |  | |>  < 
;;; |_| |_| |_|\__,_|\__|_|  |_/_/\_\
;;;
;;; Copyright (C) 2007 Ole Arndt <ole@sugarshark.com>
;;;

(in-package :vecmath)


(defmacro with-matrix-rows (slots vector &body body)
  (let ((vec (gensym))
        (index-counter 0))
    `(let ((,vec ,vector))
       (declare (ignorable ,vec))
       ,@(let ((vector (if (and (consp vector) (eq (car vector) 'the))
                           (third vector)
                           vector)))
              (and (symbolp vector)
                   `((declare (%variable-rebinding ,vec ,vector)))))
       ,vec
       (macrolet ,(mapcar (lambda (slot-entry)
                            (let ((var-name
                                   (if (symbolp slot-entry)
                                       slot-entry
                                       (car slot-entry)))
                                  (index
                                   (if (symbolp slot-entry)
                                       index-counter
                                       (cadr slot-entry))))
                              (setf index-counter (1+ index))
                              `(,var-name (n)
                                          (list 'aref ',vec ,index n))))
                          slots)
         ,@body))))

(defun square-matrix-p(a)
  (let ((len (length a)))
    (= (isqrt len) (sqrt len))))

;;; A square matrix with element type scalar
(deftype mat (&optional rows-and-cols (element-type 'scalar))
  `(and (simple-array ,element-type (,(if (eq '* rows-and-cols)
                                          rows-and-cols
                                          (* rows-and-cols rows-and-cols))))
        (satisfies square-matrix-p)))

(defvector mat2 ()
    ((m00 1.0) m01
     m10 (m11 1.0)))

(defvector mat3 ()
    ((m00 1.0) m01 m02
     m10 (m11 1.0) m12
     m20 m21 (m22 1.0)))

(defvector mat4 ()
    ((m00 1.0) m01 m02 m03
     m10 (m11 1.0) m12 m13
     m20 m21 (m22 1.0) m23
     m30 m31 m32 (m33 1.0)))


;; (defun mat3<-rows (a b c)
;;   (with-vectors ((ax ay az) a
;;                  (bx by bz) b
;;                  (cx cy cz) c)
;;     ))

(defun mat3-column-0* (m)
  (with-vector-elements ((ax 0) (ay 3) (az 6)) m
    (values ax ay az)))

(defun mat3-column-0 (m)
  (with-vector-elements ((ax 0) (ay 3) (az 6)) m
    (vec3 ax ay ay)))

(defun mat3-column-0! (m v)
  (with-vector-elements ((ax 0) (ay 3) (az 6)) m
    (multiple-value-setq (ax ay az)
      (vec3->values v))))

(declaim (inline mat-ensure-store))
(defun mat-ensure-store (template store)
  (declare (type mat template) (type (or null mat) store))
  (or store (vec-clone-empty template)))

(declaim (inline mat-ensure-copy))
(defun mat-ensure-copy (template store)
  (declare (type mat template) (type (or null mat) store))
  (the mat (or store (vec-copy template))))

;;;; * Matrix Multiplication With a Scalar
;;;

(defun mat-scale (m s &optional store)
  "Multiplicate a matrix with a scalar."
  (declare (type mat m) (type scalar s) (type (or null mat) store))
  (map-into (mat-ensure-store m store) #'(lambda (a) (* a s)) m))

(defvecfun mat2-scale (((m00 m01 m10 m11) m) s)
    ((:documentation "Multiplicate a two dimensional matrix with a scalar."))
  (values (* m00 s) (* m01 s) (* m10 s) (* m11 s)))

(defvecfun mat3-scale (((m00 m01 m02 m10 m11 m12 m20 m21 m22) m) s)
    ((:documentation "Multiplicate a three dimensional matrix with a scalar."))
  (values (* m00 s) (* m01 s) (* m02 s)
          (* m10 s) (* m11 s) (* m12 s)
          (* m20 s) (* m21 s) (* m22 s)))

(defvecfun mat4-scale (((m00 m01 m02 m03
                       m10 m11 m12 m13
                       m20 m21 m22 m23
                       m30 m31 m32 m33) m) s)
    ((:documentation "Multiplicate a matrix in homogenous space with a scalar."))
  (values (* m00 s) (* m01 s) (* m02 s) (* m03 s)
          (* m10 s) (* m11 s) (* m12 s) (* m13 s)
          (* m20 s) (* m21 s) (* m22 s) (* m23 s)
          (* m30 s) (* m31 s) (* m32 s) (* m33 s)))


;;;; * Matrix Multiplication
;;;

(defun mat-mul (m n &optional store)
  "Concatenate two matrices by multiplying them."
  (declare (type mat m n) (type (or null mat) store))
  (let ((rows (isqrt (length m)))
        (m (if (eq store m) (vec-copy m) m))
        (n (if (eq store n) (vec-copy n) n))
        (dst (mat-ensure-store m store)))
    (loop for i from 0 below rows do
         (loop for j from 0 below rows do
              (setf (row-major-aref dst (+ (* i rows) j))
                    (loop for k from 0 below rows 
                       sum (* (row-major-aref m (+ (* i rows) k))
                              (row-major-aref n (+ (* k rows) j))))))
         finally (return dst))))

(defvecfun mat2-mul (((m00 m01 m10 m11) m) ((n00 n01 n10 n11) n))
    ((:documentation "Concatenate two matrices by multiplying them."))
  (values (+ (* m00 n00) (* m01 n10))
          (+ (* m00 n01) (* m01 n11))
          (+ (* m10 n00) (* m11 n10))
          (+ (* m10 n01) (* m11 n11))))

(defvecfun mat3-mul (((m00 m01 m02 m10 m11 m12 m20 m21 m22) m)
                     ((n00 n01 n02 n10 n11 n12 n20 n21 n22) n))
    ((:documentation "Concatenate two matrices by multiplying them."))
  (values (+ (* m00 n00) (* m01 n10) (* m02 n20))
          (+ (* m00 n01) (* m01 n11) (* m02 n21))
          (+ (* m00 n02) (* m01 n12) (* m02 n22))
          (+ (* m10 n00) (* m11 n10) (* m12 n20))
          (+ (* m10 n01) (* m11 n11) (* m12 n21))
          (+ (* m10 n02) (* m11 n12) (* m12 n22))
          (+ (* m20 n00) (* m21 n10) (* m22 n20))
          (+ (* m20 n01) (* m21 n11) (* m22 n21))
          (+ (* m20 n02) (* m21 n12) (* m22 n22))))

(defvecfun mat3-tmul (((m00 m01 m02 m10 m11 m12 m20 m21 m22) m)
                      ((n00 n01 n02 n10 n11 n12 n20 n21 n22) n))
    ((:documentation "Concatenate two matrices with the left one transposed."))
  (values (+ (* m00 n00) (* m10 n10) (* m20 n20))
          (+ (* m00 n01) (* m10 n11) (* m20 n21))
          (+ (* m00 n02) (* m10 n12) (* m20 n22))
          (+ (* m01 n00) (* m11 n10) (* m21 n20))
          (+ (* m01 n01) (* m11 n11) (* m21 n21))
          (+ (* m01 n02) (* m11 n12) (* m21 n22))
          (+ (* m02 n00) (* m12 n10) (* m22 n20))
          (+ (* m02 n01) (* m12 n11) (* m22 n21))
          (+ (* m02 n02) (* m12 n12) (* m22 n22))))

(defvecfun mat4-mul (((m00 m01 m02 m03 m10 m11 m12 m13
                           m20 m21 m22 m23 m30 m31 m32 m33) m)
                     ((n00 n01 n02 n03 n10 n11 n12 n13
                           n20 n21 n22 n23 n30 n31 n32 n33) n))
    ((:documentation "Concatenate two matrices by multiplying them."))
  (values (+ (* m00 n00) (* m01 n10) (* m02 n20) (* m03 n30))
          (+ (* m00 n01) (* m01 n11) (* m02 n21) (* m03 n31))
          (+ (* m00 n02) (* m01 n12) (* m02 n22) (* m03 n32))
          (+ (* m00 n03) (* m01 n13) (* m02 n23) (* m03 n33))
          (+ (* m10 n00) (* m11 n10) (* m12 n20) (* m13 n30))
          (+ (* m10 n01) (* m11 n11) (* m12 n21) (* m13 n31))
          (+ (* m10 n02) (* m11 n12) (* m12 n22) (* m13 n32))
          (+ (* m10 n03) (* m11 n13) (* m12 n23) (* m13 n33))
          (+ (* m20 n00) (* m21 n10) (* m22 n20) (* m23 n30))
          (+ (* m20 n01) (* m21 n11) (* m22 n21) (* m23 n31))
          (+ (* m20 n02) (* m21 n12) (* m22 n22) (* m23 n32))
          (+ (* m20 n03) (* m21 n13) (* m22 n23) (* m23 n33))
          (+ (* m30 n00) (* m31 n10) (* m32 n20) (* m33 n30))
          (+ (* m30 n01) (* m31 n11) (* m32 n21) (* m33 n31))
          (+ (* m30 n02) (* m31 n12) (* m32 n22) (* m33 n32))
          (+ (* m30 n03) (* m31 n13) (* m32 n23) (* m33 n33))))

 ;;;;; * Matrix Transpose

(defun mat-transpose (m &optional store)
  "Transpose the matrix, rows become columns and vice versa."
  (declare (type mat m))
  (flet ((swap (matrix i j)
           (let ((tmp (row-major-aref matrix i)))
             (setf (row-major-aref matrix i) (row-major-aref matrix j)
                   (row-major-aref matrix j) tmp))))
    (let ((rows (isqrt (length m)))
          (result (mat-ensure-copy m store)))
      (loop for i from 0 below rows do
           (loop for j from (1+ i) below rows do
                (swap result (+ j (* i rows)) (+ i (* j rows))))
           finally (return result)))))

(defvecfun mat2-transpose (((m00 m01 m10 m11) m))
    ((:documentation "Transpose the matrix."))
  (values m00 m10 m01 m11))

(defvecfun mat3-transpose (((m00 m01 m02 m10 m11 m12 m20 m21 m22) m))
    ((:documentation "Transpose the matrix."))
  (values m00 m10 m20 m01 m11 m21 m02 m12 m22))

(defvecfun mat4-transpose (((m00 m01 m02 m03 m10 m11 m12 m13 m20 m21 m22 m23 m30 m31 m32 m33) m))
    ((:documentation "Transpose the matrix."))
  (values m00 m10 m20 m30 m01 m11 m21 m31 m02 m12 m22 m32 m03 m13 m23 m33))

;;;; * Calculate Matrix Determinant

(defvecfun mat2-determinant (((m00 m01 m10 m11) m))
    ((:returning-scalar t)
     (:documentation "Calculate the determinant of the matrix."))
  (- (* m00 m11) (* m10 m01)))

(defvecfun mat3-determinant (((m00 m01 m02 m10 m11 m12 m20 m21 m22) m))
    ((:returning-scalar t)
     (:documentation "Calculate the determinant of the matrix."))
  ;; Calculate the determinant by multiplying the elements of the
  ;; first row with the sub-determinants of the elements not on the
  ;; same row or column.
  (+ (- (* m00 (mat2-determinant* m11 m12 m21 m22))
        (* m01 (mat2-determinant* m10 m12 m20 m22)))
     (* m02 (mat2-determinant* m10 m11 m20 m21))))

(defvecfun mat4-determinant (((m00 m01 m02 m03 m10 m11 m12 m13 m20 m21 m22 m23 m30 m31 m32 m33) m))
    ((:returning-scalar t)
     (:documentation "Calculate the determinant of the matrix."))
  ;; Calculate the determinant by multiplying the elements of the
  ;; first row with the sub-determinants of the elements not on the
  ;; same row or column.
  (+ (- (* m00 (mat3-determinant* m11 m12 m13 m21 m22 m23 m31 m32 m33))
        (* m01 (mat3-determinant* m10 m12 m13 m20 m22 m23 m30 m32 m33)))
     (- (* m02 (mat3-determinant* m10 m11 m13 m20 m21 m23 m30 m31 m33))
        (* m03 (mat3-determinant* m10 m11 m12 m20 m21 m22 m30 m31 m32)))))


;;;; * Matrix Inversion

(defvecfun mat3-invert (((m00 m01 m02 m10 m11 m12 m20 m21 m22) m))
    ((:documentation "Invert the matrix"))
  (let ((det (mat3-determinant* m00 m01 m02 m10 m11 m12 m20 m21 m22)))
    (if (= +scalar-zero+ det)
        (values m00 m01 m02 m10 m11 m12 m20 m21 m22)
        (mat3-scale* (- (* m11 m22) (* m12 m21))
                     (- (* m02 m21) (* m01 m22))
                     (- (* m01 m12) (* m02 m11))
                     (- (* m12 m20) (* m10 m22))
                     (- (* m00 m22) (* m02 m20))
                     (- (* m02 m10) (* m00 m12))
                     (- (* m10 m21) (* m11 m20))
                     (- (* m01 m20) (* m00 m21))
                     (- (* m00 m11) (* m01 m10))
                     (invert det)))))


;;;; * Matrix Negation
;;;

(defvecfun mat2-negate (((m00 m01 m10 m11) m))
    ((:documentation "Negate the matrix."))
  (values (- m00) (- m01) 
          (- m10) (- m11)))

(defvecfun mat3-negate (((m00 m01 m02 m10 m11 m12 m20 m21 m22) m))
    ((:documentation "Negate the matrix."))
  (values (- m00) (- m01) (- m02)
          (- m10) (- m11) (- m12)
          (- m20) (- m21) (- m22)))

(defvecfun mat4-negate (((m00 m01 m02 m03 m10 m11 m12 m13 m20 m21 m22 m23 m30 m31 m32 m33) m))
    ((:documentation "Negate the matrix."))
  (values (- m00) (- m01) (- m02) (- m03)
          (- m10) (- m11) (- m12) (- m13)
          (- m20) (- m21) (- m22) (- m23)
          (- m30) (- m31) (- m32) (- m33)))

;;;; * Transform Vectors
;;;

(defun vec-transform (v m &optional store)
  "Transform a vector using a matrix."
  (declare (type mat m) (type vec v) (type (or null vec) store))
  (let ((rows (length v))
        (v (if (eq v store) (vec-copy v) v))
        (dst (vec-ensure-store v store)))
    (loop for i from 0 below rows do
         (setf (row-major-aref dst i)
               (loop for j from 0 below rows 
                  sum (* (row-major-aref m (+ (* i rows) j))
                         (row-major-aref v j))))
         finally (return dst))))

(defvecfun vec2-transform (((x y) v)
                           ((m00 m01 m10 m11) m))
    ((:documentation "Transform a vector using a matrix."))
  (values (+ (* m00 x) (* m01 y))
          (+ (* m10 x) (* m11 y))))

(defvecfun vec3-transform (((x y z) v)
                           ((m00 m01 m02 m10 m11 m12 m20 m21 m22) m))
    ((:documentation "Transform a vector using a matrix."))
  (values (+ (* m00 x) (* m01 y) (* m02 z))
          (+ (* m10 x) (* m11 y) (* m12 z))
          (+ (* m20 x) (* m21 y) (* m22 z))))

(defvecfun vec4-transform (((x y z w) v)
                           ((m00 m01 m02 m03 m10 m11 m12 m13 m20 m21 m22 m23 m30 m31 m32 m33) m))
    ((:documentation "Transform a vector using a matrix."))
  (values (+ (* m00 x) (* m01 y) (* m02 z) (* m03 w))
          (+ (* m10 x) (* m11 y) (* m12 z) (* m13 w))
          (+ (* m20 x) (* m21 y) (* m22 z) (* m23 w))
          (+ (* m30 x) (* m31 y) (* m32 z) (* m33 w))))

(defvecfun mat3<-normalized-axis/angle (((x y z) axis) angle)
    ((:type vec3)
     (:return-type mat3)
     (:omit-destructive-version t)
     (:documentation "Construct a rotation matrix from an axis and an angle. 
Assume the axis port is already normalized."))
  (let* ((cos (cos angle))
         (sin (sin angle))
         (omc (invert cos))
         (xy (* x y omc))
         (xz (* x z omc))
         (yz (* y z omc))
         (xsin (* x sin))
         (ysin (* y sin))
         (zsin (* z sin)))
    (values (+ cos (* x x omc)) (- xy zsin) (+ xz ysin)
            (+ xy zsin) (+ cos (* y y omc)) (- yz xsin)
            (- xz ysin) (+ yz xsin) (+ cos (* z z omc)))))

(defvecfun mat3<-axis/angle (((x y z) axis) angle)
    ((:type vec3)
     (:return-type mat3)
     (:omit-destructive-version t)
     (:documentation "Construct a rotation matrix from an axis and an angle"))
  (multiple-value-bind (a b c)
      (vec3-normalize* x y z)
    (mat3<-normalized-axis/angle* a b c angle)))


;;; matrix.lisp ends here
