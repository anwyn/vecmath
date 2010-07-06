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

;;;; ----------------------------------------------------------------------------
;;;; * Matrix types
;;;;

(defvector mat2
    ((ax 1.0) ay
     bx       (by 1.0)))

(defvector mat3
    ((ax 1.0) ay      az
      bx     (by 1.0) bz
      cx      cy      (cz 1.0)))

(defvector mat4
    ((ax 1.0) ay az aw
     bx (by 1.0) bz bw
     cx cy (cz 1.0) cw
     dx dy dz (dw 1.0)))


(declaim (inline mat-ensure-store))
(defun mat-ensure-store (template &optional store)
  (declare (type mat template) (type (or null mat) store))
  (or store (vec-clone-empty template)))

(declaim (inline mat-ensure-copy))
(defun mat-ensure-copy (template &optional store)
  (declare (type mat template) (type (or null mat) store))
  (the mat (or store (copy-seq template))))

;;;; * Matrix Multiplication With a Scalar
;;;

(defvfun mat-scale ((m mat) s &optional (store mat)) mat
  "Multiplicate a matrix with a scalar."
  (loop :with ret = (mat-ensure-store m store)
        :for a across m
        :for i = 0 then (1+ i)
        :do (setf (row-major-aref ret i) (* a s))
        :finally (return ret)))

(defvfun mat2-scale ((m mat2) s) mat2
  "Multiplicate a two dimensional matrix with a scalar."
  (values (* m.ax s) (* m.ay s) (* m.bx s) (* m.by s)))

(defvfun mat3-scale ((m mat3) s) mat3
  "Multiplicate a three dimensional matrix with a scalar."
  (values (* m.ax s) (* m.ay s) (* m.az s)
          (* m.bx s) (* m.by s) (* m.bz s)
          (* m.cx s) (* m.cy s) (* m.cz s)))

(defvfun mat4-scale ((m mat4) s) mat4
  "Multiplicate a matrix in homogenous space with a scalar."
  (values (* m.ax s) (* m.ay s) (* m.az s) (* m.aw s)
          (* m.bx s) (* m.by s) (* m.bz s) (* m.bw s)
          (* m.cx s) (* m.cy s) (* m.cz s) (* m.cw s)
          (* m.dx s) (* m.dy s) (* m.dz s) (* m.dw s)))


;;;; * Matrix Multiplication
;;;

(defvfun mat-mul ((m mat) (n mat) &optional (store mat)) mat
  "Concatenate two matrices by multiplying them."
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

(defvfun mat2-mul ((m mat2) (n mat2)) mat2
  "Concatenate two matrices by multiplying them."
  (:scalar-args-version nil)
  (values (+ (* m.ax n.ax) (* m.ay n.bx))
          (+ (* m.ax n.ay) (* m.ay n.by))
          (+ (* m.bx n.ax) (* m.by n.bx))
          (+ (* m.bx n.ay) (* m.by n.by))))

(defvfun mat3-mul ((m mat3) (n mat3)) mat3
  "Concatenate two matrices by multiplying them."
  (:scalar-args-version nil)
  (values (+ (* m.ax n.ax) (* m.ay n.bx) (* m.az n.cx))
          (+ (* m.ax n.ay) (* m.ay n.by) (* m.az n.cy))
          (+ (* m.ax n.az) (* m.ay n.bz) (* m.az n.cz))
          (+ (* m.bx n.ax) (* m.by n.bx) (* m.bz n.cx))
          (+ (* m.bx n.ay) (* m.by n.by) (* m.bz n.cy))
          (+ (* m.bx n.az) (* m.by n.bz) (* m.bz n.cz))
          (+ (* m.cx n.ax) (* m.cy n.bx) (* m.cz n.cx))
          (+ (* m.cx n.ay) (* m.cy n.by) (* m.cz n.cy))
          (+ (* m.cx n.az) (* m.cy n.bz) (* m.cz n.cz))))

(defvfun mat3-tmul ((m mat3) (n mat3)) mat3
  "Concatenate two matrices with the left one transposed."
  (:scalar-args-version nil)
  (values (+ (* m.ax n.ax) (* m.bx n.bx) (* m.cx n.cx))
          (+ (* m.ax n.ay) (* m.bx n.by) (* m.cx n.cy))
          (+ (* m.ax n.az) (* m.bx n.bz) (* m.cx n.cz))
          (+ (* m.ay n.ax) (* m.by n.bx) (* m.cy n.cx))
          (+ (* m.ay n.ay) (* m.by n.by) (* m.cy n.cy))
          (+ (* m.ay n.az) (* m.by n.bz) (* m.cy n.cz))
          (+ (* m.az n.ax) (* m.bz n.bx) (* m.cz n.cx))
          (+ (* m.az n.ay) (* m.bz n.by) (* m.cz n.cy))
          (+ (* m.az n.az) (* m.bz n.bz) (* m.cz n.cz))))

(defvfun mat4-mul ((m mat4) (n mat4)) mat4
  "Concatenate two matrices by multiplying them."
  (:scalar-args-version nil)
  (values (+ (* m.ax n.ax) (* m.ay n.bx) (* m.az n.cx) (* m.aw n.dx))
          (+ (* m.ax n.ay) (* m.ay n.by) (* m.az n.cy) (* m.aw n.dy))
          (+ (* m.ax n.az) (* m.ay n.bz) (* m.az n.cz) (* m.aw n.dz))
          (+ (* m.ax n.aw) (* m.ay n.bw) (* m.az n.cw) (* m.aw n.dw))
          (+ (* m.bx n.ax) (* m.by n.bx) (* m.bz n.cx) (* m.bw n.dx))
          (+ (* m.bx n.ay) (* m.by n.by) (* m.bz n.cy) (* m.bw n.dy))
          (+ (* m.bx n.az) (* m.by n.bz) (* m.bz n.cz) (* m.bw n.dz))
          (+ (* m.bx n.aw) (* m.by n.bw) (* m.bz n.cw) (* m.bw n.dw))
          (+ (* m.cx n.ax) (* m.cy n.bx) (* m.cz n.cx) (* m.cw n.dx))
          (+ (* m.cx n.ay) (* m.cy n.by) (* m.cz n.cy) (* m.cw n.dy))
          (+ (* m.cx n.az) (* m.cy n.bz) (* m.cz n.cz) (* m.cw n.dz))
          (+ (* m.cx n.aw) (* m.cy n.bw) (* m.cz n.cw) (* m.cw n.dw))
          (+ (* m.dx n.ax) (* m.dy n.bx) (* m.dz n.cx) (* m.dw n.dx))
          (+ (* m.dx n.ay) (* m.dy n.by) (* m.dz n.cy) (* m.dw n.dy))
          (+ (* m.dx n.az) (* m.dy n.bz) (* m.dz n.cz) (* m.dw n.dz))
          (+ (* m.dx n.aw) (* m.dy n.bw) (* m.dz n.cw) (* m.dw n.dw))))

 ;;;;; * Matrix Transpose

(defvfun mat-transpose ((m mat) &optional (store mat)) mat
  "Transpose the matrix, rows become columns and vice versa."
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

(defvfun mat2-transpose ((m mat2)) mat2
  "Transpose the matrix."
  (values m.ax m.bx m.ay m.by))

(defvfun mat3-transpose ((m mat3)) mat3
  "Transpose the matrix."
  (values m.ax m.bx m.cx m.ay m.by m.cy m.az m.bz m.cz))

(defvfun mat4-transpose ((m mat4)) mat4
  "Transpose the matrix."
  (values m.ax m.bx m.cx m.dx m.ay m.by m.cy m.dy m.az m.bz m.cz m.dz m.aw m.bw m.cw m.dw))

;;;; * Calculate Matrix Determinant

(defvfun mat2-determinant ((m mat2)) scalar
  "Calculate the determinant of the matrix."
  (- (* m.ax m.by) (* m.bx m.ay)))

(defvfun mat3-determinant ((m mat3)) scalar
  "Calculate the determinant of the matrix."
  ;; Calculate the determinant by multiplying the elements of the
  ;; first row with the sub-determinants of the elements not on the
  ;; same row or column.
  (+ (- (* m.ax (mat2-determinant* m.by m.bz m.cy m.cz))
        (* m.ay (mat2-determinant* m.bx m.bz m.cx m.cz)))
     (* m.az (mat2-determinant* m.bx m.by m.cx m.cy))))

(defvfun mat4-determinant ((m mat4)) scalar
  "Calculate the determinant of the matrix."
  ;; Calculate the determinant by multiplying the elements of the
  ;; first row with the sub-determinants of the elements not on the
  ;; same row or column.
  (+ (- (* m.ax (mat3-determinant* m.by m.bz m.bw m.cy m.cz m.cw m.dy m.dz m.dw))
        (* m.ay (mat3-determinant* m.bx m.bz m.bw m.cx m.cz m.cw m.dx m.dz m.dw)))
     (- (* m.az (mat3-determinant* m.bx m.by m.bw m.cx m.cy m.cw m.dx m.dy m.dw))
        (* m.aw (mat3-determinant* m.bx m.by m.bz m.cx m.cy m.cz m.dx m.dy m.dz)))))


;;;; * Matrix Inversion

(defvfun mat3-invert ((m mat3)) mat3
  "Invert the matrix"
  (let ((det (mat3-determinant* m.ax m.ay m.az m.bx m.by m.bz m.cx m.cy m.cz)))
    (if (= +scalar-zero+ det)
        (values m.ax m.ay m.az m.bx m.by m.bz m.cx m.cy m.cz)
        (mat3-scale* (- (* m.by m.cz) (* m.bz m.cy))
                     (- (* m.az m.cy) (* m.ay m.cz))
                     (- (* m.ay m.bz) (* m.az m.by))
                     (- (* m.bz m.cx) (* m.bx m.cz))
                     (- (* m.ax m.cz) (* m.az m.cx))
                     (- (* m.az m.bx) (* m.ax m.bz))
                     (- (* m.bx m.cy) (* m.by m.cx))
                     (- (* m.ay m.cx) (* m.ax m.cy))
                     (- (* m.ax m.by) (* m.ay m.bx))
                     (invert det)))))


;;;; * Matrix Negation
;;;

(defvfun mat2-negate ((m mat2)) mat2
  "Negate the matrix."
  (values (- m.ax) (- m.ay)
          (- m.bx) (- m.by)))

(defvfun mat3-negate ((m mat3)) mat3
  "Negate the matrix."
  (values (- m.ax) (- m.ay) (- m.az)
          (- m.bx) (- m.by) (- m.bz)
          (- m.cx) (- m.cy) (- m.cz)))

(defvfun mat4-negate ((m mat4)) mat4
  "Negate the matrix."
  (values (- m.ax) (- m.ay) (- m.az) (- m.aw)
          (- m.bx) (- m.by) (- m.bz) (- m.bw)
          (- m.cx) (- m.cy) (- m.cz) (- m.cw)
          (- m.dx) (- m.dy) (- m.dz) (- m.dw)))

;;;; * Transform Vectors
;;;

(defvfun vec-transform ((v vec) (m mat) &optional (store vec)) vec
  "Transform a vector using a matrix."
  (let ((rows (length v))
        (v (if (eq v store) (vec-copy v) v))
        (dst (vec-ensure-store v store)))
    (loop for i from 0 below rows do
          (setf (row-major-aref dst i)
                (loop for j from 0 below rows
                      sum (* (row-major-aref m (+ (* i rows) j))
                             (row-major-aref v j))))
          finally (return dst))))

(defvfun vec2-transform ((v vec2) (m mat2)) vec2
  "Transform a vector using a matrix."
  (values (+ (* m.ax v.x) (* m.ay v.y))
          (+ (* m.bx v.x) (* m.by v.y))))

(defvfun vec3-transform ((v vec3) (m mat3)) vec3
  "Transform a vector using a matrix."
  (values (+ (* m.ax v.x) (* m.ay v.y) (* m.az v.z))
          (+ (* m.bx v.x) (* m.by v.y) (* m.bz v.z))
          (+ (* m.cx v.x) (* m.cy v.y) (* m.cz v.z))))

(defvfun vec4-transform ((v vec4) (m mat4)) vec4
  "Transform a vector using a matrix."
  (values (+ (* m.ax v.x) (* m.ay v.y) (* m.az v.z) (* m.aw v.w))
          (+ (* m.bx v.x) (* m.by v.y) (* m.bz v.z) (* m.bw v.w))
          (+ (* m.cx v.x) (* m.cy v.y) (* m.cz v.z) (* m.cw v.w))
          (+ (* m.dx v.x) (* m.dy v.y) (* m.dz v.z) (* m.dw v.w))))


;;; matrix.lisp ends here
