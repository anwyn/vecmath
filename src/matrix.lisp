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
    ((ux 1.0) uy
     vx (vy 1.0)))

(defvector mat3
    ((ux 1.0) uy      uz
     vx     (vy 1.0) vz
     wx      wy      (wz 1.0)))

(defvector mat4
    ((ux 1.0) uy uz uw
     vx (vy 1.0) vz vw
     wx wy (wz 1.0) ww
     tx ty tz (tw 1.0)))

(declaim (inline mat-ensure-store))
(defun mat-ensure-store (template &optional store)
  (declare (type mat template) (type (or null mat) store))
  (or store (vec-clone-empty template)))

(declaim (inline mat-ensure-copy))
(defun mat-ensure-copy (template &optional store)
  (declare (type mat template) (type (or null mat) store))
  (the mat (or store (copy-seq template))))

;;;; ----------------------------------------------------------------------------
;;;; * Constructors and Converters

(defvfun mat2<-columns ((u vec2) (v vec2)) mat2
  "Construct a 2x2 matrix from two column vectors."
  (values u.x u.y
          v.x v.y))

(defvfun mat2<-rows ((a vec2) (b vec2)) mat2
  "Construct a 2x2 matrix from two column vectors."
  (values a.x b.x
          a.y b.y))

(defvfun mat3<-columns ((u vec3) (v vec3) (w vec3)) mat3
  "Construct a 3x3 matrix from three column vectors."
  (values u.x u.y u.z
          v.x v.y v.z
          w.x w.y w.z))

(defvfun mat3<-rows ((a vec3) (b vec3) (c vec3)) mat3
  "Construct a 3x3 matrix from three column vectors."
  (values a.x b.x c.x
          a.y b.y c.y
          a.z b.z c.z))

(defvfun mat4<-columns ((u vec4) (v vec4) (w vec4) (trans vec4)) mat4
  "Construct a 4x4 matrix from four column vectors."
  (values u.x u.y u.z u.w
          v.x v.y v.z v.w
          w.x w.y w.z w.w
          trans.x trans.y trans.z trans.w))

(defvfun mat4<-rows ((a vec4) (b vec4) (c vec4) (d vec4)) mat4
  "Construct a 4x4 matrix from four column vectors."
  (values a.x b.x c.x d.x
          a.y b.y c.y d.y
          a.z b.z c.z d.z
          a.w b.w c.w d.w))

(defvfun mat4<-columns3 ((u vec3) (v vec3) (w vec3) (trans vec3)) mat4
  "Construct a 4x4 matrix from four column vectors in three space.
The last row will be set to #(0.0 0.0 0.0 1.0)."
  (values u.x u.y u.z +scalar-zero+
          v.x v.y v.z +scalar-zero+
          w.x w.y w.z +scalar-zero+
          trans.x trans.y trans.z +scalar-one+))

(defvfun mat4<-rows3 ((a vec3) (b vec3) (c vec3) (d vec3)) mat4
  "Construct a 4x4 matrix from four column vectors."
  (values a.x b.x c.x d.x
          a.y b.y c.y d.y
          a.z b.z c.z d.z
          +scalar-zero+ +scalar-zero+ +scalar-zero+ +scalar-one+))

;;;; ----------------------------------------------------------------------------
;;;; ** Convert matrices from and to the euler-angles type.

(defvfun mat3<-euler-angles ((e euler-angles)) mat3
  "Construct a rotation matrix from three angles, describing the rotation
about the Y, Z and X axis and applied in this order."
  (let ((cy (cos e.yaw))
        (sy (sin e.yaw))
        (cp (cos e.pitch))
        (sp (sin e.pitch))
        (cr (cos e.roll))
        (sr (sin e.roll)))
    (values (* cy cp) sp (- (* sy cp))
            (+ (* sy sr) (* (- cy) sp cr)) (* cp cr) (+ (* sy sp cr) (* cy sr))
            (+ (* cy sp sr) (* sy cr)) (* (- cp) sr) (+ (* (- sy) sp sr) (* cy cr)))))

(defvfun euler-angles<-mat3 ((m mat3)) euler-angles
  (declare (ignore m.vx m.vz))
  (cond ((> m.uy (- +scalar-one+ +delta+))
         (values (atan m.wx m.wz) +scalar-pi-half+ +scalar-zero+))
        ((< m.uy (+ +scalar-minus-one+ +delta+))
         (values (atan m.wx m.wz) (- +scalar-pi-half+) +scalar-zero+))
        (t
         (values (atan (- m.uz m.ux)) (atan (- m.wy) m.vy) (asin m.uy)))))

(defvfun mat4<-euler-angles ((e euler-angles) (trans vec3)) mat4
  "Construct a rotation matrix from three angles, describing the rotation
about the Y, Z and X axis and applied in this order."
  (let ((cy (cos e.yaw))
        (sy (sin e.yaw))
        (cp (cos e.pitch))
        (sp (sin e.pitch))
        (cr (cos e.roll))
        (sr (sin e.roll)))
    (values (* cy cp) sp (- (* sy cp))
            (+ (* sy sr) (* (- cy) sp cr)) (* cp cr) (+ (* sy sp cr) (* cy sr))
            (+ (* cy sp sr) (* sy cr)) (* (- cp) sr) (+ (* (- sy) sp sr) (* cy cr))
            trans.x trans.y trans.z +scalar-one+)))

(defvfun euler-angles<-mat4 ((m mat4)) euler-angles
  (declare (ignore m.uw m.vx m.vz m.vw m.ww m.tx m.ty m.tz m.tw))
  (cond ((> m.uy (- +scalar-one+ +delta+))
         (values (atan m.wx m.wz) +scalar-pi-half+ +scalar-zero+))
        ((< m.uy (+ +scalar-minus-one+ +delta+))
         (values (atan m.wx m.wz) (- +scalar-pi-half+) +scalar-zero+))
        (t
         (values (atan (- m.uz m.ux)) (atan (- m.wy) m.vy) (asin m.uy)))))

;;;; ----------------------------------------------------------------------------
;;;; ** Convert matrices from and to the axis/angle type.

(defvfun axis/angle<-mat3 ((m mat3)) axis/angle
  "Extract an axis and an angle from a rotation matrix."
  (:scalar-args-version nil)
  (multiple-value-bind (x y z)
      (vec3-normalize* (- m.wy m.vz)
                       (- m.uz m.wx)
                       (- m.vx m.uy))
    (let ((cos (half (- 1.0 (+ m.ux m.vy m.wz))))
          (sin (half (sqrt (+ (square x)
                              (square y)
                              (square z))))))
      (values x y z (atan sin cos)))))

(defvfun mat3<-axis/angle ((axis axis/angle)) mat3
  "Construct a rotation matrix from an axis/angle."
  (let* ((cos (cos axis.angle))
         (sin (sin axis.angle))
         (omc (- +scalar-one+ cos))
         (xy (* axis.x axis.y omc))
         (xz (* axis.x axis.z omc))
         (yz (* axis.y axis.z omc))
         (xsin (* axis.x sin))
         (ysin (* axis.y sin))
         (zsin (* axis.z sin)))
    (values (+ cos (* axis.x axis.x omc)) (- xy zsin) (+ xz ysin)
            (+ xy zsin) (+ cos (* axis.y axis.y omc)) (- yz xsin)
            (- xz ysin) (+ yz xsin) (+ cos (* axis.z axis.z omc)))))

(defvfun mat3<-axis-angle ((axis vec3) angle) mat3
  "Construct a rotation matrix from an axis and an angle"
  (multiple-value-bind (a b c)
      (vec3-normalize* axis.x axis.y axis.z)
    (mat3<-axis/angle* a b c angle)))

(defvfun axis/angle<-mat4 ((m mat4)) axis/angle
  "Extract an axis and an angle from a rotation matrix."
  (:scalar-args-version nil)
  (multiple-value-bind (x y z)
      (vec3-normalize* (- m.wy m.vz)
                       (- m.uz m.wx)
                       (- m.vx m.uy))
    (let ((cos (half (- 1.0 (+ m.ux m.vy m.wz))))
          (sin (half (sqrt (+ (square x)
                              (square y)
                              (square z))))))
      (values x y z (atan sin cos)))))

(defvfun mat4<-axis/angle ((axis axis/angle)) mat4
  "Construct a rotation matrix from an axis/angle."
  (let* ((cos (cos axis.angle))
         (sin (sin axis.angle))
         (omc (- +scalar-one+ cos))
         (xy (* axis.x axis.y omc))
         (xz (* axis.x axis.z omc))
         (yz (* axis.y axis.z omc))
         (xsin (* axis.x sin))
         (ysin (* axis.y sin))
         (zsin (* axis.z sin)))
    (values (+ cos (* axis.x axis.x omc)) (- xy zsin) (+ xz ysin) +scalar-zero+
            (+ xy zsin) (+ cos (* axis.y axis.y omc)) (- yz xsin) +scalar-zero+
            (- xz ysin) (+ yz xsin) (+ cos (* axis.z axis.z omc)) +scalar-zero+
            +scalar-zero+ +scalar-zero+ +scalar-zero+ +scalar-one+)))

(defvfun mat4<-axis-angle ((axis vec3) angle) mat4
  "Construct a rotation matrix from an axis and an angle"
  (multiple-value-bind (a b c)
      (vec3-normalize* axis.x axis.y axis.z)
    (mat4<-axis/angle* a b c angle)))

;;;; ----------------------------------------------------------------------------
;;;; * Matrix Multiplication With a Scalar

(defvfun mat-scale ((m mat) s &optional (store mat)) mat
  "Multiplicate a matrix with a scalar."
  (loop :with ret = (mat-ensure-store m store)
        :for a across m
        :for i = 0 then (1+ i)
        :do (setf (row-major-aref ret i) (* a s))
        :finally (return ret)))

(defvfun mat2-scale ((m mat2) s) mat2
  "Multiplicate a two dimensional matrix with a scalar."
  (values (* m.ux s) (* m.uy s) (* m.vx s) (* m.vy s)))

(defvfun mat3-scale ((m mat3) s) mat3
  "Multiplicate a three dimensional matrix with a scalar."
  (values (* m.ux s) (* m.uy s) (* m.uz s)
          (* m.vx s) (* m.vy s) (* m.vz s)
          (* m.wx s) (* m.wy s) (* m.wz s)))

(defvfun mat4-scale ((m mat4) s) mat4
  "Multiplicate a matrix in homogenous space with a scalar."
  (values (* m.ux s) (* m.uy s) (* m.uz s) (* m.uw s)
          (* m.vx s) (* m.vy s) (* m.vz s) (* m.vw s)
          (* m.wx s) (* m.wy s) (* m.wz s) (* m.ww s)
          (* m.tx s) (* m.ty s) (* m.tz s) (* m.tw s)))


;;;; ----------------------------------------------------------------------------
;;;; * Matrix Multiplication
;;;

(defvfun mat-mul ((m mat) (n mat) &optional (store mat)) mat
  "Concatenate two matrices by multiplying them."
  (let ((dim (isqrt (array-dimension m 0)))
        (m (if (eq store m) (vec-copy m) m))
        (n (if (eq store n) (vec-copy n) n))
        (dst (mat-ensure-store m store)))
    (loop for i from 0 below dim do
         (loop for j from 0 below dim do
              (setf (row-major-aref dst (+ (* i dim) j))
                    (loop for k from 0 below dim
                       sum (* (row-major-aref m (+ (* i dim) k))
                              (row-major-aref n (+ (* k dim) j))))))
         finally (return dst))))

(defvfun mat2-mul ((m mat2) (n mat2)) mat2
  "Concatenate two matrices by multiplying them."
  (:scalar-args-version nil)
  (values (+ (* m.ux n.ux) (* m.uy n.vx))
          (+ (* m.ux n.uy) (* m.uy n.vy))
          (+ (* m.vx n.ux) (* m.vy n.vx))
          (+ (* m.vx n.uy) (* m.vy n.vy))))

(defvfun mat3-mul ((m mat3) (n mat3)) mat3
  "Concatenate two matrices by multiplying them."
  (:scalar-args-version nil)
  (values (+ (* m.ux n.ux) (* m.uy n.vx) (* m.uz n.wx))
          (+ (* m.ux n.uy) (* m.uy n.vy) (* m.uz n.wy))
          (+ (* m.ux n.uz) (* m.uy n.vz) (* m.uz n.wz))
          (+ (* m.vx n.ux) (* m.vy n.vx) (* m.vz n.wx))
          (+ (* m.vx n.uy) (* m.vy n.vy) (* m.vz n.wy))
          (+ (* m.vx n.uz) (* m.vy n.vz) (* m.vz n.wz))
          (+ (* m.wx n.ux) (* m.wy n.vx) (* m.wz n.wx))
          (+ (* m.wx n.uy) (* m.wy n.vy) (* m.wz n.wy))
          (+ (* m.wx n.uz) (* m.wy n.vz) (* m.wz n.wz))))

(defvfun mat3-tmul ((m mat3) (n mat3)) mat3
  "Concatenate two matrices with the left one transposed."
  (:scalar-args-version nil)
  (values (+ (* m.ux n.ux) (* m.vx n.vx) (* m.wx n.wx))
          (+ (* m.ux n.uy) (* m.vx n.vy) (* m.wx n.wy))
          (+ (* m.ux n.uz) (* m.vx n.vz) (* m.wx n.wz))
          (+ (* m.uy n.ux) (* m.vy n.vx) (* m.wy n.wx))
          (+ (* m.uy n.uy) (* m.vy n.vy) (* m.wy n.wy))
          (+ (* m.uy n.uz) (* m.vy n.vz) (* m.wy n.wz))
          (+ (* m.uz n.ux) (* m.vz n.vx) (* m.wz n.wx))
          (+ (* m.uz n.uy) (* m.vz n.vy) (* m.wz n.wy))
          (+ (* m.uz n.uz) (* m.vz n.vz) (* m.wz n.wz))))

(defvfun mat4-mul ((m mat4) (n mat4)) mat4
  "Concatenate two matrices by multiplying them."
  (:scalar-args-version nil)
  (values (+ (* m.ux n.ux) (* m.uy n.vx) (* m.uz n.wx) (* m.uw n.tx))
          (+ (* m.ux n.uy) (* m.uy n.vy) (* m.uz n.wy) (* m.uw n.ty))
          (+ (* m.ux n.uz) (* m.uy n.vz) (* m.uz n.wz) (* m.uw n.tz))
          (+ (* m.ux n.uw) (* m.uy n.vw) (* m.uz n.ww) (* m.uw n.tw))
          (+ (* m.vx n.ux) (* m.vy n.vx) (* m.vz n.wx) (* m.vw n.tx))
          (+ (* m.vx n.uy) (* m.vy n.vy) (* m.vz n.wy) (* m.vw n.ty))
          (+ (* m.vx n.uz) (* m.vy n.vz) (* m.vz n.wz) (* m.vw n.tz))
          (+ (* m.vx n.uw) (* m.vy n.vw) (* m.vz n.ww) (* m.vw n.tw))
          (+ (* m.wx n.ux) (* m.wy n.vx) (* m.wz n.wx) (* m.ww n.tx))
          (+ (* m.wx n.uy) (* m.wy n.vy) (* m.wz n.wy) (* m.ww n.ty))
          (+ (* m.wx n.uz) (* m.wy n.vz) (* m.wz n.wz) (* m.ww n.tz))
          (+ (* m.wx n.uw) (* m.wy n.vw) (* m.wz n.ww) (* m.ww n.tw))
          (+ (* m.tx n.ux) (* m.ty n.vx) (* m.tz n.wx) (* m.tw n.tx))
          (+ (* m.tx n.uy) (* m.ty n.vy) (* m.tz n.wy) (* m.tw n.ty))
          (+ (* m.tx n.uz) (* m.ty n.vz) (* m.tz n.wz) (* m.tw n.tz))
          (+ (* m.tx n.uw) (* m.ty n.vw) (* m.tz n.ww) (* m.tw n.tw))))

;;;; ----------------------------------------------------------------------------
;;;; * Matrix Transpose

(defvfun mat-transpose ((m mat) &optional (store mat)) mat
  "Transpose the matrix, rows become columns and vice versa."
  (flet ((swap (matrix i j)
           (let ((tmp (row-major-aref matrix i)))
             (setf (row-major-aref matrix i) (row-major-aref matrix j)
                   (row-major-aref matrix j) tmp))))
    (let ((dim (isqrt (array-dimension m 0)))
          (result (mat-ensure-copy m store)))
      (loop for i from 0 below dim do
           (loop for j from (1+ i) below dim do
                (swap result (+ j (* i dim)) (+ i (* j dim))))
           finally (return result)))))

(defvfun mat2-transpose ((m mat2)) mat2
  "Transpose the matrix."
  (values m.ux m.vx m.uy m.vy))

(defvfun mat3-transpose ((m mat3)) mat3
  "Transpose the matrix."
  (values m.ux m.vx m.wx m.uy m.vy m.wy m.uz m.vz m.wz))

(defvfun mat4-transpose ((m mat4)) mat4
  "Transpose the matrix."
  (values m.ux m.vx m.wx m.tx
          m.uy m.vy m.wy m.ty
          m.uz m.vz m.wz m.tz
          m.uw m.vw m.ww m.tw))

;;;; ----------------------------------------------------------------------------
;;;; * Calculate Matrix Determinant

(defvfun mat2-determinant ((m mat2)) scalar
  "Calculate the determinant of the matrix."
  (- (* m.ux m.vy) (* m.vx m.uy)))

(defvfun mat3-determinant ((m mat3)) scalar
  "Calculate the determinant of the matrix."
  ;; Calculate the determinant by multiplying the elements of the
  ;; first row with the sub-determinants of the elements not on the
  ;; same row or column.
  (+ (- (* m.ux (mat2-determinant* m.vy m.vz m.wy m.wz))
        (* m.uy (mat2-determinant* m.vx m.vz m.wx m.wz)))
     (* m.uz (mat2-determinant* m.vx m.vy m.wx m.wy))))

(defvfun mat4-determinant ((m mat4)) scalar
  "Calculate the determinant of the matrix."
  ;; Calculate the determinant by multiplying the elements of the
  ;; first row with the sub-determinants of the elements not on the
  ;; same row or column.
  (+ (- (* m.ux (mat3-determinant* m.vy m.vz m.vw m.wy m.wz m.ww m.ty m.tz m.tw))
        (* m.uy (mat3-determinant* m.vx m.vz m.vw m.wx m.wz m.ww m.tx m.tz m.tw)))
     (- (* m.uz (mat3-determinant* m.vx m.vy m.vw m.wx m.wy m.ww m.tx m.ty m.tw))
        (* m.uw (mat3-determinant* m.vx m.vy m.vz m.wx m.wy m.wz m.tx m.ty m.tz)))))


;;;; ----------------------------------------------------------------------------
;;;; * Matrix Inversion

(defvfun mat3-invert ((m mat3)) mat3
  "Invert the matrix"
  (let ((det (mat3-determinant* m.ux m.uy m.uz m.vx m.vy m.vz m.wx m.wy m.wz)))
    (if (= +scalar-zero+ det)
        (values m.ux m.uy m.uz m.vx m.vy m.vz m.wx m.wy m.wz)
        (mat3-scale* (- (* m.vy m.wz) (* m.vz m.wy))
                     (- (* m.uz m.wy) (* m.uy m.wz))
                     (- (* m.uy m.vz) (* m.uz m.vy))
                     (- (* m.vz m.wx) (* m.vx m.wz))
                     (- (* m.ux m.wz) (* m.uz m.wx))
                     (- (* m.uz m.vx) (* m.ux m.vz))
                     (- (* m.vx m.wy) (* m.vy m.wx))
                     (- (* m.uy m.wx) (* m.ux m.wy))
                     (- (* m.ux m.vy) (* m.uy m.vx))
                     (invert det)))))


;;;; ----------------------------------------------------------------------------
;;;; * Matrix Negation
;;;

(defvfun mat2-negate ((m mat2)) mat2
  "Negate the matrix."
  (values (- m.ux) (- m.uy)
          (- m.vx) (- m.vy)))

(defvfun mat3-negate ((m mat3)) mat3
  "Negate the matrix."
  (values (- m.ux) (- m.uy) (- m.uz)
          (- m.vx) (- m.vy) (- m.vz)
          (- m.wx) (- m.wy) (- m.wz)))

(defvfun mat4-negate ((m mat4)) mat4
  "Negate the matrix."
  (values (- m.ux) (- m.uy) (- m.uz) (- m.uw)
          (- m.vx) (- m.vy) (- m.vz) (- m.vw)
          (- m.wx) (- m.wy) (- m.wz) (- m.ww)
          (- m.tx) (- m.ty) (- m.tz) (- m.tw)))

;;;; ----------------------------------------------------------------------------
;;;; * Transform Vectors
;;;

(defvfun vec-transform ((v vec) (m mat) &optional (store vec)) vec
  "Transform a vector using a matrix."
  (let ((dim (length v))
        (v (if (eq v store) (vec-copy v) v))
        (dst (vec-ensure-store v store)))
    (loop for i from 0 below dim do
          (setf (row-major-aref dst i)
                (loop for j from 0 below dim
                      sum (* (row-major-aref m (+ (* j dim) i))
                             (row-major-aref v j))))
          finally (return dst))))

(defvfun vec2-transform ((v vec2) (m mat2)) vec2
  "Transform a vector using a matrix."
  (values (+ (* m.ux v.x) (* m.vx v.y))
          (+ (* m.uy v.x) (* m.vy v.y))))

(defvfun vec3-transform ((v vec3) (m mat3)) vec3
  "Transform a vector using a matrix."
  (values (+ (* m.ux v.x) (* m.vx v.y) (* m.wx v.z))
          (+ (* m.uy v.x) (* m.vy v.y) (* m.wy v.z))
          (+ (* m.uz v.x) (* m.vz v.y) (* m.wz v.z))))

(defvfun vec4-transform ((v vec4) (m mat4)) vec4
  "Transform a vector using a matrix."
  (values (+ (* m.ux v.x) (* m.vx v.y) (* m.wx v.z) (* m.tx v.w))
          (+ (* m.uy v.x) (* m.vy v.y) (* m.wy v.z) (* m.ty v.w))
          (+ (* m.uz v.x) (* m.vz v.y) (* m.wz v.z) (* m.tz v.w))
          (+ (* m.uw v.x) (* m.vw v.y) (* m.ww v.z) (* m.tw v.w))))


;;; matrix.lisp ends here
