;;; quat.lisp --- Simple vector and matrix math for 3d graphics
;;;                    _
;;;   __ _ _   _  __ _| |_
;;;  / _` | | | |/ _` | __|
;;; | (_| | |_| | (_| | |_
;;;  \__, |\__,_|\__,_|\__|
;;;     |_|
;;;
;;; Copyright (c) 2007 Ole Arndt <ole@sugarshark.com>
;;;

(in-package :vecmath)

(defvector quat (vec4)
    ())

(defvecfun quat<-axis/angle (((vx vy vz) v) phi)
    ((:type vec3)
     (:return-type quat)
     (:omit-destructive-version t)
     (:documentation "Construct a quaternation from an axis and an angle."))
  (declare (type angle phi))
  (let ((half-phi (/ phi 2)))
    (multiple-value-bind (x y z)
        (vec3-normalize* vx vy vz)
      (multiple-value-bind (qx qy qz)
          (vec3-scale* x y z (sin half-phi))
        (values qx qy qz (cos half-phi))))))

(defun quat<-euler (x y z)
  (let ((hx (coerce (/ x 2) 'angle))
        (hy (coerce (/ y 2) 'angle))
        (hz (coerce (/ z 2) 'angle)))
    (let ((roll (quat (sin hx) 0.0 0.0 (cos hx)))
          (pitch (quat 0.0 (sin hy) 0.0 (cos hy)))
          (yaw (quat 0.0 0.0 (sin hz) (cos hz))))
      (quat-mul (quat-mul pitch roll) yaw))))

(defun quat-identity! (q)
  (with-vector-elements (x y z w) q
    (setq x +scalar-zero+
          y +scalar-zero+
          z +scalar-zero+
          w +scalar-one+)))

(defvecfun quat-invert (((x y z w) q))
    ((:documentation "Invert the quaternion."))
  (values (- x) (- y) (- z) w))

(defvecfun quat-scale (((x y z w) q) s)
    ((:documentation "Scale the quaternion."))
  (vec4-scale* x y z w s))

(defvecfun quat-mul (((ax ay az aw) a) ((bx by bz bw) b) )
    ((:documentation "Multiplicate two quaternions."))
  (let ((s aw)
        (r bw))
    (multiple-value-bind (x y z)
        (vec3-cross* ax ay az bx by bz)
      (values (+ x (* s bx) (* r ax))
              (+ y (* s by) (* r ay))
              (+ z (* s bz) (* r az))
              (- (* s r) (vec3-dot* ax ay az bx by bz))))))

(defvecfun quat-magnitude-squared (((x y z w) q))
    ((:returning-scalar t)
     (:documentation "Returns the squared length of the quaternion."))
  (+ (square x) (square y) (square z) (square w)))

(defvecfun quat-magnitude (((x y z w) q))
    ((:returning-scalar t)
     (:documentation "Returns the length of the quaternion."))
  (sqrt (quat-magnitude-squared* x y z w)))

(defvecfun quat-normalize (((x y z w) q))
    ((:documentation "Normalize quaternion."))
  (vec4-scale* x y z w (inverse-sqrt (quat-magnitude-squared* x y z w))))

(defvecfun quat-axis (((x y z w) q))
    ((:return-type vec3)
     (:omit-destructive-version t)
     (:documentation "Returns the axis of the rotation this quaternion
represents."))
  (vec3-scale* x y z (inverse-sqrt (quat-magnitude-squared* x y z w))))

(defvecfun quat-angle (((x y z w) q))
    ((:returning-scalar t)
     (:omit-destructive-version t)
     (:documentation "Returns the angle of the rotation this quaternion
represents."))
  (atan (vec3-magnitude* x y z) w))


(defvecfun mat3<-quat (((x y z w) q))
    ((:type quat)
     (:return-type mat3)
     (:omit-destructive-version t)
     (:documentation "Convert a quaternation to a rotation matrix."))
  (let* ((x2 (+ x x))  (y2 (+ y y))  (z2 (+ z z))
         (xx (* x x2)) (yy (* y y2)) (zz (* z z2))
         (xy (* x y2)) (xz (* x z2)) (yz (* y z2))
         (wx (* w x2)) (wy (* w y2)) (wz (* w z2)))
    (values (- 1 (+ yy zz)) (- xy wz) (+ xz wy)
            (+ xy wz) (- 1 (+ xx zz)) (- yz wx)
            (- xz wy) (+ yz wx) (- 1 (+ xx yy)))))


(defvecfun mat4<-quat (((x y z w) q))
  ((:type quat)
   (:return-type mat4)
   (:omit-destructive-version t)
   (:documentation "Convert a quaternation to a rotation matrix."))
  (let* ((x2 (+ x x))  (y2 (+ y y))  (z2 (+ z z))
         (xx (* x x2)) (yy (* y y2)) (zz (* z z2))
         (xy (* x y2)) (xz (* x z2)) (yz (* y z2))
         (wx (* w x2)) (wy (* w y2)) (wz (* w z2)))
    (values (- 1 (+ yy zz)) (- xy wz) (+ xz wy) 0.0
            (+ xy wz) (- 1 (+ xx zz)) (- yz wx) 0.0
            (- xz wy) (+ yz wx) (- 1 (+ xx yy)) 0.0
            0.0 0.0 0.0 1.0)))

;;; quat.lisp ends here
