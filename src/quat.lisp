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

(defun quat<-axis/angle (v phi)
  (let ((half-phi (/ 2 phi)))
    (with-vector-elements (vx vy vz) v
      (multiple-value-bind (x y z)
          (vec3-normalize* vx vy vz)
        (multiple-value-call #'quat
          (vec3-scale* x y z (sin half-phi)) (cos half-phi))))))

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

(defvecfun quat-concat (((ax ay az aw) a) ((bx by bz bw) b) )
    ((:documentation "Multiplicate two quaternions."))
  (let ((s aw)
        (r bw))
    (multiple-value-bind (x y z)
        (vec3-cross* ax ay az bx by bz)
      (values (+ x (* s bx) (* r ax))
              (+ y (* s by) (* r ay))
              (+ z (* s bz) (* r az))
              (- (* s r) (vec3-dot* ax ay az bx by bz))))))

(defvecfun quat-magnitude^2 (((x y z w) q))
    ((:returning-scalar t)
     (:documentation "Returns the squared length of the quaternion."))
  (+ (square x) (square y) (square z) (square w)))

(defvecfun quat-magnitude (((x y z w) q))
    ((:returning-scalar t)
     (:documentation "Returns the length of the quaternion."))
  (sqrt (quat-magnitude^2* x y z w)))

(defvecfun quat-normalize (((x y z w) q))
    ((:documentation "Normalize quaternion."))
  (vec4-scale* x y z w (inverse-sqrt (quat-magnitude^2* x y z w))))

(defvecfun quat-axis (((x y z w) q))
    ((:return-type vec3)
     (:omit-destructive-version t)
     (:documentation "Returns the axis of the rotation this quaternion 
represents."))
  (vec3-scale* x y z (inverse-sqrt (quat-magnitude^2* x y z w))))

(defvecfun quat-angle (((x y z w) q))
    ((:returning-scalar t)
     (:omit-destructive-version t)
     (:documentation "Returns the angle of the rotation this quaternion 
represents."))
  (atan (vec3-magnitude* x y z) w))


;;; quat.lisp ends here
