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

(defvector quat
  (x y z (w 1.0)))


(defun quat-identity! (q)
  (with-vector (q quat)
    (setq q.x +scalar-zero+
          q.y +scalar-zero+
          q.z +scalar-zero+
          q.w +scalar-one+)))

(defvfun quat<-axis/angle ((v vec3) phi) quat
  "Get a quaternation from an axis/angle."
  (let ((half-phi (/ 2 phi)))
    (multiple-value-bind (x y z)
        (vec3-normalize* v.x v.y v.z)
      (multiple-value-call #'values
        (vec3-scale* x y z (sin half-phi))
        (cos half-phi)))))

(defvfun quat-invert ((q quat)) quat
  "Invert the quaternion."
  (values (- q.x) (- q.y) (- q.z) q.w))

(defvfun quat-scale ((q quat) s) quat
  "Scale the quaternion."
  (vec4-scale* q.x q.y q.z q.w s))

(defvfun quat-mul ((a quat) (b quat)) quat
  "Multiplicate two quaternions."
  (let ((s a.w)
        (r b.w)
        (x (- (* a.y b.z) (* a.z b.y)))
        (y (- (* a.z b.x) (* a.x b.z)))
        (z (- (* a.x b.y) (* a.y b.x))))
    (values (+ x (* s b.x) (* r a.x))
            (+ y (* s b.y) (* r a.y))
            (+ z (* s b.z) (* r a.z))
            (- (* s r)
               (+ (* a.x b.x)
                  (* a.y b.y)
                  (* a.z b.z))))))

(defvfun quat-magnitude^2 ((q quat)) scalar
  "Returns the squared length of the quaternion."
  (+ (square q.x) (square q.y) (square q.z) (square q.w)))

(defvfun quat-magnitude ((q quat)) scalar
  "Returns the length of the quaternion."
  (sqrt (quat-magnitude^2* q.x q.y q.z q.w)))

(defvfun quat-normalize ((q quat)) quat
  "Normalize quaternion."
  (vec4-scale* q.x q.y q.z q.w
               (inverse-sqrt (quat-magnitude^2* q.x q.y q.z q.w))))

(defvfun quat-axis ((q quat)) vec3
  "Returns the axis of the rotation this quaternion represents."
  (vec3-scale* q.x q.y q.z
               (inverse-sqrt (quat-magnitude^2* q.x q.y q.z q.w))))

(defvfun quat-angle ((q quat)) scalar
  "Returns the angle of the rotation this quaternion represents."
  (atan (vec3-magnitude* q.x q.y q.z) q.w))


;;; quat.lisp ends here
