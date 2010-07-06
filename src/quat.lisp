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

(defvfun quat-invert ((q quat)) quat
  "Invert the quaternion."
  (values (- q.x) (- q.y) (- q.z) q.w))

(defvfun quat-scale ((q quat) s) quat
  "Scale the quaternion."
  (values (* q.x s) (* q.y s) (* q.z s) (* q.w s)))

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
  (quat-scale* q.x q.y q.z q.w
               (inverse-sqrt (quat-magnitude^2* q.x q.y q.z q.w))))

(defvfun quat-axis ((q quat)) vec3
  "Returns the axis of the rotation this quaternion represents."
  (multiple-value-bind (x y z w)
      (if (> q.x +scalar-one+)
          (quat-normalize* q.x q.y q.z q.w)
          (values q.x q.y q.z q.w))
    (vec3-scale* x y z (inverse-sqrt (- +scalar-one+ (square w))))))

(defvfun quat-angle ((q quat)) scalar
  "Returns the angle of the rotation this quaternion represents."
  (* 2 (if (> q.x +scalar-one+)
           (atan (vec3-magnitude* q.x q.y q.z) q.w)
           (acos q.w))))


;;; quat.lisp ends here
