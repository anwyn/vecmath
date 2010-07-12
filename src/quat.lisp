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

;;;; ----------------------------------------------------------------------------
;;;; * Constructors and Converters


(defun quat-identity! (q)
  (with-vector (q quat)
    (setq q.x +scalar-zero+
          q.y +scalar-zero+
          q.z +scalar-zero+
          q.w +scalar-one+)))

;;;; ----------------------------------------------------------------------------
;;;; ** Convert quaternions from and to the euler-angles type.

(defvfun quat<-euler-angles ((e euler-angles)) quat
  (let* ((yaw (half e.yaw))
         (pitch (half e.pitch))
         (roll (half e.roll))
         (cy (cos yaw))
         (sy (sin yaw))
         (cp (cos pitch))
         (sp (sin pitch))
         (cr (cos roll))
         (sr (sin roll))
         (cycp (* cy cp))
         (sysp (* sy sp)))
    (values (+ (* cycp sr) (* sysp cr))
            (+ (* sy cp cr) (* cy sy sr))
            (- (* cy sp cr) (* sy cp sr))
            (- (* cycp cr) (* sysp sr)))))

(defvfun euler-angles<-quat ((q quat)) euler-angles
  (let* ((sx (square q.x))
         (sy (square q.y))
         (sz (square q.z))
         (sw (square q.w))
         (test (+ (* q.x q.y) (* q.z q.w)))
         (unit (+ sx sy sz sw)))
    (cond ((> test (* unit (- +scalar-half+ +delta+)))
           (values (* 2 (atan q.x q.w)) +scalar-pi-half+ +scalar-zero+))
          ((< test (* unit (- +delta+ +scalar-half+)))
           (values (* -2 (atan q.x q.w)) (- +scalar-pi-half+) +scalar-zero+))
          (t
           (let ()
             (values (atan (- (* 2 q.y q.w) (* 2 q.x q.z)) (- (+ sx sw) sy sz))
                     (asin (/ (* 2 test) unit))
                     (atan (- (* 2 q.x q.w) (* 2 q.y q.z)) (- (+ sy sw) sx sz))))))))

;;;; ----------------------------------------------------------------------------
;;;; ** Convert quaternions from and to the axis/angle type.

(defvfun axis/angle<-quat ((q quat)) axis/angle
  "Extract an axis and an angle from a quaternion."
  (multiple-value-bind (x y z w)
      (if (> q.x +scalar-one+)
          (quat-normalize* q.x q.y q.z q.w)
          (values q.x q.y q.z q.w))
    (let ((s (inverse-sqrt (- +scalar-one+ (square w)))))
      (values (* x s) (* y s) (* z s) (* 2 (acos w))))))

(defvfun quat<-axis/angle ((axis axis/angle)) quat
  "Get a quaternation from an axis/angle."
  (let* ((half-phi (* axis.angle +scalar-half+))
         (sinus (sin half-phi)))
    (values (* axis.x sinus) (* axis.y sinus) (* axis.z sinus) (cos half-phi))))

(defvfun quat<-axis-angle ((v vec3) phi) quat
  "Get a quaternation from an axis and an angle."
  (multiple-value-bind (x y z)
      (vec3-normalize* v.x v.y v.z)
    (quat<-axis/angle* x y z phi)))


;;;; ----------------------------------------------------------------------------
;;;; * Quaternion functions


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

;;;; ----------------------------------------------------------------------------
;;;; * Extractors

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
