;;; axis-angle.lisp --- Simple vector math for 3D graphics
;;;
;;;             _                             _
;;;   __ ___  _(_)___        __ _ _ __   __ _| | ___
;;;  / _` \ \/ / / __|_____ / _` | '_ \ / _` | |/ _ \
;;; | (_| |>  <| \__ \_____| (_| | | | | (_| | |  __/
;;;  \__,_/_/\_\_|___/      \__,_|_| |_|\__, |_|\___|
;;;                                     |___/
;;;
;;; Copyright (C) 2007 Ole Arndt <ole@sugarshark.com>
;;;

(in-package :vecmath)

;;;; ---------------------------------------------------------------------------
;;;; * Axis/Angle type
;;;;
;;;; The axis should always be normalized.

(defvector axis/angle
    (x y z angle))

;;;; ---------------------------------------------------------------------------
;;;; * Converters from axis/angle to and from matrices

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


;;;; ---------------------------------------------------------------------------
;;;; * Converters from axis/angle to and from quaternions

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


;;; axis-angle.lisp ends here
