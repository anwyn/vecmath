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


(defvector axis/angle
    (x y z angle))

(defvfun axis/angle<-mat3 ((m mat3)) axis/angle
  "Extract an axis and an angle from a rotation matrix."
  (:scalar-args-version nil)
  (let* ((x (- m.cy m.bz))
         (y (- m.az m.cx))
         (z (- m.bx m.ay))
         (cos (half (- 1.0 (+ m.ax m.by m.cz))))
         (sin (half (sqrt (+ (square x)
                             (square y)
                             (square z))))))
    (values x y z (atan sin cos))))

(defvfun axis/angle<-quat ((q quat)) axis/angle
  "Extract an axis and an angle from a quaternion."
  (multiple-value-bind (ax ay az)
      (quat-axis* q.x q.y q.z q.w)
    (values ax ay az (quat-angle* q.x q.y q.z q.w))))



;;; axis-angle.lisp ends here
