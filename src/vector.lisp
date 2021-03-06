;;; vector.lisp --- Simple 2d and 3d vector and matrix math library.
;;;                 _
;;; __   _____  ___| |_ ___  _ __
;;; \ \ / / _ \/ __| __/ _ \| '__|
;;;  \ V /  __/ (__| || (_) | |
;;;   \_/ \___|\___|\__\___/|_|
;;;
;;; Copyright (C) 2007 Ole Arndt <ole@sugarshark.com>
;;;

(in-package :vecmath)


;;;; ---------------------------------------------------------------------------
;;;; * Definition of vector types


(defvector vec2
    (x y))

(defvector vec3
    (x y z))

(defvector vec4
    (x y z w))

;;;; ----------------------------------------------------------------------------
;;;; * Euler Angles type
;;;;
;;;; The three angles are specified in radians an are applied in the order
;;;; they appear in the slot list.

(defvector euler-angles
    (yaw pitch roll))

;;;; ----------------------------------------------------------------------------
;;;; * Axis/Angle type
;;;;
;;;; The axis should always be normalized, the angle is in radians.

(defvector axis/angle
    (x y z angle))

;;;; ---------------------------------------------------------------------------
;;;; * Vector Constants
;;;

(defparameter +vec3-zero+ (vec3 +scalar-zero+ +scalar-zero+ +scalar-zero+))
(defparameter +vec3-x-axis+ (vec3 +scalar-one+ +scalar-zero+ +scalar-zero+))
(defparameter +vec3-y-axis+ (vec3 +scalar-zero+ +scalar-one+ +scalar-zero+))
(defparameter +vec3-z-axis+ (vec3 +scalar-zero+ +scalar-zero+ +scalar-one+))

;;;; ----------------------------------------------------------------------------
;;;; * Constructors and Converters

(defvfun vec3<-vec2 ((v vec2) &optional ((z 0.0) scalar)) vec3
  "Convert a 2 dimensional vector to 3 dimensional vector."
  (values v.x v.y z))

(defvfun vec4<-vec3 ((v vec3) &optional ((w 1.0) scalar)) vec4
  "Convert a 3 dimensional vector to a homogenous vector."
  (values v.x v.y v.z w))

;;;; ---------------------------------------------------------------------------
;;;; * Functions for vectors of arbitrary length.
;;;

(defun equal? (x y &key (epsilon +scalar-epsilon+))
  (declare (type (or null scalar) epsilon))
  (let ((test (if (null epsilon)
                  (lambda (a b)
                    (declare (type scalar a b))
                    (= a b))
                  (lambda (a b)
                    (declare (type scalar a b))
                    (<= (abs (- a b)) epsilon)))))
    (typecase x
      (scalar (funcall test x y))
      (vec (let ((length (array-dimension x 0)))
             (and (= length (array-dimension y 0))
                  (dotimes (i length t)
                    (declare (type fixnum i))
                    (unless (funcall test
                                     (row-major-aref x i)
                                     (row-major-aref y i))
                      (return nil))))))
      (t (equalp x y)))))

(declaim (inline vec-copy))
(defun vec-copy (a &optional b)
  (if b
      (loop :for val across a
            :for i = 0 then (1+ i)
            :do (setf (row-major-aref b i) val)
            :finally (return b))
      (copy-seq a)))

(declaim (inline vec-clone-empty))
(defun vec-clone-empty (v)
  "Create a vector with the same length as the given one, but
with all elements initialized to zero."
  (make-sequence (type-of v) (length v)
                 :initial-element (coerce 0 (array-element-type v))))

(declaim (inline vec-ensure-store))
(defun vec-ensure-store (template &optional store)
  (declare (type vec template) (type (or null vec) store))
  (or store (vec-clone-empty template)))

(declaim (inline vec-ensure-copy))
(defun vec-ensure-copy (template store)
  (declare (type vec template) (type (or null vec) store))
  (the vec (or store (copy-seq template))))


;;;; ----------------------------------------------------------------------------
;;;; * Swizzling
;;;

(declaim (inline %swizzle-idx)
         (ftype (function (character) fixnum) %swizzle-idx))

(defun %swizzle-idx (char)
  (ecase (char-upcase char)
    ((#\X #\R #\S) 0)
    ((#\Y #\G #\T) 1)
    ((#\Z #\B #\P) 2)
    ((#\W #\A #\Q) 3)))

(defun %swizzle-indices (spec)
  (let ((subscripts (string spec)))
    (map 'list #'%swizzle-idx subscripts)))

(defmacro swizzle* (vec spec)
  (let ((v (gensym "V")))
    `(let ((,v ,vec))
       (values ,@(loop :for n :in (%swizzle-indices spec)
                       :collect `(aref ,v ,n))))))

(declaim (inline swizzle)
         (ftype (function (vec (or string symbol)) (or scalar vec)) swizzle))

(defun swizzle (vec spec)
  "Address vector elements by names."
  (let* ((spec (string spec))
         (len (length spec)))
    (if (= 1 len)
        (aref vec (%swizzle-idx (aref spec 0)))
        (loop :with result := (make-vec :size len)
              :for i :from 0 :below len
              :do (setf (aref result i)
                        (aref vec (%swizzle-idx (aref spec i))))
              :finally (return result)))))

(define-compiler-macro swizzle (&whole form vec spec)
  (if (constantp spec)
      (let* ((v (gensym "V"))
             (spec (string spec))
             (indices (%swizzle-indices spec))
             (len (length indices)))
        (if (= 1 len)
            `(aref ,vec ,(%swizzle-idx (aref spec 0)))
            `(let ((,v ,vec))
               (,(case len
                   (2 'vec2)
                   (3 'vec3)
                   (4 'vec4)
                   (t 'vec))
                ,@(loop :for n :in indices
                        :collect `(aref ,v ,n))))))
      form))

(defsetf swizzle (vec spec) (src-vec)
  (let ((v (gensym))
        (src (gensym)))
    `(let ((,v ,vec)
           (,src ,src-vec))
       (etypecase ,src
         (scalar
          (loop :for n :in ',@(list (%swizzle-indices spec))
                :do (setf (aref ,v n) ,src)
                :finally (return ,v)))
         (vec
          (loop :for n :in ',@(list (%swizzle-indices spec))
                :for m :across ,src
                :do (setf (aref ,v n) m)
                :finally (return ,v)))))))

(defmacro with-swizzle ((&rest vectors) &body body)
  (let* ((vecs (mapcar #'ensure-car vectors))
         (bindings (mapcar (lambda (v)
                             (if (atom v)
                                 (list v v)
                                 v))
                           vectors))
         (valid-names (mapcar (lambda (sym)
                                (concatenate 'string (string sym) "."))
                              vecs))
         (accessors (remove-duplicates
                     (remove-if-not (lambda (sym)
                                      (let ((cand (string sym)))
                                        (some (lambda (name)
                                                (and (> (length cand)
                                                        (length name))
                                                     (string= name
                                                              (subseq cand 0 (length name)))))
                                              valid-names)))
                                    (remove-if-not 'symbolp (alexandria:flatten body))))))
    `(let ,bindings
       (symbol-macrolet ,(mapcar (lambda (acc)
                                   (let* ((name (string acc))
                                          (vec (symbolicate (subseq name 0 (position #\. name))))
                                          (spec (subseq name (1+ (position #\. name)))))
                                     `(,acc
                                       (swizzle ,vec ,spec))))
                          accessors)
         ,@body))))


;;;; ----------------------------------------------------------------------------
;;;; * Vector Multiplication
;;;

(defvfun vec-scale ((v vec) s &optional (store vec)) vec
  "Multiplicate a vector with a scalar."
  (loop :with ret = (vec-ensure-store v store)
        :for val across v
        :for i = 0 then (1+ i)
        :do (setf (row-major-aref ret i) (* val s))
        :finally (return ret)))

(defvfun vec2-scale ((v vec2) s) vec2
  "Multiplicate a two dimensional vector with a scalar."
  (values (* v.x s) (* v.y s)))

(defvfun vec3-scale ((v vec3) s) vec3
  "Multiplicate a three dimensional vector with a scalar."
  (values (* v.x s) (* v.y s) (* v.z s)))

(defvfun vec4-scale ((v vec4) s) vec4
  "Multiplicate a vector in homogenous space with a scalar."
  (values (* v.x s) (* v.y s) (* v.z s) (* v.w s)))


;;;; ----------------------------------------------------------------------------
;;;; * Vector Division
;;;

(defvfun vec-div ((v vec) s &optional (store vec)) vec
  "Divide a vector through a scalar."
  (vec-scale v (invert s) store))

(defvfun vec2-div ((v vec2) s) vec2
  "Divide a two dimensional vector through a scalar."
  (vec2-scale* v.x v.y (invert s)))

(defvfun vec3-div ((v vec3) s) vec3
  "Divide a three dimensional vector through a scalar."
  (vec3-scale* v.x v.y v.z (invert s)))

(defvfun vec4-div ((v vec4) s) vec4
  "Divide a vector in homogenous space through a scalar."
  (vec4-scale* v.x v.y v.z v.w (invert s)))


;;;; ----------------------------------------------------------------------------
;;;; * Vector Negation
;;;

(defvfun vec-invert ((v vec) &optional (store vec)) vec
  "Multiplicate a vector's elements with -1."
  (vec-scale v +scalar-minus-one+ store))

(defvfun vec2-invert ((v vec2)) vec2
  "Invert the vector, multiply all elements with -1. "
  (vec2-scale* v.x v.y +scalar-minus-one+))

(defvfun vec3-invert ((v vec3)) vec3
  "Invert the vector, multiply all elements with -1. "
  (vec3-scale* v.x v.y v.z +scalar-minus-one+))

(defvfun vec4-invert ((v vec4)) vec4
  "Invert the vector, multiply all elements with -1. "
  (vec4-scale* v.x v.y v.z v.w +scalar-minus-one+))

;;;; ----------------------------------------------------------------------------
;;;; * Vector Addition and Substraction
;;;

(defvfun vec-add ((a vec) (b vec) &optional (store vec)) vec
  "Add two vectors component wise."
  (map-into (vec-ensure-store a store) #'+ a b))

(defvfun vec2-add ((a vec2) (b vec2)) vec2
  "Add two vectors component wise."
  (values (+ a.x b.x) (+ a.y b.y)))

(defvfun vec3-add ((a vec3) (b vec3)) vec3
  "Add two vectors component wise."
  (values (+ a.x b.x) (+ a.y b.y) (+ a.z b.z)))

(defvfun vec4-add ((a vec4) (b vec4)) vec4
  "Add two vectors component wise."
  (values (+ a.x b.x) (+ a.y b.y) (+ a.z b.z) (+ a.w b.w)))


(defvfun vec-sub ((a vec) (b vec) &optional (store vec)) vec
  "Substract two vectors component wise."
  (map-into (vec-ensure-store a store) #'- a b))

(defvfun vec2-sub ((a vec2) (b vec2)) vec2
  "Substract two vectors component wise."
  (values (- a.x b.x) (- a.y b.y)))

(defvfun vec3-sub ((a vec3) (b vec3)) vec3
  "Substract two vectors component wise."
  (values (- a.x b.x) (- a.y b.y) (- a.z b.z)))

(defvfun vec4-sub ((a vec4) (b vec4)) vec4
  "Substract two vectors component wise."
  (values (- a.x b.x) (- a.y b.y) (- a.z b.z) (- a.w b.w)))


;;;; ----------------------------------------------------------------------------
;;;; * Vector Dot Product
;;;

(defvfun vec-dot ((a vec) (b vec)) scalar
  "Returns the dot product of the two vectors"
  (:scalar-args-version nil)
  (loop
     for m of-type scalar across a
     for n of-type scalar across b
     sum (* m n) into r of-type scalar
     finally (return r)))

(defvfun vec2-dot ((a vec2) (b vec2)) scalar
  "Returns the dot product of the two vectors"
  (+ (* a.x b.x) (* a.y b.y)))

(defvfun vec3-dot ((a vec3) (b vec3)) scalar
  "Returns the dot product of the two vectors"
  (+ (* a.x b.x) (* a.y b.y) (* a.z b.z)))

(defvfun vec4-dot ((a vec4) (b vec4)) scalar
  "Returns the dot product of the two vectors"
  (+ (* a.x b.x) (* a.y b.y) (* a.z b.z) (* a.w b.w)))


;;;; ----------------------------------------------------------------------------
;;;; * Vector Cross Product
;;;

(defvfun vec2-cross ((a vec2) (b vec2)) vec2
  "Returns the cross product of the two vectors"
  (values (- (* a.y b.x) (* a.x b.y))
          (- (* a.x b.y) (* a.y b.x))))

(defvfun vec3-cross ((a vec3) (b vec3)) vec3
  "Returns the cross product of the two vectors"
  (values (- (* a.y b.z) (* a.z b.y))
          (- (* a.z b.x) (* a.x b.z))
          (- (* a.x b.y) (* a.y b.x))))

;;;; ----------------------------------------------------------------------------
;;;; * Vector Length
;;;

(defvfun vec-magnitude^2 ((v vec)) scalar
  "Returns the squared length of the vector."
  (:scalar-args-version nil)
  (loop for a across v sum (square a)))

(defvfun vec2-magnitude^2 ((v vec2)) scalar
  "Returns the squared length of the vector."
  (+ (square v.x) (square v.y)))

(defvfun vec3-magnitude^2 ((v vec3)) scalar
  "Returns the squared length of the vector."
  (+ (square v.x) (square v.y) (square v.z)))

(defvfun vec4-magnitude^2 ((v vec4)) scalar
  "Returns the squared length of the vector."
  (+ (square v.x) (square v.y) (square v.z) (square v.w)))

(defvfun vec-magnitude ((v vec)) scalar
  "Returns the squared length of the vector."
  (:scalar-args-version nil)
  (sqrt (vec-magnitude^2 v)))

(defvfun vec2-magnitude ((v vec2)) scalar
  "Returns the length of the vector."
  (sqrt (vec2-magnitude^2* v.x v.y)))

(defvfun vec3-magnitude ((v vec3)) scalar
  "Returns the length of the vector."
  (sqrt (vec3-magnitude^2* v.x v.y v.z)))

(defvfun vec4-magnitude ((v vec4)) scalar
  "Returns the length of the vector."
  (sqrt (vec4-magnitude^2* v.x v.y v.z v.w)))


;;;; ---------------------------------------------------------------------------
;;;; * The Distance Between two Vectors
;;;

(defvfun vec-distance^2 ((a vec) (b vec)) scalar
  "Returns the squared distance between two vectors."
  (:scalar-args-version nil)
  (loop for m of-type scalar across a for n across b sum (square (- m n))))

(defvfun vec2-distance^2 ((a vec2) (b vec2)) scalar
  "Returns the squared distance between two vectors."
  (+ (square (- a.x b.x)) (square (- a.y b.y))))

(defvfun vec3-distance^2 ((a vec3) (b vec3)) scalar
  "Returns the squared distance between two vectors."
  (+ (square (- a.x b.x)) (square (- a.y b.y)) (square (- a.z b.z))))

(defvfun vec4-distance^2 ((a vec4) (b vec4)) scalar
  "Returns the squared distance between two vectors."
  (+ (square (- a.x b.x)) (square (- a.y b.y))
     (square (- a.z b.z)) (square (- a.w b.w))))

(defvfun vec-distance ((a vec) (b vec)) scalar
  "Returns the distance between two vectors."
  (:scalar-args-version nil)
  (sqrt (vec-distance^2 a b)))

(defvfun vec2-distance ((a vec2) (b vec2)) scalar
  "Returns the distance between two vectors."
  (sqrt (vec2-distance^2* a.x a.y b.x b.y)))

(defvfun vec3-distance ((a vec3) (b vec3)) scalar
  "Returns the distance between two vectors."
  (sqrt (vec3-distance^2* a.x a.y a.z b.x b.y b.z)))

(defvfun vec4-distance ((a vec4) (b vec4)) scalar
  "Returns the distance between two vectors."
  (sqrt (vec4-distance^2* a.x a.y a.z a.w b.x b.y b.z b.w)))

;;;; ---------------------------------------------------------------------------
;;;; * Vector Scaling
;;;

(defvfun vec-rescale ((v vec) len &optional (store vec)) vec
  "Scale the vector to a new magnitude."
  (vec-scale v (/ len (vec-magnitude v)) store))

(defvfun vec2-rescale ((v vec2) len) vec2
  "Scale the vector to a new magnitude."
  (vec2-scale* v.x v.y (/ len (vec2-magnitude* v.x v.y))))

(defvfun vec3-rescale ((v vec3) len) vec3
  "Scale the vector to a new magnitude."
  (vec3-scale* v.x v.y v.z (/ len (vec3-magnitude* v.x v.y v.z))))

(defvfun vec4-rescale ((v vec4) len) vec4
  "Scale the vector to a new magnitude."
  (vec4-scale* v.x v.y v.z v.w (/ len (vec4-magnitude* v.x v.y v.z v.w))))


;;;; ---------------------------------------------------------------------------
;;;; * Vector truncation
;;;

(defvfun vec-truncate ((v vec) len &optional (store vec)) vec
  "Truncate the vector to a maximal magnitude."
  (let ((ms (vec-magnitude^2 v)))
    (if (> ms (square len))
        (vec-scale v (/ len (sqrt ms)) store)
        (vec-copy v store))))

(defvfun vec2-truncate ((v vec2) len) vec2
  "Truncate the vector to a maximal magnitude."
  (let ((ms (vec2-magnitude^2* v.x v.y)))
    (if (> ms (square len))
        (vec2-scale* v.x v.y (/ len (sqrt ms)))
        (values v.x v.y))))

(defvfun vec3-truncate ((v vec3) len) vec3
  "Truncate the vector to a maximal magnitude."
  (let ((ms (vec3-magnitude^2* v.x v.y v.z)))
    (if (> ms (square len))
        (vec3-scale* v.x v.y v.z (/ len (sqrt ms)))
        (values v.x v.y v.z))))

(defvfun vec4-truncate ((v vec4) len) vec4
  "Truncate the vector to a maximal magnitude."
  (let ((ms (vec4-magnitude^2* v.x v.y v.z v.w)))
    (if (> ms (square len))
        (vec4-scale* v.x v.y v.z v.w (/ len (sqrt ms)))
        (values v.x v.y v.z v.w))))


;;;; ---------------------------------------------------------------------------
;;;; * Vector Normalization
;;;

(defvfun vec-normalize ((v vec) &optional (store vec)) vec
  "Normalize the vector, scale to magnitude one."
  (vec-scale v (inverse-sqrt (vec-magnitude^2 v)) store))

(defvfun vec2-normalize ((v vec2)) vec2
  "Normalize the vector, scale to magnitude one."
  (vec2-scale* v.x v.y
               (inverse-sqrt (vec2-magnitude^2* v.x v.y))))

(defvfun vec3-normalize ((v vec3)) vec3
  "Normalize the vector, scale to magnitude one."
  (vec3-scale* v.x v.y v.z
               (inverse-sqrt (vec3-magnitude^2* v.x v.y v.z))))

(defvfun vec4-normalize ((v vec4)) vec4
  "Normalize the vector, scale to magnitude one."
  (vec4-scale* v.x v.y v.z v.w
               (inverse-sqrt (vec4-magnitude^2* v.x v.y v.z v.w))))


;;;; ---------------------------------------------------------------------------
;;;; * Angle between two Vectors
;;;

(defvfun vec-angle ((a vec) (b vec)) scalar
  (:scalar-args-version nil)
  (acos (/ (vec-dot a b) (vec-magnitude a) (vec-magnitude b))))

(defvfun vec2-angle ((a vec2) (b vec2)) scalar
  "Return the angle between two vectors in radians."
  (abs (atan (- (* a.x b.y) (* a.y b.x))
             (vec2-dot* a.x a.y b.x b.y))))

(defvfun vec3-angle ((a vec3) (b vec3)) scalar
  "Return the angle between two vectors in radians."
  (abs (atan (multiple-value-call #'vec3-magnitude*
               (vec3-cross* a.x a.y a.z b.x b.y b.z))
             (vec3-dot* a.x a.y a.z b.x b.y b.z))))


;;;; ---------------------------------------------------------------------------
;;;; * Vector Interpolation
;;;

(defvfun vec-interpolate ((a vec) (b vec) alpha &optional (store vec)) vec
  "Interpolate a vector from vector `a' to vector `b',
depending on the interpolation factor alpha."
  (:scalar-args-version nil)
  (let ((beta (invert alpha)))
    (map-into (vec-ensure-store a store)
              #'(lambda (m n)
                  (+ (* m beta)
                     (* n alpha)))
              a b)))

(defvfun vec2-interpolate ((a vec2) (b vec2) alpha) vec2
  "Interpolate a vector from vector `a' to vector `b',
depending on the interpolation factor alpha."
  (let ((beta (invert alpha)))
    (values (+ (* a.x beta) (* b.x alpha))
            (+ (* a.y beta) (* b.y alpha)))))

(defvfun vec3-interpolate ((a vec3) (b vec3) alpha) vec3
  "Interpolate a vector from vector `a' to vector `b',
depending on the interpolation factor alpha."
  (let ((beta (invert alpha)))
    (values (+ (* a.x beta) (* b.x alpha))
            (+ (* a.y beta) (* b.y alpha))
            (+ (* a.z beta) (* b.z alpha)))))

(defvfun vec4-interpolate ((a vec4) (b vec4) alpha) vec4
  "Interpolate a vector from vector `a' to vector `b',
depending on the interpolation factor alpha."
  (let ((beta (invert alpha)))
    (values (+ (* a.x beta) (* b.x alpha))
            (+ (* a.y beta) (* b.y alpha))
            (+ (* a.z beta) (* b.z alpha))
            (+ (* a.w beta) (* b.w alpha)))))


;;; vector.lisp ends here
