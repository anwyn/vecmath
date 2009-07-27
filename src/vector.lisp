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

;; (declaim (optimize (speed 3) (space 0) (debug 0) (safety 0)))

;;;; * Vector Types
;;;

(defmacro defvector (type supers slots &body options)
  (let* ((element-type (if supers
                           (or (get (car supers) 'element-type)
                               (error "Can not get element-type of super class ~a.
Are you are sure it has been defined with `defvector'?."
                                      (string (car supers))))
                           (or (cadr (assoc :element-type options)) 'scalar)))
         (super-slots (and supers
                           (or (get (car supers) 'slot-list)
                               (error "Can not get slots of super class ~a.
Are you are sure it has been defined with `defvector'?."
                                      (string (car supers))))))
         (effective-slots (append super-slots slots))
         (simple-slots (mapcar #'(lambda (s)
                                   (if (consp s)
                                       (car s)
                                       s)) effective-slots))
         (unique-slots (mapcar #'(lambda (a) (gensym (string a))) simple-slots))
         (vec (gensym "V"))
         (vec->values (intern (concatenate 'string (string type) (string '#:->values))))
         (vec<-values (intern (concatenate 'string (string type) (string '#:<-values))))
         (vec<-values! (intern (concatenate 'string (string type) (string '#:<-values) "!")))
         (cloner (intern (concatenate 'string (string type) (string '#:-clone))))
         (copier (intern (concatenate 'string (string type) (string '#:-copy)))))
    `(progn
       (deftype ,type () '(simple-array ,element-type (,(length effective-slots))))
       (eval-when (:compile-toplevel :load-toplevel :execute)
         (setf (get ',type 'slot-list) ',effective-slots)
         (setf (get ',type 'element-type) ',element-type))
       (defmacro ,vec->values (v)
         (list 'with-vector-elements ',unique-slots v
               '(values ,@unique-slots)))
       (defmacro ,vec<-values (form)
         (list 'multiple-value-bind ',unique-slots form
               '(,type ,@unique-slots)))
       (defmacro ,vec<-values! (v form)
         (list 'let (list (list ',vec (list 'or v (list ',type))))
               (list 'with-vector-elements ',unique-slots ',vec
                     (list 'multiple-value-setq ',unique-slots form) ',vec)))
       (declaim (inline ,cloner ,copier)
                (ftype (function (,type) ,type) ,cloner)
                (ftype (function (,type &optional (or null ,type)) ,type) ,copier))
       (defun ,cloner (v)
         "Clone vector V."
         (declare (type ,type v) ,*optimization*)
         (with-vector-elements ,unique-slots v (,type ,@unique-slots)))
       (defun ,copier (a &optional b)
         "Copy vector A into vector B. When B is nil, an new vector will be made.
Returns B or the new vector."
         (declare (type ,type a) (type (or null ,type) b) ,*optimization*)
         (,vec<-values! b (,vec->values a)))
       (declaim (inline ,type))
       (defstruct (,type ,@(when supers (list (list ':include (car supers))))
                         (:type (vector ,element-type))
                         (:copier nil)
                         (:constructor)
                         (:constructor ,type ,(cons '&optional simple-slots)))
         ,@(mapcar (lambda (s)
                     (if (consp s)
                         (list (car s) (cadr s) ':type element-type)
                         (list s (coerce 0 element-type) ':type element-type))) slots)))))



;;;; * Macro Definitions
;;;

(defmacro with-vector-elements (vars vector &body body)
  (let ((vec (gensym))
        (index-counter 0))
    (declare (type fixnum index-counter))
    `(let ((,vec ,vector))
       (declare (type (simple-array scalar) ,vec) (ignorable ,vec))
       (symbol-macrolet ,(mapcar (lambda (var-entry)
                                   (let ((var-name
                                          (if (symbolp var-entry)
                                              var-entry
                                              (car var-entry)))
                                         (index
                                          (if (symbolp var-entry)
                                              index-counter
                                              (cadr var-entry))))
                                     (declare (type fixnum index))
                                     (setf index-counter (1+ index))
                                     `(,var-name
                                       (row-major-aref ,vec ,index))))
                                 vars)
         ,@body))))

(eval-when (:compile-toplevel :load-toplevel :execute)
  (defun %make-with-vectors (vectors body)
    (if (null vectors)
        `(progn ,@body)
        (let ((vars (car vectors))
              (vec (cadr vectors))
              (rest (cddr vectors)))
          `(with-vector-elements ,vars ,vec
             ,(%make-with-vectors rest body))))))

(defmacro with-vectors (vectors &body body)
  (%make-with-vectors vectors body))

(eval-when (:compile-toplevel :load-toplevel :execute)
  ;; generate vector manipulation functions
  (defun %make-vec-funcs (type nname args options body)
    (let* ((return-type (or (second (assoc :return-type options)) type))
           (returning-vector (not (second (assoc :returning-scalar options))))
           (destructive-version-p (and returning-vector
                                       (not (second (assoc :omit-destructive-version options)))))
           (doc (second (assoc :documentation options)))
           (inline-p (not (eq t (second (assoc :no-inline options)))))
           (vname (intern (concatenate 'string (string nname) "*")))
           (dname (intern (concatenate 'string (string nname) "!")))
           (vec<-values (intern (concatenate 'string (string return-type)
                                             (string '#:<-values))))
           (vec<-values! (intern (concatenate 'string (string return-type)
                                              (string '#:<-values) "!")))
           (scalars-only (remove-if #'consp args))
           (vectors-only (remove-if-not #'consp args))
           (element-type (or (get type 'element-type)
                             (error "Can not get element-type of vector ~a.
Are you are sure it has been defined with `defvector'?." (string type))))
           (scalar-args
            (mapcan #'(lambda (a)
                        (if (consp a) (copy-tree (car a)) (list a))) args))
           (vectors-and-scalars
            (mapcar #'(lambda (a)
                        (if (consp a) (cadr a) a)) args)))
      (append '(progn)

              ;; define the multiple values version, always inline
              `((declaim (inline ,vname)
                         ;; (ftype (function ,(mapcar (lambda (v)
                         ;;                                                      (declare (ignore v))
                         ;;                                                      'scalar)
                         ;;                                                    scalar-args)
                         ;;                                           (values ,@(if returning-vector
                         ;;                                                         (mapcar (lambda (v)
                         ;;                                                                   (declare (ignore v))
                         ;;                                                                   'scalar)
                         ;;                                                                 (first args))
                         ;;                                                         '(scalar))))
                         ;;                                 ,vname)
                         ))
              `((defun ,vname ,scalar-args
                  ,(concatenate 'string doc "
Multiple values version. Takes individual vector components as arguments
and returns the result as multiple values.")
                  (declare (type ,element-type ,@scalar-args) ,*optimization*)
                  ,@body))
              
              ;; define the normal version
              (when inline-p
                `((declaim (inline ,nname))))

;;;               `((declaim (ftype (function ,(append
;;;                                             (mapcar (lambda (v)
;;;                                                       (if (consp v)
;;;                                                           (list 'vec (length (car v)) 'scalar)
;;;                                                           'scalar))
;;;                                                     args)
;;;                                             (when returning-vector
;;;                                               `(&optional ,return-type)))
;;;                                           ,(if returning-vector return-type 'scalar))
;;;                                 ,nname)))
              
              `((defun ,nname ,(append vectors-and-scalars (when returning-vector
                                                             `(&optional store)))
                  ,(concatenate 'string doc
                                (when returning-vector "
The result is stored in the optional third parameter `store' if provided.
If `store' is nil, a new vector will be created with the result values."))
                  (declare (type ,type ,@(mapcar #'cadr vectors-only))
                           ,@(when scalars-only
                                   `((type ,element-type ,@scalars-only)))
                           ,@(when returning-vector
                                   `((type (or null ,return-type) store)))
                           ,*optimization*)
                  (with-vectors ,(mapcan #'copy-tree vectors-only)
                    ,(if returning-vector
                         `(if store
                              (,vec<-values! store (,vname ,@scalar-args))
                              (,vec<-values (,vname ,@scalar-args)))
                         `(the ,element-type (,vname ,@scalar-args))))))

              ;; destructive version
              (when (and destructive-version-p inline-p)
                `((declaim (inline ,dname))))

;;;               (when destructive-version-p
;;;                 `((declaim (ftype (function , (mapcar (lambda (v)
;;;                                                         (if (consp v)
;;;                                                             (list 'vec (length (car v)) 'scalar)
;;;                                                             'scalar))
;;;                                                       args)
              
;;;                                               ,return-type)
;;;                                   ,dname))))
              
              (when destructive-version-p
                `((defun ,dname ,(append vectors-and-scalars)
                    ,(concatenate 'string doc "
Will override the first parameter with the result.")
                    (with-vectors ,(mapcan #'copy-tree vectors-only)
                      (,vec<-values! ,(second (first vectors-only))
                                     (,vname ,@scalar-args))))))))))

(defmacro defvecfun (function-name args options &body body)
  (let* ((name (string function-name))
         (type (or (second (assoc :type options))
                   (intern (subseq name 0 (position #\- name))))))
    (%make-vec-funcs type function-name args options body)))


;;;; * Types
;;;

;;; A vector of arbitrary length with element type scalar
(deftype vec (&optional len (element-type 'scalar))
  `(simple-array ,element-type (,len)))

(defvector vec2 ()
    (x y))

(defvector vec3 (vec2)
    (z))

(defvector vec4 (vec3)
    ((w 1.0)))

;;;; * Vector Constants
;;;

(defparameter +vec3-zero+ (vec3 +scalar-zero+ +scalar-zero+ +scalar-zero+))
(defparameter +vec3-x-axis+ (vec3 +scalar-one+ +scalar-zero+ +scalar-zero+))
(defparameter +vec3-y-axis+ (vec3 +scalar-zero+ +scalar-one+ +scalar-zero+))
(defparameter +vec3-z-axis+ (vec3 +scalar-zero+ +scalar-zero+ +scalar-one+))

;;;; * Functions for vectors of arbitrary length.
;;;

(defun vec-equal (x y &optional epsilon)
  (declare (type (simple-array scalar) x y)
           (type (or null scalar) epsilon))
  (let ((length (length x)))
    (and (= length (length y))
         (if (null epsilon)
             (dotimes (i length t)
               (let ((a (aref x i))
                     (b (aref y i)))
                 (unless (= a b)
                   (return nil))))
             (dotimes (i length t)
               (let ((a (aref x i))
                     (b (aref y i)))
                 (unless (<= (abs (- a b)) epsilon)
                   (return nil))))))))

(declaim (inline vec-copy))
(defun vec-copy (a &optional b)
  (if b
      (map-into b #'identity a)
      (copy-seq a)))

(declaim (inline vec-clone-empty))
(defun vec-clone-empty (v)
  "Create a vector with the same length as the given one, but 
with all elements initialized to zero."
  (make-sequence (type-of v) (length v)
                 :initial-element (coerce 0 (array-element-type v))))

(declaim (inline vec-ensure-store))
(defun vec-ensure-store (template store)
  (declare (type vec template) (type (or null vec) store))
  (or store (vec-clone-empty template)))

(declaim (inline vec-ensure-copy))
(defun vec-ensure-copy (template store)
  (declare (type vec template) (type (or null vec) store))
  (the vec (or store (vec-copy template))))

;;;; * Vector Multiplication
;;;

(defun vec-scale (v s &optional store)
  "Multiplicate a vector with a scalar."
  (declare (type vec v) (type (or null vec) store))
  (map-into (vec-ensure-store v store) #'(lambda (a) (* a s)) v))

(defvecfun vec2-scale (((x y) v) s)
    ((:documentation "Multiplicate a two dimensional vector with a scalar."))
  (values (* x s) (* y s)))

(defvecfun vec3-scale (((x y z) v) s)
    ((:documentation "Multiplicate a three dimensional vector with a scalar."))
  (values (* x s) (* y s) (* z s)))

(defvecfun vec4-scale (((x y z w) v) s)
    ((:documentation "Multiplicate a vector in homogenous space with a scalar."))
  (values (* x s) (* y s) (* z s) (* w s)))


 ;;;; * Vector Division
;;;

(defun vec-div (v s &optional store)
  "Divide a vector through a scalar."
  (vec-scale v (invert s) store))

(defvecfun vec2-div (((x y) v) s)
    ((:documentation "Divide a two dimensional vector through a scalar."))
  (vec2-scale* x y (invert s)))

(defvecfun vec3-div (((x y z) v) s)
    ((:documentation "Divide a three dimensional vector through a scalar."))
  (vec3-scale* x y z (invert s)))

(defvecfun vec4-div (((x y z w) v) s)
    ((:documentation "Divide a vector in homogenous space through a scalar."))
  (vec4-scale* x y z w (invert s)))



;;;; * Vector Negation
;;;

(defun vec-invert (v &optional store)
  "Multiplicate a vector's elements with -1."
  (vec-scale v +scalar-minus-one+ store))

(defvecfun vec2-invert (((x y) v))
    ((:documentation "Invert the vector, multiply all elements with -1. "))
  (vec2-scale* x y +scalar-minus-one+))

(defvecfun vec3-invert (((x y z) v))
    ((:documentation "Invert the vector, multiply all elements with -1. "))
  (vec3-scale* x y z +scalar-minus-one+))

(defvecfun vec4-invert (((x y z w) v))
    ((:documentation "Invert the vector, multiply all elements with -1. "))
  (vec4-scale* x y z w +scalar-minus-one+))

;;;; * Vector Addition and Substraction
;;;

(defun vec-add (a b &optional store)
  "Add two vectors component wise."
  (declare (type vec a b) (type (or null vec) store))
  (map-into (vec-ensure-store a store) #'+ a b))

(defvecfun vec2-add (((ax ay) a) ((bx by) b))
    ((:documentation "Add two vectors component wise."))
  (values (+ ax bx) (+ ay by)))

(defvecfun vec3-add (((ax ay az) a) ((bx by bz) b))
    ((:documentation "Add two vectors component wise."))
  (values (+ ax bx) (+ ay by) (+ az bz)))

(defvecfun vec4-add (((ax ay az aw) a) ((bx by bz bw) b))
    ((:documentation "Add two vectors component wise."))
  (values (+ ax bx) (+ ay by) (+ az bz) (+ aw bw)))


(defun vec-sub (a b &optional store)
  "Substract two vectors component wise."
  (declare (type vec a b) (type (or null vec) store))
  (map-into (vec-ensure-store a store) #'- a b))

(defvecfun vec2-sub (((ax ay) a) ((bx by) b))
    ((:documentation "Substract two vectors component wise."))
  (values (+ ax bx) (+ ay by)))

(defvecfun vec3-sub (((ax ay az) a) ((bx by bz) b))
    ((:documentation "Substract two vectors component wise."))
  (values (+ ax bx) (+ ay by) (+ az bz)))

(defvecfun vec4-sub (((ax ay az aw) a) ((bx by bz bw) b))
    ((:documentation "Substract two vectors component wise."))
  (values (+ ax bx) (+ ay by) (+ az bz) (+ aw bw)))


;;;; * Vector Dot Product
;;;

(declaim (inline vec-dot))
(defun vec-dot (a b)
  "Returns the dot product of the two vectors"
  (declare (type vec a b)
           (optimize (speed 3)))
  (loop
     for m of-type scalar across a
     for n of-type scalar across b
     sum (* m n) into r of-type scalar
     finally (return r)))

(defvecfun vec2-dot (((ax ay) a) ((bx by) b))
    ((:returning-scalar t)
     (:documentation "Returns the dot product of the two vectors"))
  (+ (* ax bx) (* ay by)))

(defvecfun vec3-dot (((ax ay az) a) ((bx by bz) b))
    ((:returning-scalar t)
     (:documentation "Returns the dot product of the two vectors"))
  (+ (* ax bx) (* ay by) (* az bz)))

(defvecfun vec4-dot (((ax ay az aw) a) ((bx by bz bw) b))
    ((:returning-scalar t)
     (:documentation "Returns the dot product of the two vectors"))
  (+ (* ax bx) (* ay by) (* az bz) (* aw bw)))


;;;; * Vector Cross Product
;;;

(defvecfun vec2-cross (((ax ay) a) ((bx by) b))
    ((:documentation "Returns the cross product of the two vectors"))
  (values (- (* ay bx) (* ax by))
          (- (* ax by) (* ay bx))))

(defvecfun vec3-cross (((ax ay az) a) ((bx by bz) b))
    ((:documentation "Returns the cross product of the two vectors"))
  (values (- (* ay bz) (* az by))
          (- (* az bx) (* ax bz))
          (- (* ax by) (* ay bx))))


;;;; * Vector Length
;;;

(defun vec-magnitude^2 (v)
  "Returns the squared length of the vector."
  (declare (type vec v))
  (loop for a across v sum (square a)))

(defvecfun vec2-magnitude^2 (((x y) v))
    ((:returning-scalar t)
     (:documentation "Returns the squared length of the vector."))
  (+ (square x) (square y)))

(defvecfun vec3-magnitude^2 (((x y z) v))
    ((:returning-scalar t)
     (:documentation "Returns the squared length of the vector."))
  (+ (square x) (square y) (square z)))

(defvecfun vec4-magnitude^2 (((x y z w) v))
    ((:returning-scalar t)
     (:documentation "Returns the squared length of the vector."))
  (+ (square x) (square y) (square z) (square w)))

(defun vec-magnitude (v)
  "Returns the squared length of the vector."
  (sqrt (vec-magnitude^2 v)))

(defvecfun vec2-magnitude (((x y) v))
    ((:returning-scalar t)
     (:documentation "Returns the length of the vector."))
  (sqrt (vec2-magnitude^2* x y)))

(defvecfun vec3-magnitude (((x y z) v))
    ((:returning-scalar t)
     (:documentation "Returns the length of the vector."))
  (sqrt (vec3-magnitude^2* x y z)))

(defvecfun vec4-magnitude (((x y z w) v))
    ((:returning-scalar t)
     (:documentation "Returns the length of the vector."))
  (sqrt (vec4-magnitude^2* x y z w)))


;;;; * The Distance Between two Vectors
;;;

(defun vec-distance^2 (a b)
  "Returns the squared distance between two vectors."
  (declare (type vec a b))
  (loop for m across a for n across b sum (square (- m n))))

(defvecfun vec2-distance^2 (((ax ay) a) ((bx by) b))
    ((:returning-scalar t)
     (:documentation "Returns the squared distance between two vectors."))
  (+ (square (- ax bx)) (square (- ay by))))

(defvecfun vec3-distance^2 (((ax ay az) a) ((bx by bz) b))
    ((:returning-scalar t)
     (:documentation "Returns the squared distance between two vectors."))
  (+ (square (- ax bx)) (square (- ay by)) (square (- az bz))))

(defvecfun vec4-distance^2 (((ax ay az aw) a) ((bx by bz bw) b))
    ((:returning-scalar t)
     (:documentation "Returns the squared distance between two vectors."))
  (+ (square (- ax bx)) (square (- ay by))
     (square (- az bz)) (square (- aw bw))))

(defun vec-distance (a b)
  "Returns the distance between two vectors."
  (declare (type vec a b))
  (sqrt (vec-distance^2 a b)))

(defvecfun vec2-distance (((ax ay) a) ((bx by) b))
    ((:returning-scalar t)
     (:documentation "Returns the distance between two vectors."))
  (sqrt (vec2-distance^2* ax ay bx by)))

(defvecfun vec3-distance (((ax ay az) a) ((bx by bz) b))
    ((:returning-scalar t)
     (:documentation "Returns the distance between two vectors."))
  (sqrt (vec3-distance^2* ax ay az bx by bz)))

(defvecfun vec4-distance (((ax ay az aw) a) ((bx by bz bw) b))
    ((:returning-scalar t)
     (:documentation "Returns the distance between two vectors."))
  (sqrt (vec4-distance^2* ax ay az aw bx by bz bw)))

;;;; * Vector Scaling
;;;


(defun vec-rescale (v len &optional store)
  "Scale the vector to a new magnitude."
  (vec-scale v (/ len (vec-magnitude v)) store))

(defvecfun vec2-rescale (((x y) v) len)
    ((:documentation "Scale the vector to a new magnitude."))
  (vec2-scale* x y (/ len (vec2-magnitude* x y))))

(defvecfun vec3-rescale (((x y z) v) len)
    ((:documentation "Scale the vector to a new magnitude."))
  (vec3-scale* x y z (/ len (vec3-magnitude* x y z))))

(defvecfun vec4-rescale (((x y z w) v) len)
    ((:documentation "Scale the vector to a new magnitude."))
  (vec4-scale* x y z w (/ len (vec4-magnitude* x y z w))))


;;;; * Vector truncation
;;;


(defun vec-truncate (v len &optional store)
  "Truncate the vector to a maximal magnitude."
  (let ((ms (vec-magnitude^2 v)))
    (if (> ms (square len))
        (vec-scale v (/ len (sqrt ms)) store)
        (vec-copy v store))))

(defvecfun vec2-truncate (((x y) v) len)
    ((:documentation "Truncate the vector to a maximal magnitude."))
  (let ((ms (vec2-magnitude^2* x y)))
    (if (> ms (square len))
        (vec2-scale* x y (/ len (sqrt ms)))
        (values x y))))

(defvecfun vec3-truncate (((x y z) v) len)
    ((:documentation "Truncate the vector to a maximal magnitude."))
  (let ((ms (vec3-magnitude^2* x y z)))
    (if (> ms (square len))
        (vec3-scale* x y z (/ len (sqrt ms)))
        (values x y z))))

(defvecfun vec4-truncate (((x y z w) v) len)
    ((:documentation "Truncate the vector to a maximal magnitude."))
  (let ((ms (vec4-magnitude^2* x y z w)))
    (if (> ms (square len))
        (vec4-scale* x y z w (/ len (sqrt ms)))
        (values x y z w))))


;;;; * Vector Normalization
;;;

(defun vec-normalize (v &optional store)
  "Normalize the vector, scale to magnitude one."
  (vec-scale v (inverse-sqrt (vec-magnitude^2 v)) store))

(defvecfun vec2-normalize (((x y) v))
    ((:documentation "Normalize the vector, scale to magnitude one."))
  (vec2-scale* x y (inverse-sqrt (vec2-magnitude^2* x y ))))

(defvecfun vec3-normalize (((x y z) v))
    ((:documentation "Normalize the vector, scale to magnitude one."))
  (vec3-scale* x y z (inverse-sqrt (vec3-magnitude^2* x y z))))

(defvecfun vec4-normalize (((x y z w) v))
    ((:documentation "Normalize the vector, scale to magnitude one."))
  (vec4-scale* x y z w (inverse-sqrt (vec4-magnitude^2* x y z w))))


;;;; * Angle between two Vectors
;;;

(defun vec-angle (a b)
  (declare (type vec a b))
  (acos (/ (vec-dot a b) (vec-magnitude a) (vec-magnitude b))))

(defvecfun vec2-angle (((ax ay) a) ((bx by) b))
    ((:returning-scalar t)
     (:documentation "Return the angle between two vectors in radians."))
  (abs (atan (- (* ax by) (* ay bx))
             (vec2-dot* ax ay bx by))))

(defvecfun vec3-angle (((ax ay az) a) ((bx by bz) b))
    ((:returning-scalar t)
     (:documentation "Return the angle between two vectors in radians."))
  (multiple-value-bind (x y z)
      (vec3-cross* ax ay az bx by bz)
    (abs (atan (vec3-magnitude* x y z)
               (vec3-dot* ax ay az bx by bz)))))


;;;; * Vector Interpolation
;;;

(defun vec-interpolate (a b alpha &optional store)
  "Interpolate a vector from vector `a' to vector `b',
depending on the interpolation factor alpha."
  (let ((beta (invert alpha)))
    (map-into (vec-ensure-store a store)
              #'(lambda (m n)
                  (+ (* m beta)
                     (* n alpha)))
              a b)))

(defvecfun vec2-interpolate (((ax ay) a) ((bx by) b) alpha)
    ((:documentation "Interpolate a vector from vector `a' to vector `b',
depending on the interpolation factor alpha."))
  (let ((beta (invert alpha)))
    (values (+ (* ax beta) (* bx alpha))
            (+ (* ay beta) (* by alpha)))))

(defvecfun vec3-interpolate (((ax ay az) a) ((bx by bz) b) alpha)
    ((:documentation "Interpolate a vector from vector `a' to vector `b',
depending on the interpolation factor alpha."))
  (let ((beta (invert alpha)))
    (values (+ (* ax beta) (* bx alpha))
            (+ (* ay beta) (* by alpha))
            (+ (* az beta) (* bz alpha)))))

(defvecfun vec4-interpolate (((ax ay az aw) a) ((bx by bz bw) b) alpha)
    ((:documentation "Interpolate a vector from vector `a' to vector `b',
depending on the interpolation factor alpha."))
  (let ((beta (invert alpha)))
    (values (+ (* ax beta) (* bx alpha))
            (+ (* ay beta) (* by alpha))
            (+ (* az beta) (* bz alpha))
            (+ (* aw beta) (* bw alpha)))))

;;; vector.lisp ends here
