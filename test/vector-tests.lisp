;;; vector-tests.lisp --- testing the vectors
;;;                 _                  _            _
;;; __   _____  ___| |_ ___  _ __     | |_ ___  ___| |_ ___
;;; \ \ / / _ \/ __| __/ _ \| '__|____| __/ _ \/ __| __/ __|
;;;  \ V /  __/ (__| || (_) | | |_____| ||  __/\__ \ |_\__ \
;;;   \_/ \___|\___|\__\___/|_|        \__\___||___/\__|___/
;;;
;;; Copyright (C) 2007 Ole Arndt <ole@sugarshark.com>
;;;

(in-package :vecmath-test)

(in-suite vecmath-tests)

(defsuite* vector-tests)

(deftest vector-equal-test ()
  (is (equal? (vec3) (vec3 0.0 0.0 0.0)))
  (is (equal? (vec3) (vec3 0.1) :epsilon 0.1)))

(deftest vector-copy-test ()
  (is (equal? (vec3) (vec 0.0 0.0 0.0))))

(deftest vector-scale-test ()
  (is (equal? (vec-scale (vec2 1.0 1.0) 2.0)
              (vec2 2.0 2.0)))
  (is (equal? (vec-scale (vec3 1.0 1.0 1.0) 2.0)
              (vec3 2.0 2.0 2.0)))
  (is (equal? (vec-scale (vec4 1.0 1.0 1.0 1.0) 2.0)
              (vec4 2.0 2.0 2.0 2.0)))
  (is (equal? (vec2-scale (vec2 1.0 1.0) 2.0)
              (vec2 2.0 2.0)))
  (is (equal? (vec3-scale (vec3 1.0 1.0 1.0) 2.0)
              (vec3 2.0 2.0 2.0)))
  (is (equal? (vec4-scale (vec4 1.0 1.0 1.0 1.0) 2.0)
              (vec4 2.0 2.0 2.0 2.0))))

(deftest vector-div-test ()
  (is (equal? (vec-div (vec2 1.0 1.0) 0.5 (vec2))
              (vec2 2.0 2.0)))
  (is (equal? (vec3-div (vec3 1.0 1.0 1.0) 0.5)
              (vec3 2.0 2.0 2.0)))
  (is (equal? (vec4-div (vec4 1.0 1.0 1.0 1.0) 0.5)
              (vec4 2.0 2.0 2.0 2.0)))
  (is (equal? (vec2-div (vec2 1.0 1.0) 0.5)
              (vec2 2.0 2.0)))
  (is (equal? (vec3-div (vec3 1.0 1.0 1.0) 0.5)
              (vec3 2.0 2.0 2.0)))
  (is (equal? (vec4-div (vec4 1.0 1.0 1.0 1.0) 0.5)
              (vec4 2.0 2.0 2.0 2.0))))

(deftest vector-invert-test ()
  (is (equal? (vec-invert (vec2 1.0 1.0))
              (vec2 -1.0 -1.0)))
  (is (equal? (vec-invert (vec3 1.0 1.0 1.0))
              (vec3 -1.0 -1.0 -1.0)))
  (is (equal? (vec-invert (vec4 1.0 1.0 1.0 1.0))
              (vec4 -1.0 -1.0 -1.0 -1.0)))
  (is (equal? (vec-invert (vec4 1.0 1.0 1.0 1.0))
              (vec4-invert (vec4 1.0 1.0 1.0 1.0)))))

(deftest vector-addition-test ()
  (is (equal? (vec-add (vec2 1.0 1.0) (vec2 1.0 1.0))
              (vec2 2.0 2.0)))
  (is (equal? (vec-add (vec2 1.0 1.0) (vec2 1.0 1.0))
              (vec-sub (vec2 4.0 4.0) (vec2 2.0 2.0)))))

(deftest vector-dot-test ()
  (is (= 9
         (vec-dot (vec3 1.0 1.0 1.0) (vec3 3.0 3.0 3.0))
         (vec3-dot (vec3 1.0 1.0 1.0) (vec3 3.0 3.0 3.0))
         (vec3-dot*  1.0 1.0 1.0 3.0 3.0 3.0))))

(deftest swizzle-test ()
  (let ((v (vec4 1.0 2.0 3.0 4.0))
        (color (vec4 1.1 2.2 3.3 4.4))
        (coord (vec4 1.2 2.3 3.4 4.5)))
    (is (= 1.0 (swizzle v :x))
        (= 2.0 (swizzle v :y))
        (= 3.0 (swizzle v :z))
        (= 4.0 (swizzle v :w)))
    (is (= 1.1 (swizzle color :r))
        (= 2.2 (swizzle color :g))
        (= 3.3 (swizzle color :b))
        (= 4.4 (swizzle color :a)))
    (is (= 1.2 (swizzle coord :s))
        (= 2.3 (swizzle coord :t))
        (= 3.4 (swizzle coord :p))
        (= 4.5 (swizzle coord :q)))
    (is (equal? #(1.0 1.0) (swizzle v :xx))
        (equal? #(2.0 1.0) (swizzle v :yx)))))

(deftest with-swizzle-test ()
  (let ((v (vec4  1.0 2.0 3.0 4.0)))
    (with-swizzle
      (is (equal? #(1.0 3.0) v.xz)
          (equal? #(4.0 2.0 3.0) v.wyz)))))

;;; vector-tests.lisp ends here
