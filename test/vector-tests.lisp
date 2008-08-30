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

(deftest vector-copy-test ()
  (is (vec-equal (vec3) (make-vec3))))

(deftest vector-scale-test ()
  (is (vec-equal (vec-scale (vec2 1.0 1.0) 2.0)
                 (vec2 2.0 2.0)))
  (is (vec-equal (vec-scale (vec3 1.0 1.0 1.0) 2.0)
                 (vec3 2.0 2.0 2.0)))
  (is (vec-equal (vec-scale (vec4 1.0 1.0 1.0 1.0) 2.0)
                 (vec4 2.0 2.0 2.0 2.0)))
  (is (vec-equal (vec2-scale (vec2 1.0 1.0) 2.0)
                 (vec2 2.0 2.0)))
  (is (vec-equal (vec3-scale (vec3 1.0 1.0 1.0) 2.0)
                 (vec3 2.0 2.0 2.0)))
  (is (vec-equal (vec4-scale (vec4 1.0 1.0 1.0 1.0) 2.0)
                 (vec4 2.0 2.0 2.0 2.0))))

(deftest vector-div-test ()
  (is (vec-equal (vec-div (vec2 1.0 1.0) 0.5)
                 (vec2 2.0 2.0)))
  (is (vec-equal (vec-div (vec3 1.0 1.0 1.0) 0.5)
                 (vec3 2.0 2.0 2.0)))
  (is (vec-equal (vec-div (vec4 1.0 1.0 1.0 1.0) 0.5)
                 (vec4 2.0 2.0 2.0 2.0)))
  (is (vec-equal (vec2-div (vec2 1.0 1.0) 0.5)
                 (vec2 2.0 2.0)))
  (is (vec-equal (vec3-div (vec3 1.0 1.0 1.0) 0.5)
                 (vec3 2.0 2.0 2.0)))
  (is (vec-equal (vec4-div (vec4 1.0 1.0 1.0 1.0) 0.5)
                 (vec4 2.0 2.0 2.0 2.0))))

(deftest vector-invert-test ()
  (is (vec-equal (vec-invert (vec2 1.0 1.0))
                 (vec2 -1.0 -1.0)))
  (is (vec-equal (vec-invert (vec3 1.0 1.0 1.0))
                 (vec3 -1.0 -1.0 -1.0)))
  (is (vec-equal (vec-div (vec4 1.0 1.0 1.0 1.0))
                 (vec4 -1.0 -1.0 -1.0 -1.0))))



;;; vector-tests.lisp ends here
