(in-package :cl-classifiers)

(defparameter *test-x*
  '((2 0.1) (3 0.2) (6 1) (-1 0)
    (2 0.1) (3 0.3) (4 -1.2) (6 1) (-1 0)
    (1 0.4) (6 1) (-1 0)
    (2 0.1) (4 1.4) (5 0.5) (6 1) (-1 0)
    (1 -0.1) (2 -0.2) (3 0.1) (4 1.1) (5 0.1) (6 1) (-1 0)))

(defparameter *test-y*
  '(1 2 1 2 3))

(cffi:load-foreign-library "/Users/stinky/Downloads/liblinear-1.91/liblinear.so.1")

(cffi:defcstruct %feature-node
  (index :int)
  (value :double))

(cffi:defcstruct %problem
  (l :int)
  (n :int)
  (y (:pointer :double))
  (x (:pointer (:pointer %feature-node)))
  (bias :double))

(cffi:defcstruct %parameter
  (solver-type :int)
  (eps :double)
  (c :double)
  (nr-weight :int)
  (weight-label (:pointer :int))
  (weight (:pointer :double))
  (p :double))

(cffi:defcstruct %model
  (param %parameter)
  (nr-class :int)
  (nr-feature :int)
  (w (:pointer :double))
  (label (:pointer :int))
  (bias :double))

(cffi:defcfun ("check_parameter" %check-parameter) (:pointer :char)
  (prob (:pointer %problem))
  (param (:pointer %parameter)))

(cffi:defcfun ("train" %train) (:pointer %model)
  (prob (:pointer %problem))
  (param (:pointer %parameter)))

(defun make-pr ()
  (let ((pr (cffi:foreign-alloc '%problem)))
    (setf (cffi:foreign-slot-value pr '%problem 'l) 5)
    (setf (cffi:foreign-slot-value pr '%problem 'n) 6)
    (setf (cffi:foreign-slot-value pr '%problem 'bias) (coerce 1.0 'double-float))

    (setf (cffi:foreign-slot-value pr '%problem 'y)
          (cffi:foreign-alloc :double :count (length *test-y*)))
    (loop for i from 0 below (length *test-y*)
                   do (setf (cffi:mem-aref (cffi:foreign-slot-value pr '%problem 'y) :double i)
                            (coerce (elt *test-y* i) 'double-float)))

    (let ((nodes (cffi:foreign-alloc '%feature-node :count 24))
          (node-p (cffi:foreign-alloc '(:pointer (:pointer %feature-node)) :count 5)))
      (setf (cffi:mem-aref node-p '(:pointer (:pointer %feature-node)) 0) nodes)

      (let ((indices (loop for i from 0 below 24
                           for (index value) = (elt *test-x* i)
                           for node = (cffi:mem-aref nodes '%feature-node i)
                           do (setf (cffi:foreign-slot-value node '%feature-node 'index)
                                    (coerce index 'integer))
                           do (setf (cffi:foreign-slot-value node '%feature-node 'value)
                                    (coerce value 'double-float))
                           when (= index -1)
                           collect i)))
        (loop for i from 1 below (length indices)
              for j in indices
              do (setf (cffi:mem-aref node-p '(:pointer %feature-node) i)
                       (cffi:mem-aref nodes '%feature-node (1+ j))))
        (setf (cffi:foreign-slot-value pr '%problem 'x)
              node-p)))
 
    pr))

(defun make-pa ()
  (let ((params (cffi:foreign-alloc '%parameter)))
    (setf (cffi:foreign-slot-value params '%parameter 'solver-type) 0)
    (setf (cffi:foreign-slot-value params '%parameter 'eps) (coerce 0.01 'double-float))
    (setf (cffi:foreign-slot-value params '%parameter 'c) (coerce 1 'double-float))
    (setf (cffi:foreign-slot-value params '%parameter 'nr-weight) 0)
    (setf (cffi:foreign-slot-value params '%parameter 'p) 0.1)
    (setf (cffi:foreign-slot-value params '%parameter 'weight-label) (cffi:make-pointer 0))
    (setf (cffi:foreign-slot-value params '%parameter 'weight) (cffi:make-pointer 0))

    params))

