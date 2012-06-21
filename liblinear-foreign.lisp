(in-package :cl-classifiers)

(defparameter *default-bias* -1.0)
(defparameter *default-solver-type*
  (cffi:foreign-enum-value '%solver-types :l2r-lr))

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

(cffi:defcfun ("predict" %predict) :double
  (model (:pointer %model))
  (x (:pointer %feature-node)))

(defun parse-data-line (line)
  (let* ((items (split-line line)))
    (if (null items)
      nil
      (list (parse-number:parse-number (first items))
            (loop for item in (rest items)
                  for (index value) = (mapcar #'parse-number:parse-number
                                              (split-line item '(#\:)))
                  collect (list index value))))))

(defun extract-metadata-from-file (fn)
  (let ((l 0)
        (n 0)
        (count 0))
    (with-open-file (s fn)
      (loop for line = (read-line s nil nil)
            while line
            for data = (parse-data-line line)
            for max-n = (apply #'max (mapcar #'first (cadr data)))
            do (incf l)
            do (incf count (length (cadr data)))
            when (> max-n n)
            do (setf n max-n)))
    (list count l n)))

(defun %init-problem (fn &key (bias *default-bias*))
  (let ((prob (cffi:foreign-alloc '%problem)))
    (destructuring-bind (node-count l n)
        (extract-metadata-from-file fn)
      (when (> bias 0)
        (incf n)
        (incf node-count l))

      (setf (cffi:foreign-slot-value prob '%problem 'l) l)
      (setf (cffi:foreign-slot-value prob '%problem 'n) n)
      (setf (cffi:foreign-slot-value prob '%problem 'bias)
            (coerce 1.0 'double-float))

      (let* ((y-mem (cffi:foreign-alloc :double :count l))
             (x-mem-size (+ node-count l))
             (x-mem (cffi:foreign-alloc '%feature-node :count x-mem-size))
             (x-p (cffi:foreign-alloc '(:pointer %feature-node) :count l)))
        (setf (cffi:foreign-slot-value prob '%problem 'y) y-mem)
        (setf (cffi:foreign-slot-value prob '%problem 'x) x-p)

        (setf (cffi:mem-aref x-p '(:pointer %feature-node) 0)
              (cffi:mem-aref x-mem '%feature-node))

        (with-open-file (s fn)
          (loop for line = (read-line s nil nil)
                for l-idx from 0
                with count = 0
                while line
                                
                for data = (parse-data-line line)
                for label = (first data)
                for nodes = (if (> bias 0)
                              (append (cadr data) (list (list n 1.0) (list -1 0.0)))
                              (append (cadr data) (list (list -1 0.0))))

                when (>= l-idx l) do (error "l out of bounds")

                do (setf (cffi:mem-aref y-mem :double l-idx)
                         (coerce label 'double-float))

                do (loop for (index value) in nodes
                         for node = (cffi:mem-aref x-mem '%feature-node count)
                         when (>= count x-mem-size) do (error "node count out of bounds")
                         do (setf (cffi:foreign-slot-value node '%feature-node 'index)
                                  (coerce index 'integer))
                         do (setf (cffi:foreign-slot-value node '%feature-node 'value)
                                  (coerce value 'double-float))
                         do (incf count))

                when (< l-idx (1- l))
                do (setf (cffi:mem-aref x-p '(:pointer %feature-node) (1+ l-idx))
                         (cffi:mem-aref x-mem '%feature-node count))))))

    prob))

(defun %free-problem (prob)
  (cffi:foreign-free (cffi:mem-aref (cffi:foreign-slot-value prob '%problem 'x)
                                    '(:pointer %feature-node)))
  (cffi:foreign-free (cffi:foreign-slot-value prob '%problem 'x))
  (cffi:foreign-free (cffi:foreign-slot-value prob '%problem 'y))
  (cffi:foreign-free prob))

(defun %init-parameter (&key (solver-type *default-solver-type*))
  (let ((param (cffi:foreign-alloc '%parameter)))
    (setf (cffi:foreign-slot-value param '%parameter 'solver-type) solver-type)
    (setf (cffi:foreign-slot-value param '%parameter 'eps)
          (coerce 0.01 'double-float))
    (setf (cffi:foreign-slot-value param '%parameter 'c)
          (coerce 1 'double-float))
    (setf (cffi:foreign-slot-value param '%parameter 'nr-weight) 0)
    (setf (cffi:foreign-slot-value param '%parameter 'p)
          (coerce 0.1 'double-float))
    (setf (cffi:foreign-slot-value param '%parameter 'weight-label)
          (cffi:null-pointer))
    (setf (cffi:foreign-slot-value param '%parameter 'weight)
          (cffi:null-pointer))

    param))

(cffi:defcfun ("destroy_param" %destroy-param) :void
  (param (:pointer %parameter)))

(defun %free-parameter (param)
  (%destroy-param param)
  (cffi:foreign-free param))

(cffi:defcfun ("free_model_content" %free-model-content) :void
  (model (:pointer %model)))

(defun %free-model (model)
  (%free-model-content model)
  (cffi:foreign-free model))

(defun %write-feature-nodes (x-ptr nodes)
  (loop for i from 0
        for (index value) in nodes
        for node = (cffi:mem-aref x-ptr '%feature-node i)
        do (setf (cffi:foreign-slot-value node '%feature-node 'index)
                 (coerce index 'integer))
        do (setf (cffi:foreign-slot-value node '%feature-node 'value)
                 (coerce value 'double-float))))

(cffi:defcallback standard-write :void
    ((s :string))
  (format t "~a" s))

(cffi:defcfun ("set_print_string_function" %set-print-string-func) :void
  (print-func :pointer))

(cffi:defcfun ("cross_validation" %cross-validation) :void
  (prob (:pointer %problem))
  (param (:pointer %parameter))
  (nr-fold :int)
  (target (:pointer :double)))

(cffi:defcenum %solver-types
  :L2R-LR
  :L2R-L2LOSS-SVC-DUAL
  :L2R-L2LOSS-SVC
  :L2R-L1LOSS-SVC-DUAL
  :MCSVM-CS
  :L1R-L2LOSS-SVC
  :L1R-LR
  :L2R-LR-DUAL
  (:L2R-L2LOSS-SVR 11)
  :L2R-L2LOSS-SVR-DUAL
  :L2R-L1LOSS-SVR-DUAL)

(cffi:defcfun ("load_model" %load-model) (:pointer %model)
  (model-file-name :string))

(cffi:defcfun ("save_model" %save-model) :int
  (model-file-name :string)
  (model (:pointer %model)))
