(in-package :cl-classifiers)

(defstruct model
  foreign-problem foreign-parameter foreign-model)

(defun make-model-from-file (fn &key (bias *default-bias*))
  (let* ((f-prob (%init-problem fn :bias bias))
         (f-param (%init-parameter))
         (f-model (%train f-prob f-param)))
    (make-model :foreign-problem f-prob
                :foreign-parameter f-param
                :foreign-model f-model)))

(defun dispose-model (model)
  (%free-parameter (model-foreign-parameter model))
  (%free-problem (model-foreign-problem model))
  (%free-model (model-foreign-model model))

  nil)

(defun model-bias (model)
  (cffi:foreign-slot-value (model-foreign-model model) '%model 'bias))

(defun model-feature-count (model)
  (cffi:foreign-slot-value (model-foreign-model model) '%model 'nr-feature))

(defun predict (x model)
  (let ((x (if (> (model-bias model) 0)
             (append x (list (list (1+ (model-feature-count model)) 1.0) (list -1 0.0)))
             (append x (list (list -1 0.0)))))
        (x-mem (cffi:foreign-alloc '%feature-node :count (length x)))
        (result nil))
    (%write-feature-nodes x-mem x)
    (setf result (%predict (model-foreign-model model)
                           x-mem))
    (cffi:foreign-free x-mem)

    result))
