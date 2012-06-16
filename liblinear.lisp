(in-package :cl-classifiers)

(defstruct model
  foreign-problem foreign-parameter foreign-model)

(defun make-model-from-file (fn)
  (let* ((f-prob (%init-problem fn))
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