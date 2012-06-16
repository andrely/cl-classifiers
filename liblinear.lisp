(in-package :cl-classifiers)

(defstruct model
  foreign-problem foreign-parameter foreign-model)

(defun make-model-from-file (fn)
  (let ((model (make-model))
        (f-prob ))))
