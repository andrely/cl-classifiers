(in-package :cl-classifiers)

(defstruct model
  foreign-problem foreign-parameter foreign-model)

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
        (n 0))
    (with-open-file (s fn)
      (loop for line = (read-line s nil nil)
            while line
            for data = (parse-data-line line)
            for max-n = (apply #'max (mapcar #'first (cadr data)))
            do (incf l)
            when (> max-n n)
            do (setf n max-n)))
    (list l n)))

(defun make-model-from-file (fn)
  (extract-metadata-from-file fn))