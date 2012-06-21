(in-package :cl-classifiers-test)

(defparameter *simple-data-file* (asdf:system-relative-pathname :cl-classifiers-test "simple_ex"))
(defparameter *iris-data-file* (asdf:system-relative-pathname :cl-classifiers-test "iris"))

(fiveam:test test-liblinear
  (fiveam:is (equal '(14 5 5)
             (cl-classifiers::extract-metadata-from-file *simple-data-file*))))