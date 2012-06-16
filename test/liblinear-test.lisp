(in-package :cl-classifiers-test)

(defparameter *simple-data-file* (asdf:system-relative-pathname :cl-classifiers-test "simple_ex"))

(fiveam:test test-liblinear
  (fiveam:is (equal '(5 5)
             (cl-classifiers::extract-metadata-from-file *simple-data-file*))))