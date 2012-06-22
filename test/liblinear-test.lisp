(in-package :cl-classifiers-test)

(defparameter *simple-data-file* (asdf:system-relative-pathname :cl-classifiers-test "simple_ex"))
(defparameter *iris-data-file* (asdf:system-relative-pathname :cl-classifiers-test "iris"))
(defparameter *red-wine-data-file* (asdf:system-relative-pathname :cl-classifiers-test
                                                                  "wine-quality-red"))
(defparameter *white-wine-data-file* (asdf:system-relative-pathname :cl-classifiers-test
                                                                    "wine-quality-white"))

(fiveam:test test-liblinear
  (fiveam:is (equal '(14 5 5)
             (cl-classifiers::extract-metadata-from-file *simple-data-file*))))