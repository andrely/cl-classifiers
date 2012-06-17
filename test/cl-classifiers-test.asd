(defsystem "cl-classifiers-test"
  :description ""
  :author ""
  :licence ""
  :components ((:file "cl-classifiers-test")
               (:file "liblinear-test" :depends-on ("cl-classifiers-test")))
  :depends-on ("cl-classifiers" "fiveam"))
