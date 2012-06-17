(defsystem "cl-classifiers"
  :description ""
  :author ""
  :licence ""
  :components ((:file "cl-classifiers")
               (:file "utilities" :depends-on ("cl-classifiers"))
               (:file "liblinear-foreign" :depends-on ("cl-classifiers"
                                                       "utilities"))
               (:file "liblinear" :depends-on ("liblinear-foreign")))
  :depends-on ("cffi" "parse-number"))
