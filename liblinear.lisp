(in-package :cl-classifiers)

(defstruct model
  foreign-problem foreign-parameter foreign-model)

(defun train-model-from-file (fn &key (bias *default-bias*))
  (let* ((f-prob (%init-problem fn :bias bias))
         (f-param (%init-parameter))
         (f-model (%train f-prob f-param))
         (check-ptr (%check-parameter f-prob f-param)))

    ;; TODO add error handler to release already allocated
    ;; foreign memory here
    (when (not (cffi:null-pointer-p check-ptr))
      (let ((msg (cffi:convert-from-foreign check-ptr :string)))
        (cffi:foreign-free check-ptr)
        (error msg)))
    
    (make-model :foreign-problem f-prob
                :foreign-parameter f-param
                :foreign-model f-model)))

(defun dispose-model (model)
  (%free-parameter (model-foreign-parameter model))
  (%free-model (model-foreign-model model))

  (if (not (cffi:null-pointer-p (model-foreign-problem model)))
    (%free-problem (model-foreign-problem model)))
  
  nil)

(defun save-model-to-file (model filename)
  (%save-model filename (model-foreign-model model)))

(defun load-model-from-file (filename)
  (let* ((foreign-model (%load-model filename))
         (foreign-param (cffi:foreign-slot-value foreign-model '%model 'param)))
    (make-model :foreign-problem (cffi:null-pointer)
                :foreign-parameter foreign-param
                :foreign-model foreign-model)))

(defun model-bias (model)
  (cffi:foreign-slot-value (model-foreign-model model) '%model 'bias))

(defun model-feature-count (model)
  (cffi:foreign-slot-value (model-foreign-model model) '%model 'nr-feature))

(defun model-class-count (model)
  (cffi:foreign-slot-value (model-foreign-model model) '%model 'nr-class))

(defun model-data-count (model)
  (if (cffi:null-pointer-p (model-foreign-problem model))
    nil
    (cffi:foreign-slot-value (model-foreign-problem model) '%problem 'l)))

(defun model-weight-vector (model)
  (let* ((w-mem (cffi:foreign-slot-value (model-foreign-model model) '%model 'w))
         (feature-count (model-feature-count model))
         (class-count (model-class-count model))
         (len (* feature-count class-count)))
    (when (> (model-bias model) 0)
      (incf len class-count))
    (cffi:convert-from-foreign w-mem (list :array :double len))))

(defun model-solver-type (model)
  (cffi:foreign-enum-keyword '%solver-types
                             (cffi:foreign-slot-value (model-foreign-parameter model)
                                                      '%parameter 'solver-type)))

(defun predict (x model)
  (let* ((x (if (> (model-bias model) 0)
              (append x (list (list (1+ (model-feature-count model)) 1.0) (list -1 0.0)))
              (append x (list (list -1 0.0)))))
         (x-mem (cffi:foreign-alloc '%feature-node :count (length x)))
         (result nil))
    (%write-feature-nodes x-mem x)
    (setf result (%predict (model-foreign-model model)
                           x-mem))
    (cffi:foreign-free x-mem)

    result))

(defun enable-output ()
  (%set-print-string-func (cffi:callback standard-write)))

(defun disable-output ()
  (%set-print-string-func (cffi:null-pointer)))

(defun cross-validation (model &key (folds 10))
  (unless (null (model-data-count model))
    (let* ((l (model-data-count model))
           (target (cffi:foreign-alloc :double :count l))
           (y (cffi:foreign-slot-value (model-foreign-problem model) '%problem 'y))

           (total-correct 0)
           (total-error 0)
           (sum-v 0)
           (sum-y 0)
           (sum-vv 0)
           (sum-yy 0)
           (sum-vy 0))
    
      (%cross-validation (model-foreign-problem model)
                         (model-foreign-parameter model)
                         folds
                         target)

      (cond ((member (model-solver-type model)
                     '(:L2R-L2LOSS-SVR :L2R-L1LOSS-SVR-DUAL :L2R-L2LOSS-SVR-DUAL))
             (loop for i from 0 below l
                   for y = (cffi:mem-aref y :double i)
                   for v = (cffi:mem-aref target :double i)
                   do (incf total-error (* (- v y) (- v y)))
                   do (incf sum-v v)
                   do (incf sum-y y)
                   do (incf sum-vv (* v v))
                   do (incf sum-yy (* y y))
                   do (incf sum-vy (* v y)))

             (format t "Cross Validation Mean squared error = ~$~%"
                     (/ total-error l))
             (format t "Cross Validation Squared correlation coefficient = ~$~%"
                     (/ (* (- (* l sum-vy)
                              (* sum-v sum-y))
                           (- (* l sum-vy)
                              (* sum-v sum-y)))
                        (* (- (* l sum-vv)
                              (* sum-v sum-v))
                           (- (* l sum-yy)
                              (* sum-y sum-y))))))
            (t (loop for i from 0 below l
                     when (= (cffi:mem-aref target :double i)
                             (cffi:mem-aref y :double i))
                     do (incf total-correct))

               (format t "Cross validation accuracy = ~$ %~%"
                       (* 100.0 (/ total-correct l)))))
    
      (cffi:foreign-free target)

      nil)))
