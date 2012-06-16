(in-package :cl-classifiers)

(defun split-line (line &optional (whitespace '(#\space #\newline #\tab #\newline)) (delimiter nil))
  (let ((last (1- (length line))))
    (loop for c across line
          for i from 0
          with in-word = nil
          with in-delimiter = nil
          with start = 0

          when (equal c delimiter)
          do (setf in-delimiter (if in-delimiter nil t))
          
          when (and (not in-word) (not (member c whitespace)))
          do (setf start i
                   in-word t)
          
          when (and in-word (member c whitespace) (not in-delimiter))
          do (setf in-word nil)
          and collect (subseq line start i)

          when (and in-word (= i last))
          collect (subseq line start (1+ i)))))
