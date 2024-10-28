(defun parse-arithmetic (expression)
    "Parses an arithmetic expression and converts it to Lisp."
    (let* ((operators '(#\+ #\- #\* #\/)))
        (loop for op in operators
            when (search (string op) expression)
                do (let* ((parts (split-string op expression))
                            (left (string-trim '(#\space #\;) (first parts)))
                            (right (string-trim '(#\space #\;) (second parts))))
                            (return (format nil "(~C ~A ~A)" op (parse-arithmetic left) (parse-arithmetic right))))
            finally (return (string-trim '(#\space #\;) expression)))))

(defun split-string (delimiter string)
  "Splits STRING into substrings bounded by matches for DELIMITER."
  (loop with start = 0
        for end = (position delimiter string :start start) 
        collect (subseq string start end)
        while end do (setf start (1+ end))))


(defun parse-condition (condition)
  "Parses a C condition and converts it to Lisp prefix notation."
  (labels ((parse-or (cond)
             (let ((parts (cl-ppcre:split "\\s*\\|\\|\\s*" cond)))
               (if (> (length parts) 1)
                   (format nil "(or ~{~A~^ ~})" (mapcar #'parse-and parts))
                   (parse-and (first parts)))))
           (parse-and (cond)
             (let ((parts (cl-ppcre:split "\\s*&&\\s*" cond)))
               (if (> (length parts) 1)
                   (format nil "(and ~{~A~^ ~})" (mapcar #'parse-not parts))
                   (parse-not (first parts)))))
           (parse-not (cond)
             (if (cl-ppcre:scan "\\s*!\\s*(?!\\=)" cond)
                 (let ((part (cl-ppcre:regex-replace "\\s*!\\s*(?!\\=)" cond "")))
                   (format nil "(not ~A)" (parse-condition (string-trim '(#\space) part))))
                 (parse-comparison cond)))
           (parse-comparison (cond)
             (let ((operators '(("==" . "=") ("!=" . "/=") (">=" . ">=") ("<=" . "<=") (">" . ">") ("<" . "<"))))
               (loop for op in operators
                     when (cl-ppcre:scan (format nil "\\s*~A\\s*" (car op)) cond)
                     do (let* ((parts (cl-ppcre:split (format nil "\\s*~A\\s*" (car op)) cond))
                               (left (string-trim '(#\space) (first parts)))
                               (right (string-trim '(#\space) (second parts))))
                          (return (format nil "(~A ~A ~A)" (cdr op) left right)))
                     finally (return cond)))))
    (parse-or condition)))

(defun remove-types (params)
  "Removes types from parameter list."
  (let ((param-list (cl-ppcre:split "," params)))
    (reduce (lambda (acc param)
              (let ((tokens (cl-ppcre:split "\\s+" (string-trim '(#\space) param))))
                (if acc
                    (concatenate 'string acc " " (second tokens))
                    (second tokens))))
            param-list
            :initial-value '())))

(defun parse-inc (increment)
  "Parses the increment part of a for loop."
  (cond
    ((cl-ppcre:scan "\\+\\+" increment)
     (let ((var (string-trim '(#\space) (cl-ppcre:regex-replace "\\+\\+" increment ""))))
       (format nil "(incf ~A)" var)))
    ((cl-ppcre:scan "--" increment)
     (let ((var (string-trim '(#\space) (cl-ppcre:regex-replace "--" increment ""))))
       (format nil "(decf ~A)" var)))
    (t
     (let* ((parts (cl-ppcre:split "=" increment))
            (var (string-trim '(#\space) (first parts)))
            (value (string-trim '(#\space) (second parts))))
       (format nil "(setf ~A ~A)" var (parse-arithmetic value))))))

