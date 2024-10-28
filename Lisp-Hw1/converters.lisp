(defvar *if-condition-stack* nil
  "Stack to keep track of if conditions for corresponding else statements.")

(defvar *first-definition* t
  "Flag to check if the current line is the first variable definition.")

(defun conversion-foo (line-type)
  "Selects the appropriate conversion function based on the line type."
  (case line-type
    (if-statement 'convert-if)
    (else-statement 'convert-else)
    (for-loop 'convert-for)
    (while-loop 'convert-while)
    (function-definition 'convert-function-definition)
    (function-prototype 'convert-function-prototype)
    (function-call 'convert-function-call)
    (variable-definition 'convert-definition)
    (variable-assignment 'convert-assignment)
    (assignment-by-function 'convert-assignment-by-function)
    (return-statement 'convert-return-statement)
    (closing-brace 'convert-closing-brace)
    (t 'convert-unknown)))

(defun convert (line &optional next-line)
  "Converts a line of C code into Lisp using the appropriate conversion function."
  (let* ((trimmed-line (string-trim '(#\space #\tab) line))
         (type (line-type trimmed-line))
         (conversion-fn (conversion-foo type)))
    (when conversion-fn
      (if (eq type 'variable-definition)
          (funcall (intern (symbol-name conversion-fn)) trimmed-line next-line)
          (funcall (intern (symbol-name conversion-fn)) trimmed-line)))))
          
(defun convert-if (line)
  "Converts an if statement from C to Lisp."
  (let* ((start (search "(" line))
         (end (search ")" line :start2 start))
         (condition (subseq line (1+ start) end)))
    (push (parse-condition condition) *if-condition-stack*)
    (format nil "(if ~A (progn" (parse-condition condition))))

(defun convert-else (line)
  "Converts an else statement from C to Lisp."
  (let ((condition (pop *if-condition-stack*)))
    (format nil "(if (not ~A) (progn" condition)))

(defun convert-for (line)
  "Converts a for loop from C to Lisp."
  (let* ((start (search "(" line))
         (end (search ")" line :start2 start))
         (for-content (subseq line (1+ start) end))
         (tokens (cl-ppcre:split ";" for-content))
         (init (first tokens))
         (condition (second tokens))
         (increment (third tokens))
         (init-parts (cl-ppcre:split "=" init))
         (var (second (cl-ppcre:split "\\s+" (first init-parts))))
         (start-value (second init-parts)))
    (format nil "(loop for ~A = ~A~%then ~A~%while ~A do (progn"
            var
            (parse-arithmetic start-value)
            (parse-inc increment)
            (parse-condition condition))))

(defun convert-while (line)
  "Converts a while loop from C to Lisp."
  (let* ((start (search "(" line))
         (end (search ")" line :start2 start))
         (condition (subseq line (1+ start) end)))
    (format nil "(loop while ~A do (progn" (parse-condition condition))))


(defun convert-definition (current-line next-line)
  "Converts variable assignment from C to Lisp."
  (let* ((parts (cl-ppcre:split "\\s*=\\s*" current-line))
         (var (second (cl-ppcre:split "\\s+" (first parts))))
         (value (string-trim '(#\;) (second parts)))
         (is-last-definition (or (null next-line) (not (cl-ppcre:scan "\\w+\\s+\\w+\\s*=\\s*\\w+;" next-line)))))
    (cond
      (*first-definition*
       (setf *first-definition* nil)
       (if is-last-definition 
          (progn
            (setf *first-definition* t)
            (format nil "(let ((~A ~A))" var (parse-arithmetic value)))
          (format nil "(let* ((~A ~A)" var (parse-arithmetic value))))
      (is-last-definition
        (setf *first-definition* t)
        (format nil "(~A ~A))" var (parse-arithmetic value)))
      (t
        (format nil "(~A ~A)" var (parse-arithmetic value))))))

(defun convert-assignment (line)
  "Converts a variable definition from C to Lisp."
  (let* ((parts (cl-ppcre:split "\\s*=\\s*" line))
         (var (first parts))
         (value (second parts)))
    (format nil "(setf ~A ~A)" var (parse-arithmetic value))))

(defun convert-function-definition (line)
  "Converts a function definition from C to Lisp."
  (let* ((tokens (cl-ppcre:split "\\s+" line))
         (name-start (search "(" line))
         (name-end (search ")" line :start2 name-start))
         (name (subseq line (1+ (search " " line)) name-start))
         (params (subseq line (1+ name-start) name-end))
         (clean-params (remove-types params)))
    (if clean-params
        (format nil "(defun ~A (~A)" name clean-params)
        (format nil "(defun ~A ()" name))))

(defun convert-function-prototype (line)
  "Converts a C function prototype to a Lisp declaim statement."
  (let* ((return-type-end (search " " line))
         (return-type (string-trim '(#\space) (subseq line 0 return-type-end)))
         (name-start (1+ return-type-end))
         (name-end (search "(" line :start2 name-start))
         (name (string-trim '(#\space) (subseq line name-start name-end)))
         (args-start (1+ name-end))
         (args-end (search ")" line :start2 args-start))
         (args (subseq line args-start args-end))
         (arg-types (mapcar (lambda (arg)
                              (cond
                                ((cl-ppcre:scan "int" arg) "integer")
                                ((cl-ppcre:scan "float" arg) "float")
                                ((cl-ppcre:scan "double" arg) "float")
                                ((cl-ppcre:scan "bool" arg) "boolean")
                                ((cl-ppcre:scan "char" arg) "character")
                                (t "unknown")))
                            (cl-ppcre:split "\\s*,\\s*" args)))
         (lisp-return-type (cond
                             ((string= return-type "int") "integer")
                             ((string= return-type "float") "float")
                             ((string= return-type "double") "float")
                             ((string= return-type "bool") "boolean")
                             ((string= return-type "char") "character")
                             (t "unknown"))))
    (format nil "(declaim (ftype (function (~{~A~^ ~}) ~A) ~A))"
            arg-types
            lisp-return-type
            name)))

(defun convert-function-call (line)
  "Converts a function call from C to Lisp."
  (let* ((name-end (search "(" line))
         (name (subseq line 0 name-end))
         (args-start (1+ name-end))
         (args-end (search ")" line :start2 args-start))
         (args (subseq line args-start args-end))
         (clean-args (cl-ppcre:regex-replace-all "\\s*,\\s*" args " ")))
    (if (string= (string name) "printf")
        (let ((formatted-args (cl-ppcre:regex-replace-all "%[dcs]" clean-args "~A")))
          (setf formatted-args (cl-ppcre:regex-replace-all "\\\\n" formatted-args "~%"))
          (format nil "(format t ~A)" formatted-args))
        (format nil "(~A ~A)" name clean-args))))

(defun convert-assignment-by-function (line)
  "Converts a variable assignment by function return from C to Lisp."
  (let* ((parts (cl-ppcre:split "=" line))
         (var-part (first parts))
         (value (string-trim '(#\space #\;) (second parts)))
         (var-parts (cl-ppcre:split "\\s+" var-part))
         (var (second (remove-if (lambda (x) (string= x "")) var-parts)))
         (fn-call-end (search ")" value))
         (fn-call (subseq value 0 (1+ fn-call-end))))
    (format nil "(setf ~A ~A)" var (convert-function-call fn-call))))

(defun convert-return-statement (line)
  "Converts a return statement from C to Lisp."
  (let* ((value-start (+ (search "return" line) (length "return")))
         (value (subseq line (1+ value-start))))
    (format nil "~A" (parse-arithmetic value))))

(defun convert-closing-brace (line)
  "Converts a closing brace from C to Lisp."
  (format nil "))"))

(defun convert-unknown (line)
  "Handles unknown lines of C code."
  (if (string= line "")
      nil
      (format nil ";; Unknown line: ~A" line)))