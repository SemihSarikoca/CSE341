(let ((quicklisp-init (merge-pathnames "quicklisp/setup.lisp" (user-homedir-pathname))))
  (when (probe-file quicklisp-init)
    (load quicklisp-init)))

(quicklisp:quickload "cl-ppcre")
(use-package :cl-ppcre)

(load "converters.lisp")
(load "helper.lisp")
(load "read-write.lisp")

(defun line-type (line)
  "Determines the type of a line of C code using regex."
  (cond
    ;; if statement
    ((cl-ppcre:scan "if\\s*\\(.*\\)\\s*\\{" line) 'if-statement)
    ;; else statement
    ((cl-ppcre:scan "else\\s*\\{" line) 'else-statement)
    ;; for loop
    ((cl-ppcre:scan "for\\s*\\(.*\\)\\s*\\{" line) 'for-loop)
    ;; while loop
    ((cl-ppcre:scan "while\\s*\\(.*\\)\\s*\\{" line) 'while-loop)
    ;; variable assignment by function return
    ((cl-ppcre:scan "\\w+\\s*=\\s*\\w+\\s*\\(.*\\)\\s*;" line) 'assignment-by-function)
    ;; return statement
    ((cl-ppcre:scan "^\\s*return\\s+.*;" line) 'return-statement)
    ;; function definition
    ((cl-ppcre:scan "\\w+\\s+\\w+\\s*\\(.*\\)\\s*\\{" line) 'function-definition)
    ;; function prototype
    ((cl-ppcre:scan "\\w+\\s+\\w+\\s*\\(.*\\)\\s*;" line) 'function-prototype)
    ;; function call
    ((cl-ppcre:scan "\\w+\\s*\\(.*\\)\\s*;" line) 'function-call)
    ;; variable assignment
    ((cl-ppcre:scan "^\\s*\\w+\\s*=\\s*[^=].*;" line) 'variable-assignment)
    ;; variable definition
    ((cl-ppcre:scan "\\w+\\s*=\\s*[^=].*;" line) 'variable-definition)
    ;; closing
    ((cl-ppcre:scan "^\\s*}\\s*" line) 'closing-brace)
    ;; unknown
    (t 'unknown)))

(defun process-lines (lines)
  "Processes each line and converts it to Lisp code recursively."
  (labels ((process-helper (lines index result)
             (if (>= index (length lines))
                 (reverse result)
                 (let ((line (nth index lines))
                       (next-line (nth (1+ index) lines)))
                   (process-helper lines (1+ index) (cons (convert line next-line) result))))))
    (process-helper lines 0 '())))

(defun main (input-file output-file)
  "Main function to read C file, convert to Lisp, and write to output file."
  (let* ((lines (read-file input-file))
        (converted-lines (process-lines lines)))
    (write-file output-file converted-lines)))

(main "input.c" "output.lisp")
