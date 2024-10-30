(defun read-file (filename)
    "Reads the C file line by line and returns a list of non-whitespace lines."
    (with-open-file (stream filename :direction :input)
        (loop for line = (read-line stream nil)
                    while line
                    unless (string= (string-trim '(#\Space #\Tab #\Newline) line) "")
                    collect line)))

(defun write-file (filename content)
  "Writes the converted Lisp code to the output file."
  (with-open-file (stream filename :direction :output :if-does-not-exist :create :element-type 'character)
    (dolist (line content)
      (when line
        (write-line line stream)))
    (write-line "(main)" stream))) ;; can be removed if main is not needed or is called differently