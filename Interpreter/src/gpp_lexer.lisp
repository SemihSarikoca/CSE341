(defparameter *keywords*
  '(("and" . KW_AND) ("or" . KW_OR) ("not" . KW_NOT)
    ("equal" . KW_EQUAL) ("less" . KW_LESS) ("nil" . KW_NIL)
    ("list" . KW_LIST) ("append" . KW_APPEND) ("concat" . KW_CONCAT)
    ("set" . KW_SET) ("deffun" . KW_DEFFUN) ("for" . KW_FOR)
    ("if" . KW_IF) ("exit" . KW_EXIT) ("load" . KW_LOAD)
    ("disp" . KW_DISP) ("true" . KW_TRUE) ("false" . KW_FALSE)
    ("while" . KW_WHILE) ("load" . KW_LOAD)))

(defparameter *operators*
  '(("+" . OP_PLUS) ("-" . OP_MINUS) ("/" . OP_DIV)
    ("*" . OP_MULT) ("(" . OP_OP) (")" . OP_CP)
    ("," . OP_COMMA)))

(defparameter *comment-token* 'COMMENT)
(defparameter *fraction-token* 'VALUEF)
(defparameter *integer-token* 'VALUEI)
(defparameter *identifier-token* 'IDENTIFIER)
(defparameter *syntax-error-token* 'SYNTAX_ERROR)

(defun keyword-token (word)
  (cdr (assoc word *keywords* :test #'string=)))

(defun operator-token (symbol)
  (cdr (assoc symbol *operators* :test #'string=)))

(defun is-integer (word)
  (and (not (zerop (length word)))
       (every #'digit-char-p word)))

(defun is-fraction (word)
  (let ((colon-pos (position #\: word)))
    (and colon-pos
         (is-integer (subseq word 0 colon-pos))
         (is-integer (subseq word (1+ colon-pos))))))

(defun is-identifier (word)
  (and (alpha-char-p (char word 0))
       (every (lambda (c) (or (alpha-char-p c) (digit-char-p c) (char= c #\_))) word)))

(defun is-comment (word)
  (when (>= (length word) 2)
    (and (char= (char word 0) #\;)
         (char= (char word 1) #\;))))

(defun split-line (line)
  (let ((tokens '())
        (current-token ""))
    (labels ((add-token ()
               (when (not (string= current-token ""))
                 (push current-token tokens)
                 (setf current-token ""))))
      (loop for char across line do
           (cond
             ((char= char #\;)
              (add-token)
              (push (subseq line (position #\; line)) tokens)
              (return))
             ((char= char #\Space)
              (add-token))
             ((find char '(#\+ #\- #\/ #\* #\( #\) #\,))
              (add-token)
              (push (string char) tokens))
             (t
              (setf current-token (concatenate 'string current-token (string char))))))
      (add-token))
    (reverse tokens)))

(defun tokenize (line)
  (let ((tokens '()))
    (dolist (word (split-line line))
      (leT ((token (oR (keyword-token word)
                       (operator-token word)
                       (cond
                         ((is-integer word) *integer-token*)
                         ((is-fraction word) *fraction-token*)
                         ((is-identifier word) *identifier-token*)
                         ((is-comment word) *comment-token*)
                         (t
                          (format nil "~a: \"~a\" cannot be tokenized" *syntax-error-token* word))))))
        (push token tokens)
        (when (eq token *comment-token*)
          (return-from tokenize (reverse tokens)))))
    (reverse tokens)))

(defun read-eval-print-loop ()
  (loop
    (format t "> ")
    (let ((line (read-line *standard-input* nil)))
      (when line
        (if (string= line "(quit)")
            (return)
            (let ((tokens (tokenize line)))
              (format t "~{~a~%~}" tokens)))))))

(defun gpplexer (&optional (filename nil))
  (if filename
      (progn
        (with-open-file (stream filename)
          (loop for line = (read-line stream nil)
                while line do
                (let ((tokens (tokenize line)))
                  (format t "~{~a~%~}" tokens))))
        (read-eval-print-loop))
      (read-eval-print-loop)))