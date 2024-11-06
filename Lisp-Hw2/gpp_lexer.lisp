(defparameter *keywords* 
  '(("and" . KW_AND) ("or" . KW_OR) ("not" . KW_NOT)
    ("equal" . KW_EQUAL) ("less" . KW_LESS) ("nil" . KW_NIL)
    ("list" . KW_LIST) ("append" . KW_APPEND) ("concat" . KW_CONCAT)
    ("set" . KW_SET) ("deffun" . KW_DEFFUN) ("for" . KW_FOR)
    ("if" . KW_IF) ("exit" . KW_EXIT) ("load" . KW_LOAD)
    ("print" . KW_DISP) ("true" . KW_TRUE) ("false" . KW_FALSE)))
(defparameter *operators*
  '(("+" . OP_PLUS) ("-" . OP_MINUS) ("/" . OP_DIV)
    ("*" . OP_MULT) ("(" . OP_OP) (")" . OP_CP)
    ("," . OP_COMMA)))
(defparameter *comment-token* 'COMMENT)      ; For comments starting with ;;
(defparameter *fraction-token* 'VALUEF)      ; For fractions like 123:12
(defparameter *integer-token* 'VALUEI)       ; For integers like 123
(defparameter *identifier-token* 'IDENTIFIER) ; For valid identifiers
(defparameter *syntax-error-token* 'SYNTAX_ERROR) ; For unknown tokens or errors

(defun keyword-token (word)
  (cdr (assoc word *keywords* :test #'string=)))

(defun operator-token (symbol)
  (cdr (assoc symbol *operators* :test #'string=)))

(defun is-integer (word)
  ;; Checks if a word represents an integer
  (every #'digit-char-p word))

(defun is-fraction (word)
  ;; Checks if a word represents a fraction like 123:45
  (let ((colon-pos (position #\: word)))
    (and colon-pos
         (every #'digit-char-p (subseq word 0 colon-pos))
         (every #'digit-char-p (subseq word (1+ colon-pos)))
         (not (string= (subseq word (1+ colon-pos)) "0")))))

(defun is-identifier (word)
  ;; Checks if a word is a valid identifier (letters, digits, and underscores, starts with a letter)
  (and (alpha-char-p (char word 0))
       (every (lambda (c) (or (alpha-char-p c) (digit-char-p c) (char= c #\_))) word)))

(defun is-comment (word)
  ;; Checks if a word is a comment starting with ;;
  (string= word ";;"))

(defun split-line (line)
  (let ((tokens '())
        (current-token ""))
    (labels ((add-token ()
               (when (not (string= current-token ""))
                 (push current-token tokens)
                 (setf current-token ""))))
      (loop for char across line do
           (cond
             ((char= char #\Space)
              (add-token))
             ((or (char= char #\() (char= char #\)))
              (add-token)
              (push (string char) tokens))
             (t
              (setf current-token (concatenate 'string current-token (string char))))))
      (add-token))
    (reverse tokens)))

(defun tokenize (line)
  (let ((tokens '()))
    (dolist (word (split-line line))
      (let ((token (or (keyword-token word)
                       (operator-token word)
                       (cond
                         ((is-integer word) *integer-token*)
                         ((is-fraction word) *fraction-token*)
                         ((is-identifier word) *identifier-token*)
                         ((is-comment word) *comment-token*)
                         (t *syntax-error-token*)))))
        (push token tokens)
        (when (eq token *comment-token*)
          (return-from tokenize tokens))))
    (reverse tokens)))

(defun read-eval-print-loop ()
  (loop
    (format t "> ")
    (let ((line (read-line *standard-input* nil)))
      (when line
        (let ((tokens (tokenize line)))
          (format t "~{~a~%~}" tokens))))))

(defun gppinterpreter (&optional (filename nil))
  (if filename
      (progn
        (with-open-file (stream filename)
          (loop for line = (read-line stream nil)
                while line do
                (let ((tokens (tokenize line)))
                  (format t "~{~a~%~}" tokens)))))
      (read-eval-print-loop)))

;; Test the gppinterpreter function
;(gppinterpreter "myhelloworld.g++")
(gppinterpreter)