(load "src/gpp_lexer.lisp")

(defun parse-expr (tokens)
  (cond
    ((null tokens) (values nil tokens))
    ((eq (first tokens) 'IDENTIFIER) (values t (rest tokens)))
    ((eq (first tokens) 'VALUEF) (values t (rest tokens)))
    ((eq (first tokens) 'VALUEI) (values t (rest tokens)))
    ((parse-arithmetic-exp tokens))
    ((parse-boolean tokens))
    ((parse-logical-exp tokens))
    ((parse-fcall tokens))
    ((parse-function-def tokens))
    ((parse-list tokens))
    ((parse-assignment tokens))
    ((parse-control-statement tokens))
    ((parse-display tokens))
    ((parse-exit tokens))
    ((parse-load tokens))
    (t (values nil tokens))))

(defun parse-arithmetic-exp (tokens)
  (cond
    ((and (eq (first tokens) 'OP_OP) (eq (second tokens) 'OP_PLUS))
     (multiple-value-bind (expr1 remaining-tokens1) (parse-expr (cddr tokens))
       (multiple-value-bind (expr2 remaining-tokens2) (parse-expr remaining-tokens1)
         (if (and expr1 expr2 (eq (first remaining-tokens2) 'OP_CP))
             (values t (rest remaining-tokens2))
             (values nil tokens)))))
    ;; Diğer aritmetik ifadeler için benzer kontroller ekleyin.
    (t (values nil tokens))))

(defun parse-boolean (tokens)
  (cond
    ((eq (first tokens) 'KW_TRUE) (values t (rest tokens)))
    ((eq (first tokens) 'KW_FALSE) (values t (rest tokens)))
    (t (values nil tokens))))

(defun parse-logical-exp (tokens)
  (cond
    ((and (eq (first tokens) 'OP_OP) (eq (second tokens) 'KW_AND))
     (multiple-value-bind (expr1 remaining-tokens1) (parse-expr (cddr tokens))
       (multiple-value-bind (expr2 remaining-tokens2) (parse-expr remaining-tokens1)
         (if (and expr1 expr2 (eq (first remaining-tokens2) 'OP_CP))
             (values t (rest remaining-tokens2))
             (values nil tokens)))))
    ;; Diğer mantıksal ifadeler için benzer kontroller ekleyin.
    (t (values nil tokens))))

(defun parse-fcall (tokens)
  (if (and (eq (first tokens) 'OP_OP) (eq (second tokens) 'IDENTIFIER))
      (multiple-value-bind (expr remaining-tokens) (parse-expr (cddr tokens))
        (if (and expr (eq (first remaining-tokens) 'OP_CP))
            (values t (rest remaining-tokens))
            (values nil tokens)))
      (values nil tokens)))

(defun parse-function-def (tokens)
  (if (and (eq (first tokens) 'OP_OP) (eq (second tokens) 'KW_DEFFUN))
      (let ((remaining-tokens (cddr tokens)))
        (if (and (eq (first remaining-tokens) 'IDENTIFIER)
                 (eq (second remaining-tokens) 'OP_OP)
                 (eq (third remaining-tokens) 'IDENTIFIER)
                 (eq (fourth remaining-tokens) 'OP_CP))
            (multiple-value-bind (expr remaining-tokens2) (parse-expr (cddddr remaining-tokens))
              (if (and expr (eq (first remaining-tokens2) 'OP_CP))
                  (values t (rest remaining-tokens2))
                  (values nil tokens)))
            (values nil tokens)))
      (values nil tokens)))

(defun parse-list (tokens)
  (cond
    ((eq (first tokens) 'KW_NIL) (values t (rest tokens)))
    ((and (eq (first tokens) 'OP_OP) (eq (second tokens) 'KW_LIST) (eq (third tokens) 'OP_CP))
     (values t (cdddr tokens)))
    ;; Diğer liste ifadeleri için benzer kontroller ekleyin.
    (t (values nil tokens))))

(defun parse-assignment (tokens)
  (if (and (eq (first tokens) 'OP_OP) (eq (second tokens) 'KW_SET))
      (let ((remaining-tokens (cddr tokens)))
        (if (and (eq (first remaining-tokens) 'IDENTIFIER))
            (multiple-value-bind (expr remaining-tokens2) (parse-expr (rest remaining-tokens))
              (if (and expr (eq (first remaining-tokens2) 'OP_CP))
                  (values t (rest remaining-tokens2))
                  (values nil tokens)))
            (values nil tokens)))
      (values nil tokens)))

(defun parse-control-statement (tokens)
  (cond
    ((and (eq (first tokens) 'OP_OP) (eq (second tokens) 'KW_IF))
     (multiple-value-bind (expr1 remaining-tokens1) (parse-expr (cddr tokens))
       (multiple-value-bind (expr2 remaining-tokens2) (parse-expr remaining-tokens1)
         (if (eq (first remaining-tokens2) 'OP_CP)
             (values t (rest remaining-tokens2))
             (multiple-value-bind (expr3 remaining-tokens3) (parse-expr remaining-tokens2)
               (if (and expr3 (eq (first remaining-tokens3) 'OP_CP))
                   (values t (rest remaining-tokens3))
                   (values nil tokens)))))))
    ;; Diğer kontrol ifadeleri için benzer kontroller ekleyin.
    (t (values nil tokens))))

(defun parse-display (tokens)
  (if (and (eq (first tokens) 'OP_OP) (eq (second tokens) 'KW_DISP))
      (multiple-value-bind (expr remaining-tokens) (parse-expr (cddr tokens))
        (if (and expr (eq (first remaining-tokens) 'OP_CP))
            (values t (rest remaining-tokens))
            (values nil tokens)))
      (values nil tokens)))

(defun parse-exit (tokens)
  (if (and (eq (first tokens) 'OP_OP) (eq (second tokens) 'KW_EXIT) (eq (third tokens) 'OP_CP))
      (values t (cdddr tokens))
      (values nil tokens)))

(defun parse-load (tokens)
  (if (and (eq (first tokens) 'OP_OP) (eq (second tokens) 'KW_LOAD))
      (let ((remaining-tokens (cddr tokens)))
        (if (and (eq (first remaining-tokens) 'IDENTIFIER) (eq (second remaining-tokens) 'OP_CP))
            (values t (cdddr remaining-tokens))
            (values nil tokens)))
      (values nil tokens)))

(defun syntax-check (line)
  (let ((tokens (tokenize line)))
    (multiple-value-bind (expr remaining-tokens) (parse-expr tokens)
      (if (and expr (null remaining-tokens))
          (format t "Syntax is correct.~%")
          (format t "Syntax error.~%")))))

(defun read-eval-syntax-check-loop ()
  (loop
    (format t "> ")
    (let ((line (read-line *standard-input* nil)))
      (when line (syntax-check line)))))

(defun gppsyntax (&optional (filename nil))
  (if filename
      (progn
        (with-open-file (stream filename)
          (loop for line = (read-line stream nil)
                while line do
                (syntax-check line)))
        (read-eval-syntax-check-loop))
      (read-eval-syntax-check-loop)))