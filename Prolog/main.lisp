(defun variable-p (term)
  (and (stringp term) (= (length term) 1) (upper-case-p (elt term 0))))

(defun unify (term1 term2 substitution)
  (cond ((equal term1 term2)
         substitution)
        ((variable-p term1)
         (unify-variable term1 term2 substitution))
        ((variable-p term2)
         (unify-variable term2 term1 substitution))
        ((and (listp term1) (listp term2))
         (unify-lists term1 term2 substitution))
        (t nil)))

(defun unify-variable (var term substitution)
  "Unifies a variable with a term, updating the substitution."
  (if (assoc var substitution)
      (apply-substitution var substitution term)
      (cons (cons var term) substitution)))

(defun apply-substitution (var substitution term)
  "Substitute a variable in a term using a given substitution."
  (cond ((equal term var) (cdr (assoc var substitution)))
        ((listp term) (mapcar (lambda (x) (apply-substitution var substitution x)) term))
        (t term)))

(defun unify-lists (list1 list2 substitution)
  (if (or (null list1) (null list2))
      (if (and (null list1) (null list2)) substitution nil)
      (let ((new-substitution (unify (car list1) (car list2) substitution)))
        (if new-substitution
            (unify-lists (cdr list1) (cdr list2) new-substitution)
            nil))))

(defun match-axiom (axiom predicate args substitution)
  (if (listp axiom)
      (let ((head (car axiom))
            (body (cdr axiom)))
        (if (equal predicate (car head))
            (let ((axiom-args (cdr head)))
              (when (= (length axiom-args) (length args))
                (unify axiom-args args substitution)))))))

(defun resolve (axioms query substitution)
  (if (null query)
      (convert-substitution-to-list substitution)
      (let* ((current-query (car query))
             (predicate (car current-query))
             (args (cdr current-query)))
        (loop for axiom in axioms
              for result = (match-axiom axiom predicate args substitution)
              when result
                do (return (if (eq (length query) 1)
                               (convert-substitution-to-list result)
                               (resolve axioms (cdr query) result)))
              finally (return nil)))))

(defun convert-substitution-to-list (substitution)
  (when substitution
    (mapcar (lambda (pair) (list (car pair) (cdr pair))) substitution)))

(defun prolog_prove (axioms query)
  (resolve axioms query nil))



(let ((axioms '((( "father" "jim" "jill" ))
                (( "mother" "mary" "jill" ))
                (( "father" "samm" "jim" ))
                (( "ancestor" "X" "Y") "<" ("parent" "X" "Y"))
                (( "ancestor" "X" "Y") "<" ("ancestor" "X" "Z") ("ancestor" "Z" "Y"))
                (( "parent" "X" "Y") "<" ("mother" "X" "Y"))
                (( "parent" "X" "Y") "<" ("father" "X" "Y"))))
      (query '(("father" "X" "jill"))))
  (format t "~a" (prolog_prove axioms query)))