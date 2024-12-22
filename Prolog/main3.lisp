(defun variable-p (term)
  "Check if the term is a variable (a single uppercase letter)."
  (and (stringp term) (= (length term) 1) (upper-case-p (elt term 0))))


(defun unify (term1 term2 substitution)
  "Unifies two terms. If they are already unified, return the substitution."
  (format t "[DEBUG] Trying to unify: ~A with ~A~%" term1 term2)  ; Debugging line
  (cond ((equal term1 term2) 
         (format t "[DEBUG] Terms are equal, no change in substitution~%")  ; Debugging line
         substitution)  ; If they are the same, no change in substitution
        ((variable-p term1) 
         (format t "[DEBUG] Unifying variable ~A with term ~A~%" term1 term2)  ; Debugging line
         (unify-variable term1 term2 substitution))
        ((variable-p term2) 
         (format t "[DEBUG] Unifying variable ~A with term ~A~%" term2 term1)  ; Debugging line
         (unify-variable term2 term1 substitution))
        ((and (listp term1) (listp term2))  ; Both are lists (terms within a predicate)
         (unify-lists term1 term2 substitution))
        (t nil)))

(defun unify-variable (var term substitution)
  "Unifies a variable with a term, updating the substitution."
  (format t "[DEBUG] Unifying variable ~A with term ~A, current substitution: ~A~%" var term substitution)
  (if (assoc var substitution)
      (prolog-substitute var substitution term)
      (let ((new-substitution (cons (cons var term) substitution)))
        (format t "[DEBUG] Updated substitution: ~A~%" new-substitution)
        new-substitution)))

(defun prolog-substitute (var substitution term)
  "Substitute a variable in a term using a given substitution."
  (format t "[DEBUG] Substituting variable ~A in term ~A with substitution ~A~%" var term substitution)
  (cond ((equal term var) (cdr (assoc var substitution)))
        ((listp term) (mapcar (lambda (x) (prolog-substitute var substitution x)) term))
        (t term)))

(defun unify-lists (list1 list2 substitution)
  "Unifies two lists element by element."
  (format t "[DEBUG] Unifying lists: ~A with ~A~%" list1 list2)  ; Debugging line
  (if (or (null list1) (null list2))
      (if (and (null list1) (null list2)) 
          (progn (format t "[DEBUG] Lists are of equal length~%") substitution)  ; Debugging line
          nil)  ; Both lists must be the same length
      (let ((new-substitution (unify (car list1) (car list2) substitution)))
        (if new-substitution
            (unify-lists (cdr list1) (cdr list2) new-substitution)
            nil))))

(defun is-rule (axiom)
  "Check if axiom is a rule (contains '<')"
  (and (listp axiom) 
       (member "<" axiom :test #'equal)))

(defun match-axiom (axiom predicate args substitution)
  "Match an axiom with a query predicate and arguments."
  (format t "[DEBUG] Matching axiom: ~A with predicate ~A and args ~A~%" axiom predicate args)
  (if (is-rule axiom)
      (process-rule axiom predicate args substitution)
      (process-fact axiom predicate args substitution)))

(defun process-rule (rule predicate args substitution)
  "Process a rule, returning new queries and substitution if matched"
  (let* ((head (car rule))
         (body (cddr rule)))
    (when (equal (car head) predicate)
      (format t "[DEBUG] Rule head matches predicate~%")
      (let ((unified-subs (unify (cdr head) args substitution)))
        (when unified-subs
          (cons body unified-subs))))))

(defun process-fact (fact predicate args substitution)
  "Process a fact, returning substitution if matched"
  (let ((head (car fact)))
    (when (equal (car head) predicate)
      (format t "[DEBUG] Fact head matches predicate~%")
      (format t "[DEBUG] Trying to unify: ~A with ~A~%" (cdr head) args)
      (let ((unified-subs (unify (cdr head) args substitution)))
        (when unified-subs
          unified-subs)))))

(defun resolve (axioms query substitution)
  "Tries to resolve a query using the axioms, applying unification recursively."
  (format t "[DEBUG] Resolving query: ~A with substitution: ~A~%" query substitution)
  (if (null query)
      substitution
      (let* ((current-query (car query))
             (current-head (car current-query))
             (predicate (car current-head))
             (args (cdr current-head)))
        (loop for axiom in axioms
              for result = (match-axiom axiom predicate args substitution)
              when result
                do (return (if (eq (length query) 1)
                             result
                             (resolve axioms (cdr query) result)))
              finally (return nil)))))

(defun convert-substitution-to-list (substitution)
  "Converts a substitution list into a list of variable-value pairs."
  (when substitution
    (list (mapcar (lambda (pair) 
                    (list (car pair) (cdr pair))) 
                  substitution))))

(defun prolog_prove (axioms query)
  (let ((result (resolve axioms (list query) nil)))
    (when result
      (convert-substitution-to-list result))))

;; Test case remains the same

(let ((axioms '((( "father" "jim" "jill" ))
                (( "mother" "mary" "jill" ))
                (( "father" "samm" "jim" ))
                (( "ancestor" "X" "Y") "<" ("parent" "X" "Y"))
                (( "ancestor" "X" "Y") "<" ("ancestor" "X" "Z") ("ancestor" "Z" "Y"))
                (( "parent" "X" "Y") "<" ("mother" "X" "Y"))
                (( "parent" "X" "Y") "<" ("father" "X" "Y"))))
      (query '(("father" "X" "jim"))))
  (format t "~a" (prolog_prove axioms query)))