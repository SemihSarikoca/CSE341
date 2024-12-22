

(defvar *facts* '()
  "Global list to store facts.")

(defvar *rules* '()
  "Global list to store rules.")

(defun variable-p (term)
  "Check if the term is a variable (a single uppercase letter)."
  (and (stringp term) (= (length term) 1) (upper-case-p (elt term 0))))

(defun unify (term1 term2 substitution)
  "Unify two terms with the given substitution."
  (format t "[DEBUG] Trying to unify: ~A with ~A~%" term1 term2)
  (cond ((equal term1 term2)
         (format t "[DEBUG] Terms are equal, no change in substitution~%")
         substitution)
        ((variable-p term1)
         (format t "[DEBUG] Unifying variable ~A with ~A~%" term1 term2)
         (unify-variable term1 term2 substitution))
        ((variable-p term2)
         (format t "[DEBUG] Unifying variable ~A with ~A~%" term2 term1)
         (unify-variable term2 term1 substitution))
        ((and (listp term1) (listp term2))
         (if (equal (first term1) (first term2))
             (unify-args (rest term1) (rest term2) substitution)
             nil))
        (t
         (format t "[DEBUG] Cannot unify ~A with ~A~%" term1 term2)
         nil)))

(defun unify-variable (var term substitution)
  "Unify a variable with a term, updating the substitution."
  (format t "[DEBUG] Unify-variable called with var: ~A and term: ~A~%" var term)
  (if (assoc var substitution)
      (let ((existing (cdr (assoc var substitution))))
        (format t "[DEBUG] Variable ~A already bound to ~A, attempting to unify with ~A~%" var existing term)
        (unify existing term substitution))
      (progn
        (if (occurs-check var term substitution)
            (progn
              (format t "[DEBUG] Occurs check failed for variable ~A and term ~A~%" var term)
              nil)
            (cons (cons var term) substitution)))))

(defun occurs-check (var term substitution)
  "Check if variable occurs in term to prevent infinite loops."
  (cond
    ((equal var term) t)
    ((and (listp term) (some (lambda (x) (occurs-check var x substitution)) term)) t)
    ((variable-p term)
     (let ((binding (assoc term substitution)))
       (and binding (occurs-check var (cdr binding) substitution))))
    (t nil)))

(defun unify-args (args1 args2 substitution)
  "Unify two lists of arguments."
  (if (and args1 args2)
      (let ((new-subst (unify (first args1) (first args2) substitution)))
        (if new-subst
            (unify-args (rest args1) (rest args2) new-subst)
            nil))
      (if (and (null args1) (null args2))
          substitution
          nil)))

(defun apply-substitutions (term substitutions)
  "Apply the current substitutions to the term."
  (cond
    ((variable-p term)
     (let ((binding (assoc term substitutions)))
       (if binding
           (apply-substitutions (cdr binding) substitutions)
           term)))
    ((listp term)
     (mapcar (lambda (x) (apply-substitutions x substitutions)) term))
    (t term)))

(defun is-rule (axiom)
  "Check if the axiom is a rule."
  (and (listp axiom)
       (member "<" axiom :test #'equal)))

(defun populate-database (axioms)
  "Populate the global *facts* and *rules* from axioms."
  (dolist (axiom axioms)
    (if (is-rule axiom)
        (let ((head (car axiom))
              (body (cddr axiom)))
          (push (cons head body) *rules*))
        (let ((fact (car axiom)))
          (push fact *facts*)))))

(defun resolve (goals substitution)
  "Resolve the current goals against the *facts* and *rules* with the given substitution."
  (format t "[DEBUG] Resolving goals: ~A with substitution: ~A~%" goals substitution)
  (if (null goals)
      (list substitution)
      (let ((current-goal (apply-substitutions (first goals) substitution))
            (rest-goals (rest goals))
            (solutions '()))
        ;; Search in facts
        (dolist (fact *facts* solutions)
          (let ((unified-subs (unify fact current-goal substitution)))
            (when unified-subs
              (dolist (result (resolve rest-goals unified-subs))
                (push result solutions)))))

        ;; Search in rules
        (dolist (rule *rules* solutions)
          (let ((head (car rule))
                (body (cdr rule)))
            (when (equal (first head) (first current-goal))
              (let ((new-substitutions (unify head current-goal substitution)))
                (when new-substitutions
                  (let ((expanded-goals (mapcar (lambda (pred)
                                                 (apply-substitutions pred new-substitutions))
                                               body))
                        (combined-goals (append expanded-goals rest-goals)))
                    (dolist (result (resolve combined-goals new-substitutions))
                      (push result solutions))))))))

        solutions)))

(defun convert-substitution-to-list (substitution)
  "Convert the substitution alist to the required list format."
  (mapcar (lambda (pair)
            (list (car pair) (cdr pair)))
          substitution))

(defun prolog_prove (axioms query)
  "Prove the query against the given axioms."
  ;; Reset global databases
  (setf *facts* '())
  (setf *rules* '())
  ;; Populate databases
  (populate-database axioms)
  ;; Resolve query
  (let ((results (resolve query nil)))
    (when results
      (mapcar #'convert-substitution-to-list results))))

;; Test case

(let ((axioms '((( "father" "jim" "jill" ))
                (( "mother" "mary" "jill" ))
                (( "father" "samm" "jim" ))
                (( "ancestor" "X" "Y") "<" ("parent" "X" "Y"))
                (( "ancestor" "X" "Y") "<" ("ancestor" "X" "Z") ("ancestor" "Z" "Y"))
                (( "parent" "X" "Y") "<" ("mother" "X" "Y"))
                (( "parent" "X" "Y") "<" ("father" "X" "Y"))))
      (query '(("parent" "X" "jim"))))
  (format t "~a~%" (prolog_prove axioms query))
  (format t "~a~%" *rules*)
  (format t "~a~%" *facts*))