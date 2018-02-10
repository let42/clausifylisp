(defun termp (term)
	(or
		(constp term)
		(variablep term)
		(and (listp term) (funp term))))

(defun variablep (v)
	(and 
		(not (operatorp v))
		(not (listp v))
		(symbolp v) 
		(char= #\? (char (symbol-name v) 0))))

(defun constp (term)
	(and 
		(not (listp term))
		(not (variablep term))
		(not (operatorp term))
		(or
			(numberp term)
			(letterp term))))

(defun letterp (term)
 	(not (parse-integer (symbol-name term) :start 0 :end 1 :junk-allowed t)))

(defun funp (term)
	(and 
		(not (null term)) 
		(symbolp (first term))
		(not (operatorp (first term)))
		(every 'termp (rest term))))

(defun operatorp (op)
	(or 
        		(eq op 'not)
        		(eq op 'and)
		(eq op 'or)
		(eq op 'implies)
       		(eq op 'every)
        		(eq op 'exist)))

(defun arity (f)
	(if (listp f) (- (list-length f) 1)))

(defun is-wff (f)
	(cond
		((termp f) T)

		(T (or 
			(and 
			(= (arity f) 2)
			(member (first f) '(and or implies))
			(is-wff (second f))
			(is-wff (third f)))

			(and 
			(= (arity f) 2)
			(member (first f) '(exist every))
			(variablep (second f))
			(is-wff (third f)))

			(and
			(= (arity f) 1)
			(member (first f) '(not))
			(is-wff (second f)))))))


;
; --- Rewrite rules
;

(defun rew(f &optional univars)
	(cond
		((not (listp f)) f) 
		((eq (first f) 'implies) (rew-implication f univars))
		((eq (first f) 'not) (rew-not f univars))
		((eq (first f) 'every) (rew-every f univars))
		((eq (first f) 'exist) (rew-exist f univars))
		((eq (first f) 'and) 
			(list 'and (rew (second f) univars) (rew (third f) univars)))
		((eq (first f) 'or) 
			(list 'or(rew (second f) univars) (rew (third f) univars)))
		(T f)))

;; Implication in terms of or

(defun rew-implication (f &optional univars)
	(rew (list 'or (list 'not (second f)) (third f)) univars))

;; Negation inwards
(defun rew-not (f &optional univars)
	(cond 
		((not (listp (second f))) f)
		((eq (first (second f)) 'not) (rew (second (second f)) univars))
		((eq (first (second f)) 'and)
			(rew (list 'or (list 'not (second (second f)))
				      (list 'not (third (second f)))) univars))
		((eq (first (second f)) 'or)
			(rew (list 'and (list 'not (second (second f)))
				      (list 'not (third (second f)))) univars))
		((eq (first (second f)) 'implies)
			(rew (list 'implies (list 'not (second (second f)) 
				      (list 'not (third (second f))))) univars))
		((eq (first (second f)) 'every)
			(rew (list 'exist (second (second f))
				      (list 'not (third (second f)))) univars))
		((eq (first (second f)) 'exist)
			(rew (list 'every (second (second f))
				      (list 'not (third (second f)))) univars))
		(T f)))

;; Drop universal quantifier
(defun rew-every (f &optional univars)
	(rew (third f) (append univars (list (second f)))))

;; Skolemize existential quantifier
(defun rew-exist (f &optional univars)
	(cond
		((null univars) 
			(rew (substitute (skolem-variable) (second f) (third f)) univars))
		(T (rew (subst (skolem-function univars) (second f) (third f)) univars))))


;
; -- Distributivity law
;

(defun dist (f)
	(cond
		((not (listp f)) f)
		((and
			(eq (list-length f) 3)
			(listp (second f))
			(eq (first f) 'or)
			(eq (first (second f)) 'and))
		(list 'and 
			(list 'or (dist (second (second f))) (dist (third f)))
			(list 'or (dist (third (second f))) (dist (third f)))))

		((and
			(eq (list-length f) 3)
			(listp (third f))
			(eq (first f) 'or)
			(eq (first (third f)) 'and))
		(list 'and 
			(list 'or (dist (second f)) (dist (second (third f))))
			(list 'or (dist (second f)) (dist (third (third f))))))
		(T f)))

;
; --- Binary conjunction and disjunction to n-ary
;

(defun simplify (f)
	(cond
		((not (listp f)) f)

;; Case (X a b) => (X a b)
		((and
			(member (first f) '(and or))
			(is-literal (second f))
			(is-literal (third f))) 
		f)

;; Case (X (X a b) c) => (X a b c)
		((and
			(member (first f) '(and or))
			(not (is-literal (second f)))
			(eq (first f) (first (second f)))
			(is-literal (third f)))
		(append (simplify (second f)) (list (third f))))

;; Case (X a (X b c)) => (X a b c)
		((and
			(member (first f) '(and or))
			(not (is-literal (third f)))
			(eq (first f) (first (third f)))
			(is-literal (second f)))
		(append (list (first f)) 
			(list (second f)) 
			(rest (simplify (third f)))))

;; Case (X (Y a b) c) => (X (Y a b) c)
		((and
			(member (first f) '(and or))
			(not (is-literal (second f)))
			(not (eq (first f) (first (second f))))
			(is-literal (third f)))
		(append (list (first f)) 
			(list (simplify (second f))) 
			(list (third f))))

;; Case (X a (Y b c)) => (X a (Y b c))
		((and
			(member (first f) '(and or))
			(not (is-literal (third f)))
			(not (eq (first f) (first (third f))))
			(is-literal (second f)))
		(append (list (first f)) 
			(list (second f)) 
			(list (simplify (third f)))))

;; Case (X (X a b) (X c d)) => (X a b c d)
		((and
			(member (first f) '(and or))
			(not (is-literal (second f)))
			(not (is-literal (third f)))
			(eq (first f) (first (second f)))
			(eq (first f) (first (third f))))
		(append (simplify (second f)) 
			(rest (simplify (third f)))))

;; Case (X (X a b) (Y b c)) => (X a b (Y b c))
		((and
			(member (first f) '(and or))
			(not (is-literal (second f)))
			(not (is-literal (third f)))
			(eq (first f) (first (second f)))
			(not (eq (first f) (first (third f)))))
		(append (simplify (second f)) 
			(list (simplify (third f)))))

;; Case (X (Y a b) (X c d)) => (X (Y a b) c d)
		((and
			(member (first f) '(and or))
			(not (is-literal (second f)))
			(not (is-literal (third f)))
			(eq (first f) (first (third f)))
			(not (eq (first f) (first (second f)))))
		(append (list (first f)) 
			(list (simplify (second f))) 
			(rest (simplify (third f)))))

;; Case (X (Y a b) (Y c d)) => (X (Y a b) (Y c d))
		((and
			(member (first f) '(and or))
			(not (is-literal (second f)))
			(not (is-literal (third f)))
			(not (eq (first f) (first (third f))))
			(not (eq (first f) (first (second f)))))
		(append (list (first f)) 
			(list (simplify (second f))) 
			(list (simplify (third f)))))
		(T f)))


(defun is-literal (f)
	(or
		(termp f)
		(and 
			(eq (first f) 'not)
			(termp (second f)))))

;
; --- CNF converter
;

(defun tocnf (fbf)
	(if (is-wff fbf)
		(simplify (dist (rew fbf)))))

;
; --- Horn checker
;

(defun is-horn (fbf)
	(is-horn-cnf (tocnf fbf)))

(defun is-horn-cnf (fbf)
	(cond
		((null fbf) nil)
		((and (listp fbf) (eq (first fbf) 'and)) (is-horn-clause (rest fbf)))
		(T (is-horn-clause fbf))))

(defun is-horn-clause (clause)
	(or
		(and
			(listp clause)
			(eq (first clause) 'or)
			(<= (positive-lit (rest clause)) 1))
		(and
			(is-literal clause)
			(<= (positive-lit (list clause)) 1))))


(defun positive-lit (literals)
	(cond
		((null literals) 0)
		((null (rest literals))
			(if (and (listp (first literals)) (eq (first (first literals)) 'not)) 
				0
				1))
		(T (+ (positive-lit (list (first literals))) (positive-lit (rest literals))))))

	
;
; --- Generate skolem constants or functions
;

(defun skolem-variable () 
	(gentemp "SV-"))
(defun skolem-function* (&rest args) 
	(cons (gentemp "SF-") args))
(defun skolem-function (args)
(apply #'skolem-function* args))