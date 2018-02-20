;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;                                                                              ;
;                                CLAUSIFY-LISP                                 ;
;                                                                              ;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


;;;; MATRICOLA 735390 GUCCIARDI ALBERTO
;;;; MATRICOLA 702504 ARDIIT 
;;;; MATRICOLA 060678 GERALD

;;;; Qui di seguito le definizioni delle componenti che costituiscono le fbf
;;;;
;;;; termine      ::= <costante> | <variabile> | <funzione>
;;;; costante     ::= <numeri> | <simboli che cominciano con una lettera>
;;;; variabile    ::= <simboli che cominciano con un ?>
;;;; funzione     ::= '(' <simbolo> <termine>+ ')'
;;;; fbf          ::= <predicato>
;;;;              | <negazione> | <congiuinzione>
;;;;              | <disgiunzione> | <implicazione>
;;;;              | <universale> | <esistenziale>
;;;; predicato    ::= <simbolo che comincia con una lettera>
;;;;              | '(' <simbolo> <termine>+ ')'
;;;; negazione    ::= '(' not <fbf> ')'
;;;; congiunzione ::= '(' and <fbf> <fbf> ')'
;;;; disgiunzione ::= '(' or <fbf> <fbf> ')'
;;;; implicazione ::= '(' implies <fbf> <fbf> ')'
;;;; universale   ::= '(' every <variabile> <fbf> ')'
;;;; esistenziale ::= '(' exist <variabile> <fbf> ')'

;;;; Regole di sostituzione
;;;; 1.  (not (not p)) -> p
;;;; 2.  (not (and p q)) -> (or (not p) (not q))
;;;; 3.  (not (or p q)) -> (and (not p) (not q))
;;;; 4.  (not (every ?x (p ?x))) -> (every ?x (not (p ?x)))
;;;; 5.  (not (exist ?x (p ?x))) -> (exist ?x (not (p ?x)))
;;;; 6.  (implies p q) -> (or (not p) q)
;;;; 7.  (or p (and q w)) -> (and (or p q) (or p w))
;;;; 9.  (exist ?x (p ?x) -> (p sk<progressive>)
;;;; 10. (every ?y (exist ?x (p ?x ?y)) -> (every ?y (p sf<pr> ?y) ?y)

;;;; Funzioni di riconoscimento delle variabili costanti e dei simboli
;;;; 1.(is-operator (opr))
;;;; 2.(is-letter (iterm))
;;;; 3.(is-symbol (iterm))
;;;; 4.(is-variable (var))
;;;; 5.(is-fun (iterm))
;;;; 6.(is-const (iterm))
;;;; 7.(is-term (iterm))
;;;; 8.(is-literal (iform))
;;;; 9.(arity (iform))
;;;; 10.(is-wff (iform))
;;;; Funzioni che implementano le regole di sostituzione
;;;; 1.
;;;; 2.

;;;; Funzioni interfaccia
;;;; 1.(as-cnf (fbf)) -> riscrive la fbf in una formula cnf
;;;; 2.(is-horn (fbf)) -> restituisce true se la fbf e' una clausola di Horn

;;;; Restituisce true se oper e' un operatore logico
(defun is-operator (opr)
  (or
   (eq opr 'not)
   (eq opr 'and)
   (eq opr 'or)
   (eq opr 'implies)
   (eq opr 'every)
   (eq opr 'exist)))


(defun is-letter (iterm)
  (not (parse-integer (symbol-name iterm) :start 0 :end 1 :junk-allowed t)))

;;;; Restituisce true se iterm e' un simbolo che inizia con una lettera

(defun is-symbol (iterm)
  (and 
   (symbolp iterm)
   (is-letter iterm)))

;;;; 
(defun is-variable (var)
  (and
   (not (listp var))
   (not (is-operator var))
   (symbolp var)
   (char= #\? (char (symbol-name var) 0))))

(defun is-fun (iterm)
  (and
   (not (null iterm))
   (symbolp (first iterm))
   (not (is-operator (first iterm)))
   (every 'is-term (rest iterm))))

;;;; Restituisce true se iterm contiene è una costante
(defun is-const (iterm)
  (and
   (not (listp iterm))
   (not (is-variable iterm))
   (not (is-operator iterm))
   (or
    (numberp iterm)
    (is-letter iterm))))

;;;; Restituisce true se iterm e' una costante, una variabile oppure una funzione
(defun is-term (iterm)
	(or
		(is-const iterm)
		(is-variable iterm)
		(and (listp iterm) (is-fun iterm))))

;;;; Restituisce true se iform è un letterale
(defun is-literal (iform)
  (or
   (is-term iform)
   (and
    (eq (first iform) 'not)
    (is-term (second iform)))))


;;;; Ritorna l'arità di una formula 
(defun arity (iform)
  (if (listp iform) (- (list-length iform) 1)))

;;;; Restituisce true iform è una formula ben formata
;;;; 
(defun is-wff (iform)
  (cond
    ((is-term iform) T)

    (T
     (or
      (and
       (= (arity iform) 2)
       (member (first iform) '(and or implies))
       (is-wff (second iform))
       (is-wff (third iform)))

      (and
       (= (arity iform) 2)
       (member (first ifrom) '(exist every))
       (is-variable (second iform))
       (is-wff (third iform)))
      (and
       (= (arity iform) 1)
       (member (first iform) '(not))
       (is-wff (second iform)))))))

;;;;

(defun distributivity (iform)
  (cond
    ((not (listp iform)) iform)
    ((and 
      (eq (list-length iform) 3)
      (listp (second iform))
      (eq (first iform) 'or)
      (eq (first (second iform)) 'and))
     (list 'and
	   (list 'or (dist (second (second iform))) (dist (third iform)))
	   (list 'or (dist (third (second iform))) (dist (third iform)))))
    ((and
      (eq (list-length iform) 3)
      (listp (third iform))
      (eq (first iform) 'or)
      (eq (first (third iform) 'and)))
     (list 'and
	   (list 'or (dist (second iform)) (dist (second (third iform))))
	   (list 'or (dist (second iform)) (dist (third (third iform))))))
    (T iform)))

;;;; Functions for skolemization of constant and functions
(defun skolem-vars ()
  (gentemp "SV-"))

(defun skolem-function* (&rest args)
  (cons (gentemp "SF-") args))

(defun skolem-function (args)
  (apply #'skolem-function* args))


(defun rewrite (iform &optional univars)
  (cond
    ((not (listp iform)) iform)
    ((eq (first iform) 'implies) (rewrite-implication iform univars))
    ((eq (first iform) 'not) (rewrite-not iform univars))
    ((eq (first iform) 'every) (rewrite-every iform univars))
    ((eq (first iform) 'exist) (rewrite-exist iform univars))
    ((eq (first iform) 'and)
     (list 'and (rewrite (second iform) univars)
	   (rewrite (third iform) univars)))
    ((eq (first iform) 'or)
     (list 'or (rewrite (second iform) univars)
	   (rewrite (third iform) univars)))
    (T f)))

(defun rewrite-implication (iform &optional univars)
  (rewrite (list 'or (list 'not (second iform)) (third iform)) univars))

(defun rewrite-not (iform &optional univars)
  (cond
    ((not (listp (second iform))) iform)
    ((eq (first (second iform)) 'not) (rewrite (second (second iform)) univars))
    ((eq (first (second iform)) 'and)
     (rewrite (list 'or (list 'not (second (second iform)))
		    (list 'not (third (second iform)))) univars))
    ((eq (first (second iform)) 'or)
     (rewrite (list 'and (list 'not (second (second iform)))
		    (list 'not (third (second iform)))) univars))
    ((eq (first (second iform)) 'implies)
     (rewrite (list 'implies (list 'not (second (second iform)))
		    (list 'not (third (second iform)))) univars))
    ((eq (first (second iform)) 'every)
     (rewrite (list 'exist (list 'not (second (second iform)))
		    (list 'not (third (second iform)))) univars))
    ((eq (first (second iform)) 'exist)
     (rewrite (list 'every (list 'not (second (second iform)))
		    (list 'not (third (second iform)))) univars))
    (T iform)))

(defun rewrite-every (iform &optional univars)
  (rewrite (third f) (append univars (list (second iform)))))

(defun rewrite-exist (iform &optional univars)
  (cond
    ((null univars)
     (rewrite (substitute (skolem-vars) (second iform) (third iform))
	      univars))
    (T (rewrite (subst (skolem-function univars) (second iform)
		       (third iform)) univars))))

    

;;;; 
(defun simplify (iform)
  (cond
    ((not (listp iform)) f)
    (T
     )))


