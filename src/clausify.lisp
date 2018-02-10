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
;;;; termine      ::= <costante> | <simbolo> | <funzione>
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

;;;; Funzioni interfaccia:
;;;;   1.(as-cnf (fbf)) -> riscrive la fbf in una formula cnf
;;;;   2.(is-horn (fbf)) -> restituisce true se la fbf è una clausola di Horn


(defun is-term (iterm)
	(or
		(constp iterm)
		(variablep iterm)
		(funp iterm)))

