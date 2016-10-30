; This is a parser for simple Scheme expressions, such as those in EOPL, 3.1 thru 3.3.

; You will want to replace this with your parser that includes more expression types, more options for these types, and error-checking.

; Procedures to make the parser a little bit saner.
(define-datatype expression expression?
	[var-exp (id symbol?)]
  [quote-exp (id quote?)]
	[lambda-exp 
		(id (list-of symbol?))
		(bodies (list-of expression?))
	]
	[lambda-improp-exp
		(id improperlist?)
		(body (list-of expression?))
	]
	[lambda-sym-exp
		(id symbol?)
		(body (list-of expression?))
	]
	[app-exp
		(rator expression?)
		(rands (list-of expression?))
		]
	[lit-exp (id literal?)]
	[if-exp 
		(test-exp expression?)
		(then-exp expression?)
		(else-exp expression?)
	]
	[if-no-else-exp
		(test-exp expression?)
		(then-exp expression?)
	]
  [or-exp (bodies (list-of expression?))]
  [and-exp (bodies (list-of expression?))]
  [cond-exp (cases (list-of expression?))
            (bodies (list-of expression?))]
  [while-exp 
    (test-exp expression?)
    (bodies (list-of expression?))
  ]
  [let-name-exp (name symbol?)
                (vars (list-of symbol?))
                (vals (list-of expression?))
                (bodies (list-of expression?))
  ]
  ;**Change the code here
  [case-lambda-exp (exprs (list-of expression?))]
  [case-exp (val-expr expression?)
            (case-clause (lambda (x) (or (list? x) (eqv? x 'else))))
            (bodies (list-of expression?))]
  [begin-exp (bodies (list-of expression?))]
	[letrec-exp 
    (proc-names (list-of symbol?))
		(idss (list-of letrec-idss?))
    (bodiess (list-of (list-of expression?)))
		(letrec-bodies (list-of expression?))
	]
	[let-exp (vars (list-of symbol?))
           (val (list-of expression?))
		       (bodies (list-of expression?))
	]
  [set!-exp (var symbol?)
            (val expression?)]
  [define-exp (var symbol?)
              (val expression?)]
  [let*-exp (vars (list-of symbol?))
            (vals (list-of expression?))
            (bodies (list-of expression?))
  ]
  )

(define letrec-idss?
  (lambda (x)
    (or (list-of (symbol? x)) (improperlist? x))))


(define improperlist?
  (lambda (x)
    (and (pair? x) (not (list? x)))))

(define literal?
	(lambda (val)
		(or (number? val) (symbol? val) (boolean? val) (string? val))))

(define 1st car)
(define 2nd cadr)
(define 3rd caddr)


(define parse-exp         
  	(lambda (datum)
    	(cond
        [(symbol? datum) (var-exp datum)]
		    [(literal? datum) (lit-exp datum)]
        [(quote? datum) (quote-exp datum)]
		    [(not (list? datum)) (eopl:error 'parse-exp "application ~s is not a proper list" datum)]
		    [(pair? datum)
      			(cond
      				[(eqv? (1st datum) 'lambda) 
      				(cond 
      					[(< (length datum) 3) (eopl:error 'parse-exp "lambda-expression: incorrect length ~s" datum)]
      					[else 
      					(cond
      						[(list? (2nd datum)) 
      						(if (not (andmap symbol? (2nd datum))) 
      							(eopl:error 'parse-exp "lambda argument list: formals must be symbols: ~s" (2nd datum))
      							(lambda-exp (2nd datum) (map parse-exp (cddr datum)))
      						)]
      						[(symbol? (2nd datum)) (lambda-sym-exp (2nd datum) (map parse-exp (cddr datum)))]
      						[(improperlist? (2nd datum)) (lambda-improp-exp (2nd datum) (map parse-exp (cddr datum)))]
							)
      					]
      					)]
      				[(eqv? (1st datum) 'if) 
      				(cond 
      					[(> (length datum) 4) (eopl:error 'parse-exp "if-expression ~s does not have (only) test, then, and else" datum)]
      					[(<= (length datum) 2) (eopl:error 'parse-exp "if-expression ~s does not have (only) test, then, and else" datum)]
      					[(= (length datum) 3) (if-no-else-exp (parse-exp (2nd datum)) (parse-exp (3rd datum)))]
      					[else (if-exp (parse-exp (2nd datum)) (parse-exp (3rd datum)) (parse-exp (cadddr datum)))]
      					)]
      				[(eqv? (1st datum) 'let) 
      				(cond
      					[(<= (length datum) 2) (eopl:error 'parse-exp "~s-expression has incorrect length ~s" datum)]
      					[(improperlist? (2nd datum)) (eopl:error 'parse-exp "Error in parse-exp decls: not a proper list: ~s" (2nd datum))]
                [(symbol? (cadr datum)) (let-name-exp (cadr datum) (map car (3rd datum)) (map parse-exp (map cadr (3rd datum))) (map parse-exp (cdddr datum)))]
      					[(and (not (eqv? (2nd datum) '())) (ormap improperlist? (2nd datum))) (eopl:error 'parse-exp "Error in parse-exp: decls: not all proper lists: ~s" (2nd datum))]
      					[(not (andmap (lambda (ls) (eqv? (length ls) 2)) (2nd datum))) (eopl:error 'parse-exp "decls: not all length 2: ~s" (2nd datum))]
      					[(not (andmap symbol? (map car (2nd datum)))) (eopl:error 'parse-exp "decls: first members must be symbols: ~s" (2nd datum))]
      					[else (let-exp (map car (2nd datum)) (map parse-exp (map cadr (2nd datum))) (map parse-exp (cddr datum)))]
      					)]
      				[(eqv? (1st datum) 'let*)
      				(cond
      					[(not (list? (2nd datum))) (eopl:error 'parse-exp "Error in parse-exp: let* declarations not a list" datum)]
      					[(<= (length datum) 2) (eopl:error 'parse-exp "~s-expression has incorrect length ~s" datum)]
      					[else 
      					(cond
      						[(not (andmap (lambda (ls) (eqv? (length ls) 2)) (2nd datum))) (eopl:error 'parse-exp "decls: not all length 2: ~s" (2nd datum))]
      						[(not (andmap symbol? (map car (2nd datum)))) (eopl:error 'parse-exp "decls: first members must be symbols: ~s" (2nd datum))]
      						[else (let*-exp (map car (2nd datum)) (map parse-exp (map cadr (2nd datum))) (map parse-exp (cddr datum)))]
      						)
      					]
      					)
      				]
      				[(eqv? (1st datum) 'letrec)
      				(cond
      					[(<= (length datum) 2) (eopl:error 'parse-exp "Error in parse-expression: letrec expression: incorrect length: ~s" datum)]
      					[(not (list? (2nd datum))) (eopl:error 'parse-exp "Error in parse-exp: letrec: declarations is not a list" datum)]
      					[(not (andmap (lambda (ls) (eqv? (length ls) 2)) (2nd datum))) (eopl:error 'parse-exp "decls: not all length 2: ~s" (2nd datum))]
      					[(not (andmap symbol? (map car (2nd datum)))) (eopl:error 'parse-exp "decls: first members must be symbols: ~s" (2nd datum))]
      					[else (letrec-exp (map car (2nd datum)) 
                                  (map cadr (map cadr (2nd datum))) 
                                  (map (lambda (x) parse-exp (map parse-exp x))
                                    (map cddr (map cadr (2nd datum)))) 
                                  (map parse-exp (cddr datum)))])]
      				[(eqv? (1st datum) 'set!) 
      				(cond
      					[(= (length datum) 3) (set!-exp (2nd datum) (parse-exp (3rd datum)))]
      					[(<= (length datum) 2) (eopl:error 'parse-exp "set! expression ~s does not have (only) variable and expression" datum)]
      					[(> (length datum) 3) (eopl:error 'parse-exp "set! expression ~s is too long" datum)]
      					)]
              [(eqv? (1st datum) 'cond)
                (let ([last (cdr datum)])
                     (cond-exp (map parse-exp (map car last)) 
                               (map parse-exp (map cadr last))))]
              [(eqv? (1st datum) 'begin)
               (begin-exp (map parse-exp (cdr datum)))]
              [(eqv? (1st datum) 'or)
               (or-exp (map parse-exp (cdr datum)))]
              [(eqv? (1st datum) 'and)
               (and-exp (map parse-exp (cdr datum)))]
              [(eqv? (1st datum) 'while)
               (while-exp (parse-exp (2nd datum)) (map parse-exp (cddr datum)))]
              [(eqv? (1st datum) 'case)
               (case-exp (parse-exp (2nd datum)) 
                         (map car (cddr datum))
                         (map parse-exp (map cadr (cddr datum))))]
               ;**change code here.
              [(eqv? (1st datum) 'case-lambda)
               (case-lambda-exp (map lambdas-process (cdr datum)))]
              [(eqv? (1st datum) 'set!)
               (set!-exp (2nd datum) (parse-exp (3rd datum)))]
              [(eqv? (1st datum) 'define)
               (define-exp (cadr datum) (parse-exp (3rd datum)))]       
       				[else (app-exp (parse-exp (1st datum)) (map parse-exp (cdr datum)))]
       				)
      			]
     		[else (eopl:error 'parse-exp "bad expression: ~s" datum)])))

;;change codeh here!
(define lambdas-process
    (lambda (x) 
      (cond [(list? (car x)) (lambda-exp (car x) (map parse-exp (cdr x)))]
            [(improperlist? (car x)) (lambda-improp-exp (car x) (map parse-exp (cdr x)))]
            [else (lambda-sym-exp (car x) (map parse-exp (cdr x)))])))

(define unparse-exp
  (lambda (exp)
    (cases expression exp
      [var-exp (datum) datum]
      [lambda-sym-exp (id body) (append (list 'lambda id) (map unparse-exp body))]
      [lambda-exp (id body) (append (list 'lambda id) (map unparse-exp body))]
      [lambda-improp-exp (id body) (list 'lambda id (map unparse-exp body))]
      [app-exp (rator rand) (cons (unparse-exp rator) (map unparse-exp rand))]
      [lit-exp (id) id]
      [if-exp (condition true false) (list 'if (unparse-exp condition) (unparse-exp true) (unparse-exp false))]
      [if-no-else-exp (condition true) (list 'if (unparse-exp condition) (unparse-exp true))]
      [letrec-exp (id body) (append (list 'letrec (map l-id-process-for-unparse id)) (map unparse-exp body))]
      [let-exp (id body) (append (list 'let (map l-id-process-for-unparse id)) (map unparse-exp body))]
      [let*-exp (id body) (append (list 'let* (map l-id-process-for-unparse id)) (map unparse-exp body))]
      [set!-exp (var val) (list var (unparse-exp val)) ]
      )
    )
  )



; (define l-id-process-for-unparse
;   (lambda (x) 
;     (list (1st x) (unparse-exp (2nd x)))
;     )
;   )

; (define l-id-process
;   (lambda (x) 
;     (list (1st x) (parse-exp (2nd x)))
;     )
;   )

(define quote?
  (lambda (val)
    (equal? (car val) 'quote)))




