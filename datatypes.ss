
;; Parsed expression datatypes

(define-datatype expression expression?
  [var-exp        ; variable references
   (id symbol?)]
  [lit-exp        ; "Normal" data.  Did I leave out any types?
   (datum
    (lambda (x)
      (ormap 
       (lambda (pred) (pred x))
       (list number? vector? boolean? symbol? string? pair? null?))))]
  [app-exp        ; applications
   (rator expression?)
   (rands (list-of expression?))]  
  )

(define-datatype environment environment?
  [empty-env-record]
  [extended-env-record
   (syms (list-of symbol?))
   (vals (list-of scheme-value?))
   (env environment?)]
  [recursively-extended-env-record
   (proc-names (list-of symbol?))
   (idss (list-of letrec-idss?))
   (bodiess (list-of (list-of expression?)))
   (env environment?)])
	
; datatype for procedures.  At first there is only one
; kind of procedure, but more kinds will be added later.
(define-datatype proc-val proc-val?
  [prim-proc
   (name symbol?)]
  [closure (vars (lambda (obj) 
                         (or ((list-of symbol?) obj)
                             (symbol? obj)
                             (pair? obj))))
           (bodies (list-of expression?))
           (env environment?)]
  ;**change code here.
  [case-closure (lambdas (list-of lambdas?))
                (env environment?)])

;**change code here
(define lambdas?
  (lambda (obj)
    (cases expression obj
      [lambda-exp (id bodies) #t]
      [lambda-improp-exp (id bodies) #t]
      [lambda-sym-exp (id bodies) #t]
      [var-exp (id) #f]
      [quote-exp (id) #f]
      [app-exp (rator rands) #f]
      [lit-exp (id) #f]
      [if-exp (test-exp then-exp else-exp) #f]
      [if-no-else-exp (test-exp then-exp) #f]
      [or-exp (bodies) #f]
      [and-exp (bodies) #f]
      [cond-exp (cases bodies) #f]
      [while-exp (test-exp bodies) #f]
      [let-name-exp (name vars vals bodies) #f]
      [case-lambda-exp (expr) #f]
      [case-exp (val-expr case-clause bodies) #f]
      [begin-exp (bodies) #f]
      [letrec-exp (proc-names idss bodiess letrec-bodies) #f]
      [let-exp (vars val bodies) #f])))

	
;; environment type definitions
(define scheme-value?
  (lambda (x) #t))
