
;; Parsed expression datatypes

(define-datatype expression expression?
  [quote-exp (id quote?)]
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
  [lambda-improp-exp
    (id (list-checker improperlist?))
    (body (list-checker expression?))
  ]
  [lambda-sym-exp
    (id symbol?)
    (body (list-checker expression?))
  ]
  [if-exp 
    (condition expression?)
    (true expression?)
    (false expression?)
  ]
  [let-exp 
    (vars (list-of symbol?))
    (vals (list-of expression?))
    (bodies (list-of expression?))
  ]
  [lambda-exp 
    (params (list-checker symbol?))
    (bodies (list-checker expression?))
  ]
  )

(define quote?
  (lambda (val)
    (equal? val '())
    )
  )
  

;; environment type definitions

(define scheme-value?
  (lambda (x) #t))

(define-datatype environment environment?
  (empty-env-record)
  (extended-env-record
   (syms (list-of symbol?))
   (vals (list-of scheme-value?))
   (env environment?)))

; datatype for procedures.  At first there is only one
; kind of procedure, but more kinds will be added later.

(define-datatype proc-val proc-val?
  [prim-proc
   (name symbol?)
  ]
  [closure 
   (params (list-of symbol?))
   (bodies (list-of expression?))
   (env environment?)
   ]
  )
	  
	 
	
