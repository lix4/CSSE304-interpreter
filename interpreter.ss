; top-level-eval evaluates a form in the global environment

(define top-level-eval
  (trace-lambda top-level-eval(form)
    ; later we may add things that are not expressions.
    (eval-exp form init-env)))

; eval-exp is the main component of the interpreter


(define eval-exp
  (trace-lambda eval-exp(exp env)
    (cases expression exp
      [lit-exp (datum) datum]
      [quote-exp (datum) (cadr datum)]
      [app-exp (rator rands)
        (let ([proc-value (eval-exp rator env)]
              [args (eval-rands rands env)])
            (apply-proc proc-value args)
          )] 
      [var-exp (id)
				(apply-env env id; look up its value.
      	   (lambda (x) x) ; procedure to call if id is in the environment 
           (lambda () (eopl:error 'apply-env ; procedure to call if id not in env
		          "variable not found in environment: ~s"
			   id)))]
      [if-exp (condition true false) 
       (if (eval-exp condition env)
          (eval-exp true env)
          (eval-exp false env)
        )
      ]
      [let-exp (vars vals bodies)
        (eval-bodies bodies
          (extend-env vars (eval-rands vals env) env)
          )
      ]
      [lambda-improp-exp
        (id improperlist?)
        (bodies (list-checker expression?))
      ]
      [lambda-sym-exp
        (id symbol?)
        (bodies (list-checker expression?))
      ]
      [lambda-exp (params bodies) (closure params bodies env)]
      [else (eopl:error 'eval-exp "Bad abstract syntax: ~a" exp)])))

; evaluate the list of operands, putting results into a list

; (define eval-rands
;   (lambda (rands env)
;     (map eval-exp rands)))

(define eval-rands
  (lambda (rands env)
    (if (null? rands)
        '()
        (cons (eval-exp (car rands) env) (eval-rands (cdr rands) env))
      )
    )
  )

(define eval-bodies
  (lambda (bodies env)
    (let loop ((bodies bodies))
      (if (null? (cdr bodies))
          (eval-exp (car bodies) env)
          (begin 
            (eval-exp (car bodies) env)
            (loop (cdr bodies))
            )
        )
      )
    )
  )

;  Apply a procedure to its arguments.
;  At this point, we only have primitive procedures.  
;  User-defined procedures will be added later.

(define apply-proc
  (lambda (proc-value args)
    (cases proc-val proc-value
      [prim-proc (op) (apply-prim-proc op args)]
      [closure (params bodies env) (eval-bodies bodies (extend-env params args env))]
			; You will add other cases
      [else (error 'apply-proc
                   "Attempt to apply bad procedure: ~s" 
                    proc-value)])))

(define *prim-proc-names* '(+ - * map apply vector-ref add1 sub1 vector cons set-car! set-cdr! = cadar cadr caar list vector->list vector? number? procedure? symbol? / pair? not zero? >= car cdr null? eq? equal? length list->vector list?))

(define init-env         ; for now, our initial global environment only contains 
  (extend-env            ; procedure names.  Recall that an environment associates
     *prim-proc-names*   ;  a value (not an expression) with an identifier.
     (map prim-proc      
          *prim-proc-names*)
     (empty-env)))

; Usually an interpreter must define each 
; built-in procedure individually.  We are "cheating" a little bit.

(define apply-prim-proc
  (trace-lambda apply-prim-proc(prim-proc args)
    (case prim-proc
      [(+) (apply + args)]
      [(-) (apply - args)]
      [(*) (apply * args)]
      [(add1) (+ (1st args) 1)]
      [(sub1) (- (1st args) 1)]
      [(cons) (cons (1st args) (2nd args))]
      [(=) (= (1st args) (2nd args))]
      [(/) (/ (1st args) (2nd args))]
      [(not) (not (1st args))]
      [(zero?) (zero? (1st args))]
      [(>=) (>= (1st args) (2nd args))]
      [(car) (car (car args))]
      [(cdr) (cdr (car args))]
      [(eq?) (eq? (1st args) (2nd args))]
      [(equal?) (equal? (1st args) (2nd args))]
      [(list) (append args)]
      [(list?) (list? (car args))]
      [(symbol?) (symbol? (car args))]
      [(pair?) (pair? args)]
      [(null?) (null? (1st args))]
      [(length) (length (1st args))]
      [(vector) (apply vector args)]
      [(vector->list) (vector->list (car args))]
      [(vector?) (vector? (car args))]
      [(map) (map (1st args) (2nd args))]
      [(apply) (apply (1st args) (2nd args))]
      [(vector-ref) (vector-ref (1st args) (2nd args))]
      [(list->vector) (list->vector (car args))]
      [(procedure?) (proc-val? (1st args))]
      [(number?) (number? (1st args))]
      [(caar) (caaar args)]
      [(cadr) (cadar args)]
      [(cadar) (cadaar args)]
      [(set-cdr!) (set-cdr! (1st args) (2nd args))]
      [(set-car!) (set-car! (car args) (cadr args))]
      [else (error 'apply-prim-proc 
            "Bad primitive procedure name: ~s" 
            prim-op)])))



(define rep      ; "read-eval-print" loop.
  (lambda ()
    (display "--> ")
    ;; notice that we don't save changes to the environment...
    (let ([answer (top-level-eval (parse-exp (read)))])
      ;; TODO: are there answers that should display differently?
      (eopl:pretty-print answer) (newline)
      (rep))))  ; tail-recursive, so stack doesn't grow.

(define eval-one-exp
  (trace-lambda eval-one-exp(x) 
    (top-level-eval (parse-exp x))
    )
  )











