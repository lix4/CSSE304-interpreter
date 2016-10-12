;;Xiwen Li, Wenkang Dang
;;Assignment 13

; top-level-eval evaluates a form in the global environment
(define top-level-eval
  (lambda (form)
    ; later we may add things that are not expressions.
    (eval-exp form (empty-env))))

; eval-exp is the main component of the interpreter

(define eval-exp
  (lambda (exp env)
    (cases expression exp
      [lit-exp (datum) datum]
      [quote-exp (datum) (cadr datum)]
      [var-exp (id)
				(apply-env env
           id; look up its value.
      	   (lambda (x) x) ; procedure to call if id is in the environment 
           (lambda ()
                   (apply-env global-env
                   id
                   (lambda (x) x)
                   (lambda ()
                           (eopl:error 'apply-env ; procedure to call if id not in env
            		           "variable not found in environment: ~s"
            			         id)))))]
      [let-exp (vars vals bodies)
               (let ([new-env (extend-env vars (eval-rands vals env) env)])
                    (eval-bodies bodies new-env))] 
      [if-exp (test-exp then-exp else-exp)
               (if (eval-exp test-exp env)
                   (eval-exp then-exp env)
                   (eval-exp else-exp env))]
      [lambda-exp (vars bodies)
                  (closure vars bodies env)]
      [app-exp (rator rands)
        (let ([proc-value (eval-exp rator env)]
              [args (eval-rands rands env)])
          (apply-proc proc-value args))]
      [else (eopl:error 'eval-exp "Bad abstract syntax: ~a" exp)])))

; evaluate the list of operands, putting results into a list

(define eval-rands
  (lambda (rands env)
    (map (lambda (e) (eval-exp e env)) rands)))

(define eval-bodies
  (lambda (bodies env)
    (if (null? (cdr bodies))
        (eval-exp (car bodies) env)
        (begin (eval-exp (car bodies) env)
               (eval-bodies (cdr bodies) env)))))

;  Apply a procedure to its arguments.
;  At this point, we only have primitive procedures.  
;  User-defined procedures will be added later.

(define apply-proc
  (lambda (proc-value args)
    (cases proc-val proc-value
      [prim-proc (op) (apply-prim-proc op args)]
			; You will add other cases
      [closure (vars bodies env) 
               (let ([new-env (extend-env vars args env)])
                    (eval-bodies bodies new-env))]
      [else (error 'apply-proc
                   "Attempt to apply bad procedure: ~s" 
                    proc-value)])))

(define *prim-proc-names* '(+ - * / >= < add1 sub1 zero? list cons = not cons car cdr list null? assq eq? equal? 
                            atom? length list->vector list? pair? vector->list vector make-vector procedure?
                            vector-ref vector? number? symbol? set-car! set-cdr! vector-set! display newline
                            quote caar cadr cadar))

(define init-env         ; for now, our initial global environment only contains 
  (extend-env            ; procedure names.  Recall that an environment associates
     *prim-proc-names*   ;  a value (not an expression) with an identifier.
     (map prim-proc      
          *prim-proc-names*)
     (empty-env)))

(define global-env init-env)

; Usually an interpreter must define each 
; built-in procedure individually.  We are "cheating" a little bit.

(define apply-prim-proc
  (lambda (prim-proc args)
    (case prim-proc 
      [(+) (apply + args)]
      [(-) (apply - args)]
      [(*) (apply * args)]
      [(/) (/ (1st args) (2nd args))]
      [(<) (< (1st args) (2nd args))]
      [(add1) (+ (1st args) 1)]
      [(>=) (>= (1st args) (2nd args))]
      [(sub1) (- (1st args) 1)]
      [(zero?) (zero? (1st args))]
      [(list) args]
      [(not) (not (1st args))]
      [(car) (caar args)]
      [(cdr) (cdar args)]
      [(null?) (null? (1st args))]
      [(assq) (assq (1st args) (2nd args))]
      [(atom?) (atom? (1st args))]
      [(length) (length (1st args))]
      [(list->vector) (list->vector (1st args))]
      [(list?) (list? (1st args))]
      [(pair?) (pair? (1st args))]
      [(procedure?) (proc-val? (1st args))]
      [(vector->list) (vector->list (1st args))]
      [(vector) (apply vector args)]
      [(make-vector) (make-vector (1st args) (2nd args))]
      [(vector-ref) (vector-ref (1st args) (2nd args))]
      [(vector?) (vector? (1st args))]
      [(number?) (number? (1st args))]
      [(symbol?) (symbol? (1st args))]
      [(caar) (caar (1st args))]
      [(cadr) (cadr (1st args))]
      [(cadar) (cadar (1st args))]
      [(set-car!) (set-car! (1st args) (2nd args))]
      [(set-cdr!) (set-cdr! (1st args) (2nd args))]
      [(vector-set!) (vector-set! (1st args) (2nd args) (3rd args))]
      [(display) (display (1st args))]
      [(newline) (newline (1st args))]
      [(eq?) (eq? (1st args) (2nd args))]
      [(equal?) (equal? (1st args) (2nd args))]
      [(quote) (quote args)]
      [(cons) (cons (1st args) (2nd args))]
      [(=) (= (1st args) (2nd args))]
      [else (error 'apply-prim-proc 
            "Bad primitive procedure name: ~s" 
            prim-proc)])))

(define rep      ; "read-eval-print" loop.
  (lambda ()
    (display "--> ")
    ;; notice that we don't save changes to the environment...
    (let ([answer (top-level-eval (parse-exp (read)))])
      ;; TODO: are there answers that should display differently?
      (eopl:pretty-print answer) (newline)
      (rep))))  ; tail-recursive, so stack doesn't grow.

(define eval-one-exp
  (lambda (x) (top-level-eval (parse-exp x))))










