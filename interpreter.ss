;;Xiwen Li, Wenkang Dang
;;Assignment 16

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
      [cond-exp (cases bodies)
                (let ([ref-number (first-true cases (extend-env (list 'else) (list 'else) env))])
                     (if (eq? ref-number (void))
                         (void)
                         (eval-exp (list-ref bodies ref-number) env)))]
      [or-exp (bodies) (eval-or-exp bodies env)]
      [letrec-exp (proc-names idss bodiess letrec-bodies)
                  (eval-bodies letrec-bodies (extend-env-recursively
                                             proc-names idss bodiess env))]
      [case-exp (val-expr case-clause bodies)
                (let* ([val (eval-exp val-expr env)]
                       [bodies (eval-rands bodies env)]
                       [ref-number (first-contain case-clause val env)])
                      (list-ref bodies ref-number))]
      [if-no-else-exp (test-exp then-exp)
                      (if (eval-exp test-exp env)
                          (eval-exp then-exp env))]
      [while-exp (test-exp bodies)
                 (eval-while-loop test-exp bodies env)]
      [begin-exp (bodies)
                 (eval-bodies bodies env)]
      [lambda-exp (vars bodies)
                  (closure vars bodies env)]
      [lambda-improp-exp (vars bodies)
                         (closure vars bodies env)]
      [lambda-sym-exp (vars bodies)
                      (closure vars bodies env)]
      [app-exp (rator rands)
        (let ([proc-value (eval-exp rator env)]
              [args (eval-rands rands env)])
          (apply-proc proc-value args))]
      [else (eopl:error 'eval-exp "Bad abstract syntax: ~a" exp)])))

(define extend-env-recursively
    (lambda (proc-names idss bodiess old-env)
            (recursively-extended-env-record proc-names idss bodiess old-env)))

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
              (cond [(list? vars)
                     (if (null? vars)
                         (eval-bodies bodies env)
                         (let ([new-env (extend-env vars args env)])
                              (eval-bodies bodies new-env)))]
                    [(symbol? vars)
                     (let ([new-env (extend-env (list vars) (list args) env)])
                          (eval-bodies bodies new-env))]
                    [else (let* ([vars (improperlist-vars-processor vars)]
                                 [args (improperlist-args-processor vars args)]
                                 [new-env (extend-env vars args env)])
                               (eval-bodies bodies new-env))])]
      [else (error 'apply-proc
                   "Attempt to apply bad procedure: ~s" 
                    proc-value)])))

(define eval-while-loop
  (lambda (test-expr bodies env)
    (if (eval-exp test-expr env)
        (begin (eval-bodies bodies env)
               (eval-while-loop test-expr bodies env)))))

(define eval-or-exp
  (lambda (bodies env)
    (cond [(null? bodies) #f]
          [(or (eval-exp (car bodies) env) 
               (number? (eval-exp (car bodies) env))) 
           (eval-exp (car bodies) env)]
          [else (eval-or-exp (cdr bodies) env)])))

(define first-contain
  (lambda (cases val env)
    (first-contain-rec cases val 0 env)))

(define first-contain-rec
  (lambda (cases val index env)
    (cond [(null? (cdr cases))
           (cond [(eqv? (car cases) 'else) index]
                 [(member? val (car cases)) index]
                 [else (void)])]
          [(member? val (car cases)) index]
          [else (first-contain-rec (cdr cases) val (+ index 1) env)])))

(define member?
  (lambda (target ls)
    (cond [(null? ls) #f]
          [(equal? (car ls) target) #t]
          [else (member? target (cdr ls))])))


(define first-true
  (lambda (cases env)
    (first-true-rec cases 0 env)))

(define first-true-rec
  (lambda (cases index env)
    (cond [(null? (cdr cases))
           (cond [(eqv? (eval-exp (car cases) env) 'else) index]
                 [(eval-exp (car cases) env) index]
                 [else (void)])]
          [(eval-exp (car cases) env) index]
          [else (first-true-rec (cdr cases) (+ index 1) env)])))


;This procedure converts a improper list into a proper list
(define improperlist-vars-processor
  (lambda (vars)
    (if (not (pair? (cdr vars)))
        (list (car vars) (cdr vars))
        (append (list (car vars)) (improperlist-vars-processor (cdr vars))))))


(define improperlist-args-processor
  (lambda (vars args)
    (if (null? (cdr vars))
        (list args)
        (cons (car args) (improperlist-args-processor (cdr vars) (cdr args))))))


(define *prim-proc-names* '(+ - * / >= < > append eqv? list-tail quotient add1 sub1 zero? list cons = not cons car cdr list null? assq eq? equal? 
                            atom? length list->vector list? pair? vector->list vector make-vector procedure?
                            vector-ref vector? number? symbol? set-car! set-cdr! vector-set! display newline
                            quote caar cadr cadar map apply))

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
      [(>) (> (1st args) (2nd args))]
      [(add1) (+ (1st args) 1)]
      [(>=) (>= (1st args) (2nd args))]
      [(sub1) (- (1st args) 1)]
      [(zero?) (zero? (1st args))]
      [(eqv?) (eqv? (1st args) (2nd args))]
      [(list) args]
      [(not) (not (1st args))]
      [(car) (caar args)]
      [(cdr) (cdar args)]
      [(null?) (null? (1st args))]
      [(assq) (assq (1st args) (2nd args))]
      [(atom?) (atom? (1st args))]
      [(list-tail) (list-tail (car args) (cadr args))]
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
      [(append) (append (1st args) (2nd args))]
      [(quotient) (apply quotient args)]
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
      [(map) (map (lambda (x) (apply-proc (1st args) (list x))) (2nd args))]
      [(apply) (apply-proc (1st args) (2nd args))]
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

(define syntax-expand
  (lambda (exp)
    (cases expression exp
      [lit-exp (id) (lit-exp id)]
      [var-exp (id) (var-exp id)]
      [app-exp (rator rands)
               (app-exp (syntax-expand rator) (map syntax-expand rands))]
      [if-exp (test-exp then-exp else-exp)
              (if-exp (syntax-expand test-exp) 
                      (syntax-expand then-exp) 
                      (syntax-expand else-exp))]
      [if-no-else-exp (test-exp then-exp)
                      (if-no-else-exp (syntax-expand test-exp) 
                                      (syntax-expand then-exp))]
      [or-exp (bodies) (or-exp (map syntax-expand bodies))]
      [and-exp (bodies) (and-exp (map syntax-expand bodies))]
      [letrec-exp (proc-names idss bodiess letrec-bodies)
                   (letrec-exp proc-names 
                              idss 
                              (map (lambda (x) (map syntax-expand x)) bodiess)
                              (map syntax-expand letrec-bodies))]
      [cond-exp (cases bodies) (cond-exp (map syntax-expand cases) 
                                         (map syntax-expand bodies))]
      [let-name-exp (name vars vals bodies)
                    (app-exp (letrec-exp (list name)
                                         (list vars) 
                                         (map (lambda (x) (map syntax-expand x)) (map list bodies))
                                         (list (var-exp name)))
                              vals)]
      [begin-exp (bodies) (begin-exp (map syntax-expand bodies))]
      [case-exp (val-expr case-clause bodies)
                (case-exp (syntax-expand val-expr) 
                          case-clause 
                          (map syntax-expand bodies))]
      ; [let*-exp (vars vals bodies) ()]
      [quote-exp (datum) (quote-exp datum)]
      [lambda-exp (vars bodies)
                  (lambda-exp vars (map syntax-expand bodies))]
      [lambda-sym-exp (vars bodies)
                      (lambda-sym-exp vars (map syntax-expand bodies))]
      [lambda-improp-exp (vals bodies)
                         (lambda-improp-exp vals (map syntax-expand bodies))]
      [let-exp (vars vals bodies) (app-exp (lambda-exp vars (map syntax-expand bodies)) 
                                           (map syntax-expand vals))]
      [while-exp (test-exp bodies) (while-exp (syntax-expand test-exp) 
                                              (map syntax-expand bodies))]
      )))  



(define eval-one-exp
  (lambda (exp) (eval-exp (syntax-expand (parse-exp exp)) (empty-env))))    

; (define eval-one-exp
;   (lambda (x) (top-level-eval (parse-exp x))))










