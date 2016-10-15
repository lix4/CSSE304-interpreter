(define apply-continuation
	(lambda (k v)
		(k v)))

(define fact-cps
	(lambda fact-cps (n k)
		(if (zero? n)
			(apply-continuation k 1)
			(fact-cps (- n 1) 
				      (lambda (v) 
				      	      (apply-continuation k (* n v)))))))

(define list-copy-cps
	(lambda (L k)
		(if (null? L)
			(apply-continuation k '())
			(list-copy-cps (cdr L)
				           (lambda (copied-cdr)
				           	       (apply-continuation k 
				           	       	                   (cons (car L) copied-cdr)))))))

(define memq-cps
	(lambda (sym ls k)
		(cons [(null? ls) (apply-continuation k #f)]
			  [(eq? (car ls )sym) (apply-continuation k #t)]
			  [else (memq-cps sym (cdr ls) k)])))

(define intersection-cps
	(lambda (los1 los2 k)
		(if (null? los1)
			(apply-continuation k '())
			(intersection-cps (cdr los1) los2
				              (lambda (intersection-with-cdr)
				              	      (memq-cps (car los1) los2
				              	      	        (lambda (car-in-los2)
				              	      	        	    (apply-continuation k
				              	      	        	    	                (if car-in-los2
				              	      	        	    	                    (cons (car los1) intersection-with-cdr)
				              	      	        	    	                    intersection-with-cdr)))))))))

(intersection-cps '(a v x) '(a b c) list)

(define 1st car)
(define 2nd cadr)
(define 3rd caddr)

(define free-vars-cps
	(lambda (exp k)
		(cond [(symbol? exp) (apply-continuation k (list exp))]
			  [(eq? (1st exp) 'lambda) 
			   (free-vars-cps (3rd exp))]