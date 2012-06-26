
;; still leaking.

(define (stream-fold-right kons tail s*)
  (let ((s (force s*)))
    (cond ((null? s)
	   tail)
	  ((pair? s)
	   (let ((rest (let ((r (cdr s)))
			 (delay (stream-fold-right kons tail r)))))
	     ;;^ which is lazy
	     (kons (car s)
		   rest)))
	  (else
	   (error "stream-fold-right: improper stream:" s)))))
