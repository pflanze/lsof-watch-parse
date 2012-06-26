;; Copyright (c) 2012 by Christian Jaeger.  This is free software: you
;; can redistribute it and/or modify it under the terms of the GNU
;; Lesser General Public License as published by the Free Software
;; Foundation, either version 2 of the License, or (at your option)
;; any later version. See <http://www.gnu.org/licenses/>.


(define (object? v)
  (and (vector? v)
       (>= (vector-length v) 1)
       (symbol? (vector-ref v 0))))

(define (object-type v)
  (if (object? v)
      (vector-ref v 0)
      (error "not an object:" v)))

(define (maybe-object-type= v type)
  (and (vector? v)
       (>= (vector-length v) 1)
       (eq? (vector-ref v 0) type)))


(define (keyword=? a b)
  (if (and (keyword? a)
	   (keyword? b))
      (eq? a b)
      (error "at least one is not a keyword:" a b)))

(define (make-accessor type fieldnamekeyword)
  (lambda (v #!optional not-found)
    (if (maybe-object-type= v type)
	(let ((len (vector-length v)))
	  (let lp ((i 1))
	    (if (< i len)
		(if (keyword=? (vector-ref v i) fieldnamekeyword)
		    (vector-ref v (inc i))
		    (lp (+ i 2)))
		(if not-found (not-found)
		    (error "field not found in object:" fieldnamekeyword v)))))
	(error "not an object of type: " type v))))

(define-macro (define-p-ref typesym namesym fieldnamesym)
  (include "symbol-append.scm")
  `(define ,(symbol-append typesym '- namesym)
     (make-accessor ',typesym ,(string->keyword (symbol->string fieldnamesym)))))

(define-macro (define-p-struct type . defrefs)
  (include "symbol-append.scm")
  (include "srfi-1.scm") ;; for fold-right
  (let ((shortfieldsyms (map (lambda (defref)
			       (if (pair? defref)
				   (cadr defref)
				   defref))
			     defrefs)))
    `(begin
       ;; give the short one, ok?
       (define (,type #!key ,@shortfieldsyms)
	 ;; don't risk shadowing |vector|; XXX: uh, no way to secure |quote|!!
	 (##vector ',type
		   ,@(fold-right
		      (lambda (v res)
			(cons (symbol->keyword v)
			      (cons v res)))
		      '()
		      shortfieldsyms)))
       (define (,(symbol-append type '?) v)
	 (maybe-object-type= v ',type))
       ,@(map (lambda (defref)
		(let ((fmt (lambda (acc ref)
			     `(define-p-ref ,type ,acc ,ref))))
		  (if (pair? defref)
		      (fmt (car defref) (cadr defref))
		      (fmt defref defref))))
	      defrefs))))

