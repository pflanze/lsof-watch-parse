;; Copyright (c) 2012 by Christian Jaeger.  This is free software: you
;; can redistribute it and/or modify it under the terms of the GNU
;; Lesser General Public License as published by the Free Software
;; Foundation, either version 2 of the License, or (at your option)
;; any later version. See <http://www.gnu.org/licenses/>.


;; just some EXTRACTED COPY

(define (inc n)
  (+ n 1))

(define (dec n)
  (- n 1))


(define (complement fn)
  (lambda v
    (not (apply fn v))))

(define-macro (apply-values f-expr arg-expr)
  `(call-with-values (lambda ()
                       ,arg-expr)
     ,f-expr))

(define (compose f g)
  (lambda x
    (apply-values f (apply g x))))

(define (flip f)
  (lambda (x y)
    (f y x)))

(define (identity x)
  x)


(define-macro (R op . exprs)
  (define (syntax:right-associate op lis error)
    (define (right-associate fn lis error)
      (if (null? lis)
	  (error "got no element, need two")
	  (if (null? (cdr lis))
	      (error "got only one element, need two")
	      (let rec ((lis lis))
		(let* ((lis* (cdr lis))
		       (lis** (cdr lis*)))
		  (if (null? lis**)
		      (fn
		       (car lis)
		       (car lis*))
		      (fn
		       (car lis)
		       (rec lis*))))))))
    (right-associate (lambda (a b)
		       (list op a b))
		     lis
		     error))
  (syntax:right-associate op exprs
                          (lambda (msg)
                            (source-error stx msg))))

(define-macro (compose* . f-exprs)
  `(R compose ,@f-exprs))


;; XX  New functions

(define (cut11 f v1)
  ;; 1 argument before the cut, 1 after
  (lambda (v2)
    (f v1 v2)))

;; derive the number of arguments before the cut syntactically:

(define-macro (cut1 f . args)
  (let ((LASTARG (gensym 'LASTARG)))
    `(lambda (,LASTARG)
       (,f ,@args ,LASTARG))))

(define-macro (cut2 f . args)
  (let ((A1 (gensym 'A1))
	(A2 (gensym 'A2)))
    `(lambda (,A1 ,A2)
       (,f ,@args ,A1 ,A2))))


;; cj-env

(define-macro (first-then arity* access cmp)
  (define (natural? x)
    (and (integer? x)
	 (positive? x)))
  (define (make-list n v)
    (define (dec n)
      (- n 1))
    (let lp ((n n)
	     (res '()))
      (if (positive? n)
	  (lp (dec n)
	      (cons v res))
	  res)))
  
  (let ((arity (eval arity*)))
    (if (natural? arity)
        (let ((ACCESS (gensym 'access))
              (VARS (map gensym (make-list arity 'v))))
          `(let ((,ACCESS ,access))
             (lambda ,VARS
               (,cmp ,@(map (lambda (V)
                              `(,ACCESS ,V))
                            VARS))))))))

(define-macro (on access cmp)
  `(first-then 2 ,access ,cmp))

