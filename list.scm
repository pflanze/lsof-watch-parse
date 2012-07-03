;; Copyright (c) 2012 by Christian Jaeger <ch@christianjaeger.ch>.
;; This is free software: you can redistribute it and/or modify it
;; under the terms of the GNU Lesser General Public License as
;; published by the Free Software Foundation, either version 2 of the
;; License, or (at your option) any later version. See
;; <http://www.gnu.org/licenses/>.


(define (filter pred l)
  (if (null? l)
      l
      (let ((v (car l))
	    (rest (lambda ()
		    (filter pred (cdr l)))))
	(if (pred v)
	    (cons v (rest))
	    (rest)))))

(include "srfi-1.scm")

(define (list-starts-with l subl equal? found not-found)
  ;; found receives the rest of l after the common region
  (let starts-with ((l l)
		    (subl subl))
    (if (null? subl)
	(found l)
	(if (null? l)
	    (not-found)
	    (if (equal? (car l) (car subl))
		(starts-with (cdr l) (cdr subl))
		(not-found))))))

(define (list-starts-with? l subl equal?)
  (list-starts-with l subl equal?
		    (lambda (rest)
		      #t)
		    (lambda ()
		      #f)))

(TEST
 > (list-starts-with? '() '() equal?)
 #t
 > (list-starts-with? '() '(a) equal?)
 #f
 > (list-starts-with? '(a) '() equal?)
 #t
 > (list-starts-with? '(a) '(a) equal?)
 #t
 > (list-starts-with? '(a b) '(a) equal?)
 #t
 > (list-starts-with? '(a b) '(a b) equal?)
 #t
 > (list-starts-with? '(a b) '(a b c) equal?)
 #f
 > (list-starts-with? '(c a b) '(a b) equal?)
 #f
 > (list-starts-with? '(a b) '(c a b) equal?)
 #f
 ;; and list-starts-with :
 > (list-starts-with '(a b) '(a b) equal? vector vector)
 #(())
 > (list-starts-with '(a b) '(a) equal? vector vector)
 #((b))
 > (list-starts-with '(a b) '(b a) equal? vector vector)
 #()
 )

(define (list-contains? l subl equal?)
  (or (list-starts-with? l subl equal?)
      (if (null? l)
	  #f
	  (list-contains? (cdr l) subl equal?))))

(TEST
 > (list-contains? '() '() equal?)
 #t
 > (list-contains? '() '(b c) equal?)
 #f
 > (list-contains? '(a b c) '() equal?)
 #t
 > (list-starts-with? '(a b) '(c a b) equal?)
 #f
 > (list-contains? '(a b c) '(b) equal?)
 #t
 > (list-contains? '(a b c) '(b c) equal?)
 #t
 > (list-contains? '(a b c) '(b c d) equal?)
 #f
)

(define (list-split l subl equal?
		    #!optional
		    ;; limit functionality:
		    maybe-limit+remaindercont+noremaindercont)
  ;; -> reverse list of reversed sublists
  (if (null? subl)
      (error "list-split can't split on an empty list")
      (let next-found ((rlorl '())
		       (l l)
		       ;; as in Perl's split; negative meaning +inf;
		       ;; XXX but that I'm shadowing unspecified here is
		       ;; unlike Perl. Just for simplicity for now..
		       (limit (if maybe-limit+remaindercont+noremaindercont
				  (car maybe-limit+remaindercont+noremaindercont)
				  -1)))
	(if (zero? limit)
	    ;; remaindercont receives result and remainder
	    ((cadr maybe-limit+remaindercont+noremaindercont) rlorl l)
	    (let next-search ((l l)
			      (revl '()))
	      (if (null? l)
		  ((lambda (res)
		     (if maybe-limit+remaindercont+noremaindercont
			 ((caddr maybe-limit+remaindercont+noremaindercont) res)
			 res))
		   (cons revl rlorl))
		  (list-starts-with
		   l subl equal?
		   (lambda (lrest)
		     (next-found (cons revl rlorl)
				 lrest
				 (dec limit)))
		   (lambda ()
		     (next-search (cdr l)
				  (cons (car l) revl))))))))))

(TEST
 > (list-split '(a b c d) '(b c) equal?)
 ((d) (a))
 > (list-split '(a b c d) '(b) equal?)
 ((d c) (a))
 > (list-split '(a b c d) '(d) equal?)
 (() (c b a))
 ;; nonsplit:
 > (list-split '(a b c d) '(x) equal?)
 ((d c b a))
 ;; edge cases:
 > (list-split '() '(d) equal?)
 (())
 ;;> (list-split '(a) '() equal?)
 ;;  C-c C-c*** INTERRUPTED IN ##reverse oooho --hu why reverse.
 ;; anyway, now
 ;; *** ERROR IN list-split, "list.scm"@117.7 -- list-split can't split on an empty list
 ;; Can't (unlike perl split //) because it's not even a 1-char advance for the match for '().
 ;; (Yeah how is the logic of the Perl thing?)

 ;; limit mode:
 > (list-split '(a b c d) '(d) equal? (list 1 vector identity))
 #(((c b a)) ())
 > (list-split '(a b c d) '(d) equal? (list 2 vector identity))
 (() (c b a))
 > (list-split '(a X b X c d X) '(X) equal? (list 3 vector identity))
 #(((d c) (b) (a)) ())
 > (list-split '(a X b X c d X) '(X) equal? (list 2 vector identity))
 #(((b) (a)) (c d X))
 > (list-split '(a X b X c d X) '(X) equal? (list 0 vector identity))
 #(() (a X b X c d X))
 > (list-split '(a X b X c d X) '(X) equal? (list -1 vector identity))
 (() (d c) (b) (a))
 > (list-split '(a X b X c d X) '(X) equal?)
 (() (d c) (b) (a))
 )
