;; Copyright (c) 2012 by Christian Jaeger.  This is free software: you
;; can redistribute it and/or modify it under the terms of the GNU
;; Lesser General Public License as published by the Free Software
;; Foundation, either version 2 of the License, or (at your option)
;; any later version. See <http://www.gnu.org/licenses/>.


(define (close-process-returning-status p)
  (close-output-port p) ;; HMM how do we know which are open ??!
  (close-input-port p)
  (process-status p))

(define close-process-ignoring-status close-process-returning-status)

;; ('X' like on poisonous bottles)
(define (xclose-process p)
  (let ((st (close-process-returning-status p)))
    (case st
      ((0)
       'ok)
      (else
       (error "process exited with non-zero status" st)))))

(define (make-stream-read-all read)
  (lambda (p close)
    (let rec ()
      (delay
	(let ((v (read p)))
	  (if (eof-object? v)
	      (begin
		(close p)
		'())
	      (cons v (rec))))))))

(define stream-read-all (make-stream-read-all read))

(define stream-read-lines (make-stream-read-all read-line))

;; just some EXTRACTED COPY

(define (stream-fold-left fn z s)
  (let lp ((s s)
           (z z))
    ;;(delay nope
    (let ((p (force s)))
      (cond ((null? p)
             z)
            ((pair? p)
             (lp (cdr p)
                 (fn (car p)
                     z)))
            (else
             (error "stream-fold-left: improper stream, ending in:" p))))))

(include "stream-fold-right.scm")

(define (stream-map/tail func s tail)
  ;; maybe with an adjusted error message?..
  (stream-fold-right (lambda (val tail)
                       (cons (func val)
                             tail))
                     tail
                     s))

(define (_stream-map func s)
  (stream-map/tail func s '()))

;; manually inlined:

(define (stream-map func s)
  (define (stream-fold-right kons tail s)
    (delay
      (let ((s (force s)))
	(cond ((null? s)
	       tail)
	      ((pair? s)
	       (kons (car s)
		     (stream-fold-right kons tail (cdr s))))
	      (else
	       (error "stream-fold-right: improper stream:" s))))))
  (stream-fold-right (lambda (val tail)
                       (cons (func val)
                             tail))
                     '()
                     s))


(define (nonleaking:stream-map func s)
  (delay
    (let ((s (force s)))
      (if (pair? s)
	  (cons (func (car s))
		(stream-map func (cdr s)))))))


(define (stream-filter pred s #!optional (tail '()))
  (let lp ((s s))
    (delay
      (let ((p (force s)))
        (cond ((null? p)
               tail)
              ((pair? p)
               (let ((a (car p))
                     (r (cdr p)))
                 (if (pred a)
                     (cons a
                           (lp r))
                     (lp r))))
              (else
               (error "stream-filter: improper stream, ending in:" p)))))))

(define (stream->list s)
  (let rec ((s s))
    (let ((p (force s)))
      (cond ((null? p)
             '())
            ((pair? p)
             (cons (car p)
                   (rec (cdr p))))
            (else
             ;; (don't keep a reference to the stream head to avoid memory retention!)
             (error "stream->list: improper stream, ending in:" p))))))

(define (stream-drop s k)
  (if (>= k 0)
      (let iter ((s s)
                 (k k))
        (if (zero? k) s
            (let ((p (force s)))
              (if (pair? p)
                  (iter (cdr p)
                        (dec k))
                  (error "stream-drop: stream too short")))))
      (error "stream-drop: negative k:" k)))


(define (stream-take s k #!optional (tail '()))
  (if (>= k 0)
      (let rec ((s s)
                (k k))
        ;;(delay/trace "stream-take"
        (delay
          (if (zero? k)
              tail
              (let ((p (force s)))
                (cond ((pair? p)
                       (cons (car p)
                             (rec (cdr p)
                                  (dec k))))
                      ((null? p)
                       p)
                      (error "stream-take: improper stream:" p))))))
      (error "stream-take: negative k:" k)))

(define (stream-length s)
  (let lp ((s s)
           (n 0))
    (let ((p (force s)))
      (cond ((null? p)
             n)
            ((pair? p)
             (lp (cdr p)
                 (inc n)))
            (else
             ;; (don't keep a reference to the stream head to avoid memory retention!)
             (error "stream->list: improper stream, ending in:" p))))))


(define (stream-sublist s si ei)
  (stream->list (stream-take (stream-drop s si) (- ei si))))


(define (stream-for-each proc s)
  (let lp ((s s))
    (let ((s (force s)))
      (cond ((null? s)
             (void))
            ((pair? s)
             (proc (force (car s)))
             (lp (cdr s)))
            (else
             (error "improper stream:" s))))))



(define (F s)
  (let F ((s s))
    (let ((s (force s)))
      (if (pair? s)
          (cons (F (car s))
                (F (cdr s)))
          s))))


;; COPY adapted

(define-macro (DELAY expr)
  expr)
(define-macro (FV vars expr)
  expr)

(define list-group
  ;; each group is built eagerly and contains the items in reverse order
  (lambda (equal? s #!optional (tail '()))
    (DELAY
     (FV (s)
	 (if (null? s)
	     tail
	     (let ((a (car s))
		   (r (cdr s)))
	       (let rec ((prev a)
			 (s r))
		 (DELAY
		  (let lp ((s s)
			   (group (cons prev '())))
		    (FV (s)
			(if (null? s)
			    (cons group tail) ;;end?ç
			    (let ((a (car s))
				  (r (cdr s)))
			      (if (equal? prev a)
				  (lp r
				      (cons a group))
				  (cons group
					(rec a r)))))))))))))))

;;/COPY

