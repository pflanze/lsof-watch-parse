;; Partial copy of the:

;;; SRFI-1 list-processing library                      -*- Scheme -*-
;;; Reference implementation

;;; Copyright (c) 1998, 1999 by Olin Shivers. You may do as you please with
;;; this code as long as you do not remove this copyright notice or
;;; hold me liable for its use. Please send bug reports to shivers@ai.mit.edu.
;;;     -Olin

(define (null-list? l)
  (cond ((pair? l) #f)
        ((null? l) #t)
        (else (error "null-list?: argument out of domain" l))))

(define (car+cdr pair) (values (car pair) (cdr pair)))

(define (%cars+cdrs+ lists cars-final)
  (call-with-current-continuation
    (lambda (abort)
      (let recur ((lists lists))
        (if (pair? lists)
            (receive (list other-lists) (car+cdr lists)
              (if (null-list? list) (abort '() '()) ; LIST is empty -- bail out
                  (receive (a d) (car+cdr list)
                    (receive (cars cdrs) (recur other-lists)
                      (values (cons a cars) (cons d cdrs))))))
            (values (list cars-final) '()))))))

(define (fold kons knil lis1 . lists)
  (if (pair? lists)
      (let lp ((lists (cons lis1 lists)) (ans knil))    ; N-ary case
        (receive (cars+ans cdrs) (%cars+cdrs+ lists ans)
          (if (null? cars+ans) ans ; Done.
              (lp cdrs (apply kons cars+ans)))))
            
      (let lp ((lis lis1) (ans knil))                   ; Fast path
        (if (null-list? lis) ans
            (lp (cdr lis) (kons (car lis) ans))))))

;; /COPY
