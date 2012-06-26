;; Copyright (c) 2012 by Christian Jaeger.  This is free software: you
;; can redistribute it and/or modify it under the terms of the GNU
;; Lesser General Public License as published by the Free Software
;; Foundation, either version 2 of the License, or (at your option)
;; any later version. See <http://www.gnu.org/licenses/>.


(include "list.scm")

(define (string-contains? str substr)
  (list-contains? (string->list str) (string->list substr) char=?))

(TEST
 > (string-contains? "" "")
 #t
 > (string-contains? "Hello" "")
 #t
 > (string-contains? "Hello" "H")
 #t
 > (string-contains? "Hello" "Ha")
 #f
 > (string-contains? "Hello" "He")
 #t
 > (string-contains? "Hello" "ll")
 #t
 > (string-contains? "Hello" "llo")
 #t
 > (string-contains? "Hello" "lloh")
 #f
)

(define (string-ends? str substr)
  (let ((lstr (string-length str))
	(lsubstr (string-length substr)))
    (and (>= lstr lsubstr)
	 (string=? (substring str (- lstr lsubstr) lstr) substr))))

(TEST
 > (string-ends? "" "1")
 #f
 > (string-ends? "1" "")
 #t
 > (string-ends? "1" "1")
 #t
 > (string-ends? "abc" "abc")
 #t
 > (string-ends? "abc" "bc")
 #t
 > (string-ends? "abc" "b")
 #f
 > (string-ends? "abc" "c")
 #t
 > (string-ends? "" "")
 #t
 )

(define list-split-output-to-string-split-output
  (compose* reverse (cut1 map (compose list->string reverse))))

(define (string-split str substr)
  (list-split-output-to-string-split-output
   (list-split (string->list str)
	       (string->list substr)
	       char=?)))
