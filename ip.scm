;; Copyright (c) 2012 by Christian Jaeger.  This is free software: you
;; can redistribute it and/or modify it under the terms of the GNU
;; Lesser General Public License as published by the Free Software
;; Foundation, either version 2 of the License, or (at your option)
;; any later version. See <http://www.gnu.org/licenses/>.


;; === parse IP:port[->IP:port] strings as given by lsof

;; "1.2.3.4:www->5.6.7.8:48882"

(define ip:_arrow (string->list "->"))

(define (ip->ip/list:split str)
  ;; into reverse list of reverse lists
  (list-split (string->list str)
	      ip:_arrow
	      char=?))

(TEST
 > (ip->ip/list:split "1.2.3.4:80->5.6.7.8:1999")
 ((#\9 #\9 #\9 #\1 #\: #\8 #\. #\7 #\. #\6 #\. #\5)
  (#\0 #\8 #\: #\4 #\. #\3 #\. #\2 #\. #\1))
 )

(define (ip->ip:split str cont)
  (apply cont (reverse
	       (map (compose list->string reverse) (ip->ip/list:split str)))))
(TEST
 > (ip->ip:split "1.2.3.4:www->5.6.7.8:48882" vector)
 #("1.2.3.4:www" "5.6.7.8:48882")
 )

(define ip:_colon (string->list ":"))

(define (ip:rlis-split* cont)
  (lambda (rlis)
    ;; -> (cont (list port) rIP)
    (list-split rlis
		ip:_colon
		char=?
		(list 1
		      cont
		      (lambda ()
			(error "missing IP part"))))))

(TEST
 > (map (ip:rlis-split* list) (ip->ip/list:split "1.2.3.4:80->5.6.7.8:1999"))
 ((((#\1 #\9 #\9 #\9)) (#\8 #\. #\7 #\. #\6 #\. #\5))
  (((#\8 #\0)) (#\4 #\. #\3 #\. #\2 #\. #\1)))
 )

(define ip:rlis-split
  (ip:rlis-split* (lambda (lport rIP)
		    (list (list->string (reverse rIP))
			  (list->string (car lport))))))

(TEST
 > (map ip:rlis-split (ip->ip/list:split "1.2.3.4:80->5.6.7.8:1999"))
 (("5.6.7.8" "1999") ("1.2.3.4" "80"))
 )

(define ip:split
  (compose* ip:rlis-split
	    ;; well that's quite the hack, but..:
	    (lambda (l)
	      (or (and (pair? l)
		       (null? (cdr l))
		       (car l))
		  (error "wrong format")))
	    ip->ip/list:split))

(TEST
 > (ip:split "1.2.3.4:80")
 ("1.2.3.4" "80")
 > (ip:split "[::1]:80")
 ("[::1]" "80")
 )


(define (add-ipv6-brackets str)
  (string-append "[" str "]"))
