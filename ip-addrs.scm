;; Copyright (c) 2012 by Christian Jaeger.  This is free software: you
;; can redistribute it and/or modify it under the terms of the GNU
;; Lesser General Public License as published by the Free Software
;; Foundation, either version 2 of the License, or (at your option)
;; any later version. See <http://www.gnu.org/licenses/>.


;; === get a list of all IP addresses that the local host is listening
;; on

;; This is a HACK to enable distinction of incoming from outgoing TCP
;; connections. Sigh.

;; UNUSED since collection actually happens in perl.
;; XXX: ALSO, this needs to be updated to return ipv4 and ipv6 separately.

;; Parsing something like:
(define ip-addr:example_lines
  '("1: lo: <LOOPBACK,UP,LOWER_UP> mtu 16436 qdisc noqueue state UNKNOWN \\    link/loopback 00:00:00:00:00:00 brd 00:00:00:00:00:00"
    "1: lo    inet 127.0.0.1/8 scope host lo"
    "1: lo    inet6 ::1/128 scope host \\       valid_lft forever preferred_lft forever"))

(define (list-rcollect-items-after l pred)
  (let lp ((l l)
	   (res '()))
    (if (null? l) res
	(if (pred (car l))
	    (lp (cddr l)
		(cons (cadr l) res))
	    (lp (cdr l)
		res)))))

(define (ip-addr-line:maybe-addr str)
  (let ((addrs (list-rcollect-items-after
		(string-split str " ")
		(lambda (a)
		  (or (string=? a "inet")
		      (string=? a "inet6"))))))
    (case (length addrs)
      ((0)
       #f)
      ((1)
       (let ((addr (car addrs)))
	 (car (string-split addr "/"))))
      (else
       (error "parse failure, more than one inet or inet6 address in output of ip addr")))))

(TEST
 > (map ip-addr-line:maybe-addr ip-addr:example_lines)
 (#f "127.0.0.1" "::1")
 )

(define (ip-addrs)
  (stream-filter
   identity
   (stream-map ip-addr-line:maybe-addr
	       (stream-read-lines
		(open-process
		 (list path: "ip"
		       arguments: (list "--oneline" "addr")))
		xclose-process))))
