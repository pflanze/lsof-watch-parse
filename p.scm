;; Copyright (c) 2012 by Christian Jaeger.  This is free software: you
;; can redistribute it and/or modify it under the terms of the GNU
;; Lesser General Public License as published by the Free Software
;; Foundation, either version 2 of the License, or (at your option)
;; any later version. See <http://www.gnu.org/licenses/>.


(define-macro (TEST . r)
  '(begin))
(include "div.scm")
(include "stream.scm")
(include "functional.scm")
(include "string.scm")
(include "p-parse.scm")
(include "p-stream.scm")
(include "p-struct.scm")
(include "slib-sort.scm")
(include "ip.scm")

(define testpoint
  '#(point
     filehandles:
     (
      #(filehandle D: "0xa" G: "0x2;0x0"  a: "u" f: 255 i: 41 k: 1 l: " " n: "/dev/pts/38" o: "0t0" t: "CHR")
      #(filehandle G: "0x802;0x0" P: "TCP" T: "QS=0"  a: "u" d: 3779 f: 3 l: " " n: "*:ssh" o: "0t0" t: "IPv4")
      #(filehandle G: "0x802;0x0" P: "TCP" T: "QS=0"  a: "u" d: 3781 f: 4 l: " " n: "*:ssh" o: "0t0" t: "IPv6")
      #(filehandle G: "0x802;0x0" P: "TCP" T: "QS=0"  a: "u" d: 3760310 f: 18 l: " " n: "192.168.1.135:ssh->8.9.10.11:48882" o: "0t0" t: "IPv4")
      )
     ip-addrs: #(ip-addrs ipv4: ("127.0.0.1" "192.168.1.135")
			  ipv6: ("::1" "abcd::ef01:2345:6789:abfe"))
     time: 1340662680))

;; === accessors and methods

(define-p-struct process
  (pid p)
  (command-name c) ;; not commandline?
  (user u)
  (group g)
  (unknown-process-field R) ;; "running"?
  (login-name L)
  )

;; (XX hey, careful about this?:   m    marker between repeated output)

(define-p-struct filehandle
  (filename n) ;; "file name, comment, Internet address"
  (fileno f) ;; "file descriptor"
  (type t) ;; REG, CHR, FIFO, IPv4, unix, ...
  (protocol P) ;; TCP -- only present for IP
  (inode i)
  ;; @ prefix to indicate unsafety because of overloading
  (@device d) ;; for REG, CHR, FIFO  "device character code"
  ;;(@port d) ;; for IPv4 --hm not sure./not always.
  (size s) ;; for REG
  (access a) ;; r, w, u
  (device-number D) ;; major/minor device number (0x<hexadecimal>)
  (link-count k)
  (TCP/TPI T) ;; TCP/TPI information
  (offset o) ;; "file's offset (decimal)" ; "0t0" meaning invalid for TCP?
  (flags G) ;; "file flaGs (0x<hexadecimal>; names if +fg follows)"
  )

(define-p-struct ip-addrs
  ipv4
  ipv6)


(define (filehandle-is-ip? v)
  (let ((t (filehandle-type v)))
    (or (=? t "IPv4")
	(=? t "IPv6"))))

(define (filehandle-is-ipv6? v)
  (let ((t (filehandle-type v)))
    (=? t "IPv6")))

(define (filehandle-is-ip-connection? v)
  (and (filehandle-is-ip? v)
       (string-contains? (filehandle-filename v) "->")))

(define (filehandle-is-ip-listener? v)
  (and (filehandle-is-ip? v)
       ;; HACK?  QS=<send queue size> which is only available in
       ;; listening sockets? But will this only work on Linux? -- And
       ;; it's not even sufficient. grr
       (and  (string-contains? (filehandle-TCP/TPI v) "QS=")
	     (not (string-contains? (filehandle-filename v) "->")))))

(TEST
 > (length (filter filehandle-is-ip-listener? (point-filehandles testpoint)))
 2
 )

(define (ip->ip->maybe-sourceIP v listeners)
  ;; only returns a result if the ip connection is (seems to be) an
  ;; incoming connection
  (let* ((rlorl (ip->ip/list:split v))
	 (rlorl* (filter (complement
			  (lambda (ipport-rl)
			    (list-contains? listeners
					    (list
					     ;; (^use sth else than list-contains?)
					     (list->string (reverse ipport-rl)))
					    string=?)))
			 rlorl)))
    (case (length rlorl*)
      ((1)
       ;; incoming connection
       (car (ip:rlis-split (car rlorl*))))
      ((2)
       ;; outgoing connection
       #f)
      ((0)
       (error "ends end in a local listening socket? hu"))
      (else
       (error "??")))))

(TEST
 > (ip->ip->maybe-sourceIP "1.2.3.4:80->5.6.7.8:1999" '("1.2.3.4:80"))
 "5.6.7.8"
 > (ip->ip->maybe-sourceIP "1.2.3.4:80->5.6.7.8:1999" '("1.2.3.4:8080"))
 #f
 )


(define (point-listeners point *ip-addrs)
  ;; -> list of ip:port (ip really ip, '*' is expanded to (force *ip-addrs))
  (let ((*ip-addrs (delay (point-ip-addrs point
					  (lambda ()
					    (force *ip-addrs))))))
    (fold (lambda (fh res)
	    (let* ((ip:port (filehandle-filename fh))
		   (ip+port (ip:split ip:port))
		   (ip (car ip+port))
		   (port (cadr ip+port)))
	      (if (=? ip "*")
		  (append (map (lambda (ip)
				 (string-append ip ":" port))
			       ((if (filehandle-is-ipv6? fh)
				    (compose (cut1 map add-ipv6-brackets)
					     ip-addrs-ipv6)
				    ip-addrs-ipv4)
				(force *ip-addrs)))
			  res)
		  (cons ip:port res))))
	  '()
	  (filter filehandle-is-ip-listener?
		  (point-filehandles point)))))

(TEST
 > (point-listeners testpoint '("1.2.3.4"))
 ("[::1]:ssh"
  "[abcd::ef01:2345:6789:abfe]:ssh"
  "127.0.0.1:ssh"
  "192.168.1.135:ssh"))

(define (filehandle-maybe-sourceIP v listeners)
  ;; v: "1.2.3.4:www->5.6.7.8:48882"
  ;; listeners: (list "1.2.3.4:www")
  (if (filehandle-is-ip-connection? v)
      (ip->ip->maybe-sourceIP (filehandle-filename v) listeners)
      ;; (error "not an IP filehandle")
      #f))

(TEST
 > (map (lambda (fh) (filehandle-maybe-sourceIP fh (point-listeners testpoint '("1.2.3.4")))) (point-filehandles testpoint))
 (#f #f #f "8.9.10.11"))

(define-p-struct point
  time
  filehandles
  ip-addrs)

(define (point-IPconnections p)
  (filter filehandle-is-ip-connection?
	  (point-filehandles p)))


;; === extraction

(define no-ip-addrs
  (ip-addrs ipv4: '("127.0.0.1") ipv6: '("::1")))


;; if a stream missed ip-addrs in the first record, open it anew and
;; search for the first occurrence; returns no-ip-addrs object if
;; there is none.

(define (path-first-ip-addrs path)
  (let rec ((s* (open-point-stream path)))
    (let ((s (force s*)))
      (if (null? s) no-ip-addrs
	  (let ((p (car s))
		(s** (cdr s)))
	    (if (point? p)
		(point-ip-addrs p
				(lambda ()
				  (rec s**)))
		(rec s**)))))))


(define pointstream-IPconnections
  ;; -> stream of list of IPconnection
  (compose* stream-nonnull
	    (cut11 stream-map point-IPconnections)))


(define path-pp-IPconnections
  (compose* stream-pp
	    pointstream-IPconnections
	    open-point-stream))


;; === print time series

(define s. string-append)

(define _out-localtime
  (delay
    (let* ((port (open-process
		  (list path: "perl"
			arguments: (list "-wne"
					 (s. "s/^(\\d+)/localtime($1)/e; "
					     "print or die"))
			stdout-redirection: #f)))
	   (flush-thread
	    (thread-start! (make-thread (lambda ()
					  (let lp ()
					    (thread-sleep! 1)
					    (force-output port)
					    (lp)))))))
      (vector port flush-thread))))

(define (out-localtime)
  (vector-ref (force _out-localtime) 0))


(define (make-path-print printer)
  ;; printer receives (point *ip-addrs outport)
  ;; *ip-addrs may be a promise!
  (lambda (path)
    (let ((outport (out-localtime)))
      (stream-fold-left
       (lambda (point *ip-addrs)
	 (let ((*ip-addrs (point-ip-addrs point (lambda () *ip-addrs))))
	   (printer point *ip-addrs outport)
	   *ip-addrs))
       (delay (path-first-ip-addrs path))
       (open-point-stream path))
      ;; even though the flush thread should take care of this, play
      ;; safe (also to make sure the output is printed when the repl
      ;; returns):
      (force-output outport))))

(define path-print-time/numfilehandles/numIPconnections/topIP
  (make-path-print
   (lambda (p *ip-addrs outport)
     (let* ((t (point-time p))
	    (n-fh (length (point-filehandles p)))
	    (ipconns (point-IPconnections p))
	    (n-ipconn (length ipconns))
	    (top-sourceIP
	     (if (zero? n-ipconn)
		 ""
		 (let* ((l
			 (sort
			  (list-group
			   string=?
			   (sort
			    (filter
			     identity
			     (map (let ((listeners (point-listeners p
								    *ip-addrs)))
				    (lambda (conn)
				      (filehandle-maybe-sourceIP conn
								 listeners)))
				  ipconns))
			    string<?))
			  (on length >))))
		   (if (null? l)
		       ;; no incoming connection
		       " "
		       (caar l))))))
       (println port: outport
		t "  " n-fh " " n-ipconn " " top-sourceIP)))))
