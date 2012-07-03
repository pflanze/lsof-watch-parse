;; Copyright (c) 2012 by Christian Jaeger <ch@christianjaeger.ch>.
;; This is free software: you can redistribute it and/or modify it
;; under the terms of the GNU Lesser General Public License as
;; published by the Free Software Foundation, either version 2 of the
;; License, or (at your option) any later version. See
;; <http://www.gnu.org/licenses/>.


;; === parse scripting languge compatibility s-expression based readsyntax

(define (=? a b)
  (if (string? a)
      (if (string? b)
	  (string=? a b)
	  #f)
      (if (number? a)
	  (if (number? b)
	      (= a b)
	      #f)
	  (error "only supporting numbers and strings"))))

;; == table

(define (_table? v)
  (and (pair? v)
       (eq? (car v) 'table)))

(define (_table-body v)
  (if (_table? v)
      (cdr v)
      (error "not a table")))

(define (_item? v)
  (and (pair? v)
       (eq? (car v) 'item)
       (pair? (cdr v))
       (pair? (cddr v))
       (null? (cdddr v))))

(define (@item-k v)
  (cadr v))
(define (@item-v v)
  (caddr v))

(define (_item-key= i k)
  (if (_item? i)
      (string=? (@item-k i) k)
      (error "not an item" i)))

(define (_table-ref t k)
  (let lp ((b (_table-body t)))
    (if (null? b)
	(error "key not found in table:" k t)
	(let ((i (car b)))
	  (if (_item-key= i k)
	      (@item-v i)
	      (lp (cdr b)))))))

;; == list

(define (_list? v)
  (and (pair? v)
       (eq? (car v) 'list)))

(define (_list-body v)
  (if (_list? v)
      (cdr v)
      (error "not a list")))

(define _list->list _list-body)


;; === parse new readsyntax

(define (readsyntax->internal v #!optional (refs (make-table)))
  ;; refs contains references to the *converted* values, of course.
  (let rec ((v v))
    (cond ((number? v)
	   v)
	  ((string? v)
	   v)
	  ((boolean? v)
	   v)
	  ((pair? v)
	   (let ((a (car v)))
	     (if (symbol? a)
		 (case a
		   ((list)
		    (map rec (cdr v)))
		   ((ref)
		    (let ((id (cadr v)))
		      (cond ((table-ref refs id #f)
			     ;; (can use #f as miss indicator since
			     ;; no pointers to it need ever be
			     ;; shared.)
			     => (lambda (val)
				  val))
			    (else
			     ;; value has not yet been converted
			     ;; (circular datastructures).  Need to
			     ;; mutate the container afterwards.
			     (error "circular datastructures not yet implemented")))))
		   ((table)
		    (error "table not yet implemented"))
		   ((named)
		    (let ((id (cadr v))
			  (val (caddr v)))
		      (let ((val* (rec val)))
			(table-set! refs id val*)
			val*)))
		   ((object)
		    (let ((quoted-class (cadr v))
			  (kv-s (cddr v)))
		      (assert (and (pair? quoted-class)
				   (eq? (car quoted-class) 'quote)
				   (pair? (cdr quoted-class))
				   (null? (cddr  quoted-class))))
		      ;; kv-s = key val 's (flat).
		      (let ((class (cadr quoted-class))
			    (vec (make-vector (+ (length kv-s) 1))))
			(assert (symbol? class))
			(vector-set! vec 0 class)
			(let lp ((i 1)
				 (kv-s kv-s))
			  (if (null? kv-s)
			      vec
			      (let ((k (car kv-s))
				    (v (cadr kv-s))
				    (kv-s* (cddr kv-s))
				    (i* (+ i 2)))
				(vector-set! vec i k)
				(vector-set! vec (+ i 1) (rec v))
				(lp i* kv-s*)))))))
		   (else
		    (error "unknown s-expression head in:" v)))
		 (error "invalid type of s-expression head in:" v))))
	  (else
	   (error "unknown type of read datum:" v)))))

(TEST
 > (readsyntax->internal '(list 1))
(1)
;; > (readsyntax->internal '(named 1 (list 1 (ref 1))))
;; *** ERROR IN rec, "p.scm"@186.11 -- circular datastructures not yet implemented
> (readsyntax->internal '(list (named 1 (list 1)) (ref 1)))
((1) (1))
> (eq? (car #) (cadr #))
#t
> (readsyntax->internal '(object 'foo a: 1 b: "B"))
#(foo a: 1 b: "B")
)

;; heh btw. constuctor as in new readsyntax
(define (object class . args)
  (apply vector class args))
(define (object* class args)
  (apply vector class args))


;; ===  parse old readsyntax

;; toplevel datum has positional args (list), filehandles and
;; processes have keyword args (table)

;; == toplevel record accessors (timestamp + list)

(define (old:_toplevel? v)
  (and (_list? v)
       (= (length (_list-body v)) 2);;
       (_list? (cadr (_list-body v)))))

(define (old:_toplevel-body v)
  (if (old:_toplevel? v)
      (cdr v)
      (error "not a toplevel list")))

(define (old:_toplevel-timestamp v)
  (car (old:_toplevel-body v)))
(define (old:_toplevel-filehandles v)
  (cadr (old:_toplevel-body v)))


;; == conversion

(define (old:make-table->object class convert-val)
  (lambda (v)
    (object* class
	     (fold (lambda (item res)
		     (if (_item? item)
			 (cons (string->keyword (@item-k item))
			       (cons (convert-val (@item-v item))
				     res))
			 (error "not an item" item)))
		   '()
		   (_table-body v)))))

(define old:_process->process
  (old:make-table->object
   'process
   identity))

(define (old:_toplevel->point v)
  (let ((refs (make-table)))
    
    (define old:_filehandle->filehandle
      (old:make-table->object
       'filehandle
       (named
	rec
	(lambda (v)
	  ;; tables here are process entries; also, there's (ref ) and (named )
	  (if (pair? v)
	      (case (car v)
		;; COPY
		((named)
		 (let ((id (cadr v))
		       (val (caddr v)))
		   (let ((val* (rec val)))
		     (table-set! refs id val*)
		     val*)))
		((ref)
		 (let ((id (cadr v)))
		   (cond ((table-ref refs id #f)
			  ;; (can use #f as miss indicator since
			  ;; no pointers to it need ever be
			  ;; shared.)
			  => (lambda (val)
			       val))
			 (else
			  ;; value has not yet been converted
			  ;; (circular datastructures).  Need to
			  ;; mutate the container afterwards.
			  (error "circular datastructures not yet implemented")))))
		;;/COPY
		(else
		 (old:_process->process v)))
	      v)))))

    (object 'point
	    time:
	    (old:_toplevel-timestamp v)
	    filehandles:
	    (map (lambda (v)
		   ;; also handle format where toplevel was old style
		   ;; but inside was new style
		   (if (_table? v)
		       (old:_filehandle->filehandle v)
		       (readsyntax->internal v refs)))
		 (_list->list (old:_toplevel-filehandles v))))))

(TEST
 > (old:_toplevel->point '(list 1234 (list (table (item "a" 9) (item "b" (named 7 (table (item "pid" 9887) (item "nam" "foo"))))) (table (item "a" 10) (item "b" (ref 7))))))
 #(point
   time:
   1234
   filehandles:
   (#(filehandle b: #(process nam: "foo" pid: 9887) a: 9)
     #(filehandle b: #(process nam: "foo" pid: 9887) a: 10)))
 )


;; ===  parse any of the old or new readsyntax

(define (anysyntax-toplevel->internal v)
  ((if (old:_toplevel? v)
       old:_toplevel->point
       readsyntax->internal) v))


