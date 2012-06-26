;; Copyright (c) 2012 by Christian Jaeger.  This is free software: you
;; can redistribute it and/or modify it under the terms of the GNU
;; Lesser General Public License as published by the Free Software
;; Foundation, either version 2 of the License, or (at your option)
;; any later version. See <http://www.gnu.org/licenses/>.


(define (symbol-append . vals)
  (define (symbol-or-string->string v)
    (cond ((string? v)
	   v)
	  ((symbol? v)
	   (symbol->string v))
	  (else (error "invalid type:" v))))
  (string->symbol
   (apply string-append
          (map 
           symbol-or-string->string
           vals))))

