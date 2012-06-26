;; Copyright (c) 2012 by Christian Jaeger.  This is free software: you
;; can redistribute it and/or modify it under the terms of the GNU
;; Lesser General Public License as published by the Free Software
;; Foundation, either version 2 of the License, or (at your option)
;; any later version. See <http://www.gnu.org/licenses/>.


;; === infrastructure

(define (open-stream-read-all path)
  (if (string-ends? path ".gz")
      (stream-read-all (open-process (list path: "zcat"
					   arguments: (list path)))
		       xclose-process)
      (stream-read-all (open-input-file path)
		       close-port)))

(define (open-point-stream path)
  (stream-map anysyntax-toplevel->internal
	      (open-stream-read-all path)))

(define (stream-pp s)
  (stream-for-each pretty-print s))

(define (stream-nonnull s)
  (stream-filter
   (complement null?)
   s))


