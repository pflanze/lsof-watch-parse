;; Copyright (c) 2012 by Christian Jaeger <ch@christianjaeger.ch>.
;; This is free software: you can redistribute it and/or modify it
;; under the terms of the GNU Lesser General Public License as
;; published by the Free Software Foundation, either version 2 of the
;; License, or (at your option) any later version. See
;; <http://www.gnu.org/licenses/>.


(define-macro (assert expr)
  `(if (not ,expr)
       (error ,(string-append "assertment failure: "
                              (object->string expr)))))

(define-macro (named name expr)
  `(letrec ((,name ,expr))
     ,name))

