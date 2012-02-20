(library
 (c211 interop)
 (export
   call-to-java)

  (import (chezscheme))

  (define-syntax call-to-java
    (syntax-rules ()
      [(_ name)
       (printf "|!~a|!" 'name)]
      [(_ name args ...)
       (printf "|!~a~a|!" 'name
         (apply string-append
           (map
             (lambda (x) (format " ~a" x))
             (list args ...))))])))