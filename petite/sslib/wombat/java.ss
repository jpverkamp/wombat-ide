(library (wombat java)
  (export call-to-java)
  (import (chezscheme))

  (define-syntax call-to-java
    (syntax-rules ()
      [(_ n a* ...)
       (let ()
         (format "~s~a" 'n
           (apply string-append
             (map (lambda (a) (format " ~a" a))
               (list a* ...))))
         (read))])))