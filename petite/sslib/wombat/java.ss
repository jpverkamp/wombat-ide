; License: source-license.txt
; If this code is used independently, copy the license here.

(library (wombat java)
  (export call-to-java)
  (import (chezscheme))

  (define-syntax call-to-java
    (syntax-rules ()
      [(_ n a* ...)
       (let ()
         ; send the command to java
         (printf "|!~s~a|!" 'n
           (apply string-append
             (map (lambda (a) (format " ~a" a))
               (list a* ...))))

         ; get the result back
         (let ([result (read)])
           (if (and (not (null? result)) (eq? (car result) 'exception))
               (apply error (cdr result))
               result)))])))