; License: source-license.txt
; If this code is used independently, copy the license here.

(library (wombat define)
  (export lambda define)
  (import (except (chezscheme) let lambda define))
  (import (prefix (only (chezscheme) let define) chez:))

  (define-syntax lambda
    (syntax-rules ()
      [(_ a b b* ...)
       (case-lambda
         [a b b* ...]
         [others
           (error 'procedure
             (format "incorrect argument count in ~a"
               '(lambda a b b* ...)))])]))

  (define-syntax named-lambda
    (syntax-rules ()
      [(_ name a b b* ...)
       (case-lambda
         [a b b* ...]
         [others
           (error 'name "incorrect argument count")])]))

  (define-syntax define
    (syntax-rules (lambda)
      [(_ name)
       (chez:define name (void))]
      [(_ name (lambda a b b* ...))
       (chez:define name (named-lambda name a b b* ...))]
      [(_ (name a* ...) b b* ...)
       (chez:define name (named-lambda name (a* ...) b b* ...))]
      [(_ (name . a*) b b* ...)
       (chez:define name (named-lambda name a* b b* ...))]
      [(_ n v)
       (chez:define n v)])))
