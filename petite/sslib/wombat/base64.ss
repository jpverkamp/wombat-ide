; License: source-license.txt
; If this code is used independently, copy the license here.

(library
  (wombat base64)
  (export
    string->base64
    base64->string
    ;bytevector->base64
    ;base64->bytevector
)

  (import (except (chezscheme) lambda define))
  (import (wombat define))

  (define >> bitwise-arithmetic-shift-right)
  (define << bitwise-arithmetic-shift-left)
  (define && logand)
  (define // logor)

  (define base64-alpha
    "ABCDEFGHIJKLMNOPQRSTUVWXYZabcdefghijklmnopqrstuvwxyz0123456789+/=")

  (define (un-string-ref str c)
    (let ^ ([i 0])
      (cond
        [(= i (string-length str)) #f]
        [(char=? c (string-ref str i)) i]
        [else (^ (+ i 1))])))

  (define (string->base64 str)
    (let ([len (* (div (string-length str) 3) 3)])
      (define (^ i ls)
        (cond
          [(>= i len)
           (cond
             [(= (mod (string-length str) 3) 1)
              (let* ([c0 (char->integer (string-ref str i))]
                     [b0 (>> c0 2)]
                     [b1 (<< (&& c0 #b11) 4)])
                `(64 64 ,b1 ,b0 . ,ls))]
             [(= (mod (string-length str) 3) 2)
              (let* ([c0 (char->integer (string-ref str i))]
                     [c1 (char->integer (string-ref str (+ i 1)))]
                     [b0 (>> c0 2)]
                     [b1 (// (<< (&& c0 #b11) 4) (>> c1 4))]
                     [b2 (<< (&& c1 #b1111) 2)])
                `(64 ,b2 ,b1 ,b0 . ,ls))]
             [else ls])]
          [else
           (let* ([c0 (char->integer (string-ref str i))]
                  [c1 (char->integer (string-ref str (+ i 1)))]
                  [c2 (char->integer (string-ref str (+ i 2)))]
                  [b0 (>> c0 2)]
                  [b1 (// (<< (&& c0 #b000011) 4) (&& (>> c1 4) #b111111))]
                  [b2 (// (<< (&& c1 #b001111) 2) (&& (>> c2 6) #b111111))]
                  [b3 (&& c2 #b111111)])
             (^ (+ i 3) `(,b3 ,b2 ,b1 ,b0 . ,ls)))]))
      (let ([ls (^ 0 '())])
        (list->string (map
                        (lambda (i) (string-ref base64-alpha i))
                        (reverse ls))))))
#|
  (define (bytevector->base64 bv)
    (string->base64 (multibyte->string 'cp-utf8 bv)))
|#
  (define (base64->string str)
    (define (^ i ls)
      (if (>= i (string-length str))
          ls
          (let* ([b0 (un-string-ref base64-alpha (string-ref str i))]
                 [b1 (un-string-ref base64-alpha (string-ref str (+ i 1)))]
                 [b2 (un-string-ref base64-alpha (string-ref str (+ i 2)))]
                 [b3 (un-string-ref base64-alpha (string-ref str (+ i 3)))]
                 [i^ (if (= b2 64) 1 (if (= b3 64) 2 3))]
                 [c0 (// (<< (&& b0 #b111111) 2) (&& (>> b1 4) #b111111))]
                 [c1 (// (<< (&& b1 #b001111) 4) (&& (>> b2 2) #b111111))]
                 [c2 (// (<< (&& b2 #b000011) 6) (&& (>> b3 0) #b111111))])
            (cond
              [(= i^ 1) (^ (+ i 4) `(,c0 . ,ls))]
              [(= i^ 2) (^ (+ i 4) `(,c1 ,c0 . ,ls))]
              [(= i^ 3) (^ (+ i 4) `(,c2 ,c1 ,c0 . ,ls))]))))
    (let ([ls (^ 0 '())])
      (list->string (map
                      (lambda (i) (integer->char i))
                    (reverse ls)))))
#|
  (define (base64->bytevector str)
    (string->multibyte 'cp-utf8 (base64->string str)))
|#
)