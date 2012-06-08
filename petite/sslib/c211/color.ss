; Provides the C211 color library, used by image and turtle

; License: source-license.txt
; If this code is used independently, copy the license here.

#|
Constructors:
  (color r g b)
    - create a pixel

Predicates:
  (color? c)
    - tests if c is a color

Accessors:
  (color-ref c b)
    - with b as 'red 'green or 'blue, access that band of a pixel

Mutators:
  (color-set! c b v) {from (c211 color)}
    - set the value of band b in color c to value v

Other:
  (color-equal? c1 c2)
    - tests if two colors are equal
  black darkgray gray lightgray white red green blue yellow cyan magenta orange pink
    - predefined colors
|#

(library
 (c211 color)
 (export
   color color? color-ref color-set! color-equal?
   black darkgray gray lightgray white red green blue yellow cyan magenta orange pink
   )

  (import (except (chezscheme) lambda define))
  (import (wombat define))

  (define :color (make-record-type "color" '(r g b)))

  ; make colors
  (define (color r g b)
    (if (or (not (integer? r)) (< r 0) (> r 255)
            (not (integer? g)) (< g 0) (> g 255)
            (not (integer? b)) (< b 0) (> b 255))
        (error 'color
          (format "#[color ~a ~a ~a] is not a valid color, range is 0-255"
            r g b))
        ((record-constructor :color) r g b)))

  ; check if something is a color
  (define color? (record-predicate :color))

  ; verifiers
  (define (make-check pred? name)
    (lambda (proc v)
      (when (not (pred? v))
        (error proc (format "~a is not the correct type, expected ~a" v name)))))

  (define !check-integer (make-check integer? "integer"))
  (define !check-procedure (make-check procedure? "procedure"))
  (define !check-color (make-check color? "color"))

  (define !check-band
    (make-check
      (lambda (b) (or (and (integer? b) (<= 0 b 2))
                      (member b '(red green blue))))
      "a band number, 'red, 'green, or 'blue"))

  ; get a band out of a color
  (define (color-ref c b)
    (!check-color 'color-ref c)
    (!check-band 'color-ref b)
    ((record-accessor :color
       (if (eq? b 'red) 0 (if (eq? b 'green) 1 (if (eq? b 'blue) 2 b)))) c))

  ; set a band in a color
  (define (color-set! c b v)
    (!check-color 'color-set! c)
    (!check-band 'color-set! b)
    ((record-mutator :color
		     (case b
		       [(0 red) 0]
		       [(1 green) 1]
		       [(2 blue) 2]))
     c v))      

  ; tests if colors are equal
  (define (color-equal? c1 c2)
    (and (color? c1)
         (color? c2)
         (= (color-ref c1 'red) (color-ref c2 'red))
         (= (color-ref c1 'green) (color-ref c2 'green))
         (= (color-ref c1 'blue) (color-ref c2 'blue))))

  ; predefined colors
  (define black (color 0 0 0))
  (define darkgray (color 84 84 84))
  (define gray (color 192 192 192))
  (define lightgray (color 205 205 205))
  (define white (color 255 255 255))
  (define red (color 255 0 0))
  (define green (color 0 255 0))
  (define blue (color 0 0 255))
  (define yellow (color 255 255 0))
  (define cyan (color 0 255 255))
  (define magenta (color 255 0 255))
  (define orange (color 255 127 0))
  (define pink (color 188 143 143))
)