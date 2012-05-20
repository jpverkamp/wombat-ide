; Provides the C211 image library

; License: source-license.txt
; If this code is used independently, copy the license here.

#|
See exports for definitions.

From color:
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
  (c211 turtle)
  (export
    ; constructors
    hatch ; (hatch ...) create a new turtle, see code for options
    clone ; (clone turtle) create a copy of a given turtle
    ; display parameters
    live-display ; (live-display #t/#f) enable or display the live display
    live-delay ; (live-delay seconds) delay between steps in live-display move
    ; macros for branching
    block ; (block turtle cmds ...) store turtle state and restore after cmds
    repeat ; (repeat n cmds ...) repeat cmds n times for their side effects
    ; movement
    move! ; (move! turtle n) move fowards n units
    move-to! ; (move-to! turtle x y) teleport directly to a given location
    teleport! ; (teleport! turtle x y) alias for move-to!
    turtle-location ; (turtle-location turtle) get the (x y) of the turtle
    ; direction
    turn-left! ; (turn-left! turtle d) turn left (counterclockwise) d degrees
    turn-right! ; (turn-right! turtle d) turn right (clockwise) d degrees
    turn-counter-clockwise! ; (turn-counterclockwise! turtle d) turn-left! alias
    turn-clockwise! ; (turn-clockwise! turtle d) alias for turn-right!
    turn! ; (turn! turtle d) alias for turn-right!
    turn-to! ; (turn-to! turtle d) turn to a given facing (0 north, + is right)
    turtle-direction ; (turtle-direction t) get the facing for a turtle
    ; drawing
    lift-pen! ; (lift-pen! turtle) stop drawing but allow movement
    drop-pen! ; (drop-pen! turtle) start drawing
    pen-up/down ; (pen-up/down turtle) test if the pen is 'up or 'down
    set-pen-color! ; (set-pen-color! turtle color) set the pen, use (c211 image)
    pen-color ; (pen-color turtle) get the pen color
    ; display
    draw-turtle ; (draw-turtle turtle) draw this turtle and any cloned from it
    turtle->image ; (turtle->image turtle) convert this turtle into (c211 image)
    )

  (import (except (chezscheme) lambda define))
  (import (wombat define))
  (import (wombat java))

  (import (c211 color))
  (import (c211 image))

  (export (import (c211 color)))

  (define live-display (make-parameter #f))
  (define live-display-timer (make-parameter 0.1))

  (define live-delay
    (case-lambda
      [() (live-display-timer)]
      [(n)
       (call-to-java update-live-timer n)
       (live-display-timer n)]))

  (define-record turtle (id tick x y dir up/down color children lines))
  (define-record line (tick x0 y0 x1 y1 color))

  (define (tick! t) (set-turtle-tick! t (+ 1 (turtle-tick t))))
  (define (record! t l) (set-turtle-lines! t (cons l (turtle-lines t))))

  (define (live! t args)
    (when (live-display)
      (call-to-java turtle-update (turtle-id t) (map fix args))
      (void)))

  (define pi 3.141592654)
  (define (d->r d) (/ (* d pi) 180))
  (define (r->d r) (/ (* r 180) pi))

  ; wrapper to record spawns
  (define (make-turtle^ . args)
    (let ([t (apply make-turtle args)])
      (live! t `(spawn
                  ,(turtle-x t)
                  ,(turtle-y t)
                  ,(turtle-dir t)
                  ,(turtle-up/down t)
                  ,(color-ref (turtle-color t) 'red)
                  ,(color-ref (turtle-color t) 'green)
                  ,(color-ref (turtle-color t) 'blue)))
      t))

  ; create a new turtle
  (define hatch
    (case-lambda
      [()
       (make-turtle^ (gensym) 0 0.0 0.0 0.0 'down black '() '())]
      [(dir)
       (make-turtle^ (gensym) 0 0.0 0.0 (d->r dir) 'down black '() '())]
      [(x y)
       (make-turtle^ (gensym) 0 x y 0.0 'down black '() '())]
      [(x y dir)
       (make-turtle^ (gensym) 0 x y (d->r dir) 'down black '() '())]
      [(x y dir up/down color)
       (make-turtle^ (gensym) 0 x y (d->r dir) up/down color '() '())]))

  ; create a clone of a given turtle
  (define (clone t)
    (tick! t)
    (let ([new-t
            (make-turtle^
              (gensym)
              (turtle-tick t)
              (turtle-x t)
              (turtle-y t)
              (turtle-dir t)
              (turtle-up/down t)
              (turtle-color t)
              '()
              '())])
      (set-turtle-children! t (cons new-t (turtle-children t)))
      new-t))

  ; run a block of commands with a turtle then reset the state at the end
  (define-syntax block
    (syntax-rules ()
      [(_ t b* ...)
       (let ([orig-x (turtle-x t)]
             [orig-y (turtle-y t)]
             [orig-dir (turtle-dir t)]
             [orig-u/d (turtle-up/down t)]
             [orig-clr (turtle-color t)])
         (let ([result (begin b* ...)])
           (set-turtle-x! t orig-x)
           (set-turtle-y! t orig-y)
           (set-turtle-dir! t orig-dir)
           (set-turtle-up/down! t orig-u/d)
           (set-turtle-color! t orig-clr)
           (live! t `(block-reset
                       ,(turtle-x t)
                       ,(turtle-y t)
                       ,(turtle-dir t)
                       ,(turtle-up/down t)
                       ,(color-ref (turtle-color t) 'red)
                       ,(color-ref (turtle-color t) 'green)
                       ,(color-ref (turtle-color t) 'blue)))
           result))]))

  ; repeat a set of commands a given number of times
  (define-syntax repeat
    (syntax-rules ()
      [(_ n b* ...)
       (let loop ([i 0])
         (when (< i n)
           (begin b* ...)
           (loop (add1 i))))]))

  ; move a turtle t forward by n units (or backwards if negative)
  (define (move! t n)
    (move-to! t
      (+ (turtle-x t) (* n (sin (turtle-dir t))))
      (+ (turtle-y t) (* n (cos (turtle-dir t))))))

  ; move a turtle t directly to a given point x,y regardless of facing
  (define (move-to! t new-x new-y)
    (live! t `(move ,new-x ,new-y ,(turtle-up/down t)))
    (when (eq? 'down (turtle-up/down t))
      (record! t
        (make-line (turtle-tick t)
          (turtle-x t) (turtle-y t) new-x new-y (turtle-color t))))
    (tick! t)
    (set-turtle-x! t new-x)
    (set-turtle-y! t new-y))
  (define teleport! move-to!)

  ; get the turtle's current location
  (define (turtle-location t)
    (list (turtle-x t) (turtle-y t)))

  ; turn a turtle t left by d degrees
  (define (turn-left! t d)
    (turn-to! t (r->d (- (turtle-dir t) (d->r d)))))
  (define turn-counter-clockwise! turn-left!)

  ; turn a turtle t right by d degrees
  (define (turn-right! t d)
    (turn-to! t (r->d (+ (turtle-dir t) (d->r d)))))
  (define turn-clockwise! turn-right!)

  ; turn right by default
  (define turn! turn-right!)

  ; turn a turtle t directly to a given direction d (in degrees clockwise from up)
  (define (turn-to! t d)
    (live! t `(turn ,(mod d 360)))
    (tick! t)
    (set-turtle-dir! t (d->r d)))

  ; get the turtle's current rotation in degrees
  (define (turtle-direction t)
    (mod (r->d (turtle-dir t)) 360))

  ; lift the turtle's pen or put it down
  (define (lift-pen! t)
    (live! t `(pen up)) (tick! t) (set-turtle-up/down! t 'up))
  (define (drop-pen! t)
    (live! t `(pen down)) (tick! t) (set-turtle-up/down! t 'down))
  (define (pen-up/down t) (turtle-up/down t))

  ; change the pen's color
  (define (set-pen-color! t c)
    (live! t `(pen-color
                ,(color-ref c 'red)
                ,(color-ref c 'green)
                ,(color-ref c 'blue)))
    (tick! t)
    (set-turtle-color! t c))
  (define (pen-color t) (turtle-color t))

  ; get all lines from a turtle and it's children
  (define (get-lines t)
    (append (turtle-lines t)
      (if (null? (turtle-children t))
          '()
          (apply append (map get-lines (turtle-children t))))))

  ; convert a turtle to a string for sending to Java
  (define (turtle->string t)
    (apply string-append
      (map (lambda (line)
             (string-append
               (apply string-append
                 (map (lambda (x) (format "~a " x))
                   (list (line-tick line)
                         (fix (line-x0 line))
                         (fix (line-y0 line))
                         (fix (line-x1 line))
                         (fix (line-y1 line))
                         (color-ref (line-color line) 'red)
                         (color-ref (line-color line) 'green)
                         (color-ref (line-color line) 'blue))))
               "\n"))
        (sort (lambda (l1 l2) (< (line-tick l1) (line-tick l2)))
          (get-lines t)))))

  ; export to java
  (define (draw-turtle t)
    (call-to-java draw-turtle (turtle->string t))
    (void))

  ; convert to an image
  (define (turtle->image t)
    (apply base64->image (call-to-java turtle->image (turtle->string t))))

  (define (fix x) (if (and (number? x) (inexact? x))  (format "~,3f" x) x))

(record-writer (type-descriptor turtle)
  (lambda (r p wr)
    (display "#[turtle (" p)
    (for-each
      (lambda (f) (display (fix (f r)) p))
      (list turtle-x (lambda (_) " ") turtle-y))
    (display ")" p)
    (display " " p) (display (r->d (turtle-dir r)) p)
    (for-each
      (lambda (f) (display " " p) (display (fix (f r)) p))
      (list turtle-up/down turtle-color))
    (display "]" p)))

(record-writer (type-descriptor line)
  (lambda (r p wr)
    (display "#[line " p)
    (display (line-tick r) p)
    (display " (" p)
    (for-each
      (lambda (f) (display (fix (f r)) p))
      (list line-x0 (lambda (_) " ") line-y0))
    (display ") (" p)
    (for-each
      (lambda (f) (display (fix (f r)) p))
      (list line-x1 (lambda (_) " ") line-y1))
    (display ") " p)
    (display (line-color r) p)
    (display "]" p)))
)

  #|

(import (c211 turtle))

(let* ([t (spawn)]
       [u (split! t)])
  (turn-right! t 90)
  (move! t 5)
  (lift-pen! t)
  (move! t -5)
  (drop-pen! t)
  (move! t 5)
  (turn-left! u 90)
  (move! u 5)
  (printf "~a\n" t)
  (draw-turtle t))
      |#