

(library
  (c211 turtle)
  (export
    spawn split
    block repeat
    move! move-to! turtle-location
    turn-left! turn-right! turn-to! turtle-direction
    lift-pen! drop-pen! pen-up/down?
    set-pen-color! pen-color
    draw-turtle
    turtle->image)

  (import (except (chezscheme) lambda define))
  (import (wombat define))
  (import (wombat java))

  (import (c211 image))

  (define-record turtle (tick x y dir up/down color children lines))
  (define-record line (tick x0 y0 x1 y1 color))

  (define (tick! t) (set-turtle-tick! t (+ 1 (turtle-tick t))))
  (define (record! t l) (set-turtle-lines! t (cons l (turtle-lines t))))

  (define pi 3.141592654)
  (define (d->r d) (/ (* d pi) 180))
  (define (r->d r) (/ (* r 180) pi))

  ; spawn a new turtle
  (define spawn
    (case-lambda
      [() (make-turtle 0 0.0 0.0 0.0 'down black '() '())]
      [(dir) (make-turtle 0 0.0 0.0 dir 'down black '() '())]
      [(x y) (make-turtle 0 x y 0.0 'down black '() '())]
      [(x y dir) (make-turtle 0 x y dir 'down black '() '())]
      [(x y dir up/down color) (make-turtle 0 x y dir up/down color '() '())]))

  ; split a turtle into two turtles
  (define (split t)
    (tick! t)
    (let ([new-t
            (make-turtle
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
    (when (eq? 'down (turtle-up/down t))
      (record! t
        (make-line (turtle-tick t)
          (turtle-x t) (turtle-y t) new-x new-y (turtle-color t))))
    (tick! t)
    (set-turtle-x! t new-x)
    (set-turtle-y! t new-y))

  ; get the turtle's current location
  (define (turtle-location t)
    (list (turtle-x t) (turtle-y t)))

  ; turn a turtle t left by d degrees
  (define (turn-left! t d)
    (turn-right! t (- d)))

  ; turn a turtle t right by d degrees
  (define (turn-right! t d)
    (let ([r (d->r d)])
      (tick! t)
      (set-turtle-dir! t (+ r (turtle-dir t)))))

  ; turn a turtle t directly to a given direction d (in degrees clockwise from up)
  (define (turn-to! t d)
    (tick! t)
    (set-turtle-dir! t (d->r d)))

  ; get the turtle's current rotation in degrees
  (define (turtle-direction t)
    (r->d (turtle-dir t)))

  ; lift the turtle's pen or put it down
  (define (lift-pen! t) (tick! t) (set-turtle-up/down! t 'up))
  (define (drop-pen! t) (tick! t) (set-turtle-up/down! t 'down))
  (define (pen-up/down? t) (turtle-up/down t))

  ; change the pen's color
  (define (set-pen-color! t c) (tick! t) (set-turtle-color! t c))
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
                         (shorten (line-x0 line))
                         (shorten (line-y0 line))
                         (shorten (line-x1 line))
                         (shorten (line-y1 line))
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

  (define (shorten n) (* 0.01 (round (* 100.0 n))))
  (define (fix x) (if (number? x) (shorten x) x))

(record-writer (type-descriptor turtle)
  (lambda (r p wr)
    (display "#[turtle (" p)
    (for-each
      (lambda (f) (display (fix (f r)) p))
      (list turtle-x (lambda (_) " ") turtle-y))
    (display ")" p)
    (for-each
      (lambda (f) (display " " p) (display (fix (f r)) p))
      (list turtle-dir turtle-up/down turtle-color))
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