; Provides the C211 matrix library

; License: source-license.txt
; If this code is used independently, copy the license here.

#|
Constructors:
(matrix-matrix rs cs [i=0])
- create an rs x cs matrix with optional default value i
(matrix-generator rs cs p)
- create an rs x cs matrix, setting the value at r,c to (p r c)

Predicates:
(matrix? m)
- tests if something is a matrix

Accessors:
(matrix-rows m)
- how many rows the matrix has
(matrix-cols m)
- how many columns the matrix has
(matrix-ref m r c)
- get the value out of the matrix m at r,c

Mutators:
(matrix-set! m r c v)
- set the value of the matrix m at r,c to v

Other:
(print-matrix m)
- print the matrix to the console (only the top left corner for large ones)
(draw-matrix m)
- display a graphical matrix (will show all of the values, uses Java)

Parameters:
print-matrix-rows = the maximum number of printed rows
print-matrix-cols = the maximum number of printed columns
print-matrix-width = the maximum width of elements to print
|#

(library
  (c211 matrix)
  (export
    make-matrix matrix-generator
    matrix?
    matrix-rows matrix-cols matrix-ref
    matrix-set!
    vov->matrix matrix->vov
    print-matrix draw-matrix
    print-matrix-rows print-matrix-cols print-matrix-width)

  (import (except (chezscheme) lambda define))
  (import (wombat define))
  (import (wombat java))

  ; create the datatypes
  (define :matrix (make-record-type "matrix" '(rows cols data)))

  ; build a matrix
  (define make-matrix
    (case-lambda
      [(rs cs)
       (!check-integer 'make-matrix rs)
       (!check-integer 'make-matrix cs)
       ((record-constructor :matrix) rs cs (make-vector (* rs cs)))]
      [(rs cs i)
       (!check-integer 'make-matrix rs)
       (!check-integer 'make-matrix cs)
       (matrix-generator rs cs (lambda (_1 _2) i))]))

  ; generate a matrix using a generating function based on row and column
  (define (matrix-generator rs cs p)
    (!check-integer 'matrix-generator rs)
    (!check-integer 'matrix-generator cs)
    (!check-procedure 'matrix-generator p)
    (let ([m (make-matrix rs cs)])
      (let ^ ([r 0] [c 0])
        (cond
          [(= r (matrix-rows m)) m]
          [(= c (matrix-cols m)) (^ (+ r 1) 0)]
          [else
           (matrix-set! m r c (p r c))
           (^ r (+ c 1))]))))

  ; test if something is a matrix
  (define matrix? (record-predicate :matrix))

  ; get the size of the matrix
  (define matrix-rows (record-accessor :matrix 0))
  (define matrix-cols (record-accessor :matrix 1))
  (define matrix-data (record-accessor :matrix 2))

  ; verifiers
  (define (make-check pred? name)
    (lambda (proc v)
      (when (not (pred? v))
        (error proc (format "~a is not of type ~a" v name)))))

  (define !check-integer (make-check integer? "integer"))
  (define !check-procedure (make-check procedure? "procedure"))
  (define !check-matrix (make-check matrix? "matrix"))

  (define (!check-bounds proc i r c)
    (!check-integer proc r)
    (!check-integer proc c)
    (when (or (< r 0) (>= r (matrix-rows i))
              (< c 0) (>= c (matrix-cols i)))
      (error proc
        (format "(~a, ~a) is not a valid index for ~a" r c i))))

  ; access values in a matrix
  (define (matrix-ref m r c)
    (!check-matrix 'matrix-ref m)
    (!check-bounds 'matrix-ref m r c)
    (vector-ref (matrix-data m) (+ (* r (matrix-cols m)) c)))

  ; mutate the matrix
  (define (matrix-set! m r c v)
    (!check-matrix 'matrix-ref m)
    (!check-bounds 'matrix-set! m r c)
    (vector-set! (matrix-data m) (+ (* r (matrix-cols m)) c) v))

  ; convert a vector of vectors into a matrix
  (define (vov->matrix vov)
    (if (zero? (vector-length vov))
        (make-matrix 0 0)
        (matrix-generator (vector-length vov) (vector-length (vector-ref vov 0))
          (lambda (i j)
            (vector-ref (vector-ref vov i) j)))))

  ; convert a matrix into a vector of vectors
  (define (matrix->vov m)
    (apply vector
      (map (lambda (i)
             (apply vector
               (map (lambda (j)
                      (matrix-ref m i j))
                 (iota (matrix-cols m)))))
        (iota (matrix-rows m)))))

  ; display the matrix
  (define print-matrix-rows (make-parameter 10))
  (define print-matrix-cols (make-parameter 10))
  (define print-matrix-width (make-parameter 5))
  (define (print-matrix m)
    (!check-matrix 'print-matrix m)
    (let ^ ([r 0] [c 0])
      (cond
        [(> r (print-matrix-rows)) (printf "...\n") (void)]
        [(= r (matrix-rows m)) (printf "\n") (void)]
        [(> c (print-matrix-cols)) (printf "... \n") (^ (+ r 1) 0)]
        [(= c (matrix-cols m)) (printf "\n") (^ (+ r 1) 0)]
        [else
         (printf
           (let ([s (format "~a" (matrix-ref m r c))])
             (if (>= (string-length s) (print-matrix-width))
                 (string-append (substring s 0 (- (print-matrix-width) 1)) "~~ ")
                 (format
                   (string-append "~"
                     (number->string (print-matrix-width)) "a ")
                   s))))
         (^ r (+ c 1))])))

  ; draw the matrix (uses the network connection to Java)
  (define (draw-matrix m)
    (let ^ ([r 0] [c 0]
            [s (format "~a\n~a\n" (matrix-rows m) (matrix-cols m))])
      (cond
        [(= r (matrix-rows m))
         (call-to-java draw-matrix s)
         (void)]
        [(= c (matrix-cols m))
         (^ (+ r 1) 0 s)]
        [else
         (^ r (+ c 1) (string-append s
                        (format "~a\n" (matrix-ref m r c))))])))

  ; tweak how matricies are printed
  (record-writer :matrix
    (lambda (r p wr)
      (display "#[matrix " p)
                   (wr (matrix-rows r) p)
                   (display " " p)
                   (wr (matrix-cols r) p)
                   (display "]" p)))
                     )