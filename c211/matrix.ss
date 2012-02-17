; Provides the C211 tree modules
(library
  (c211 matrix)
  (export
    make-matrix ; (_ rs cs [i]) make a new rs x cs matrix (optional default i)
    matrix?     ; (_ m) check if something is a matrix
    matrix-rows ; (_ m) get the number of rows in the matrix
    matrix-cols ; (_ m) get the number of columns in the matrix
    matrix-ref  ; (_ m r c) get the value of m at (r, c)
    matrix-set! ; (_ m r c v) set the value of m at (r, c) to v
    draw-matrix ; (_ m) print the matrix to the current output port
    matrix-generator ; (_ rs cs p) generate a rs x cs matrix calling (p r c)
    )

  (import (chezscheme))

  ; create the datatypes
  (define :matrix (make-record-type "matrix" '(rows cols data)))

  ; build a matrix
  (define make-matrix
    (case-lambda
      [(rs cs)
       ((record-constructor :matrix) rs cs (make-vector (* rs cs)))]
      [(rs cs i)
       (matrix-generator rs cs (lambda (_) i))]))

  ; test if something is a matrix
  (define matrix? (record-predicate :matrix))

  ; get the size of the matrix
  (define matrix-rows (record-accessor :matrix 0))
  (define matrix-cols (record-accessor :matrix 1))
  (define matrix-data (record-accessor :matrix 2))

  ; access values in a matrix
  (define (matrix-ref m r c)
    (if (or (< r 0) (< c 0) (>= r (matrix-rows m)) (>= c (matrix-cols m)))
        (error 'matrix-ref
          (format "(~a, ~a) is not a valid index for a ~a by ~a matrix"
            r c (matrix-rows m) (matrix-cols m)))
        (vector-ref (matrix-data m) (+ (* r (matrix-cols m)) c))))

  ; mutate the matrix
  (define (matrix-set! m r c v)
    (if (or (< r 0) (< c 0) (>= r (matrix-rows m)) (>= c (matrix-cols m)))
        (error 'matrix-set!
          (format "(~a, ~a) is not a valid index for a ~a by ~a matrix"
            r c (matrix-rows m) (matrix-cols m)))
        (vector-set! (matrix-data m) (+ (* r (matrix-cols m)) c) v)))

  ; display the matrix
  (define (draw-matrix m)
    (define $draw-matrix-rows$ 10)
    (define $draw-matrix-cols$ 10)
    (define $draw-matrix-width$ 5)
    (let ^ ([r 0] [c 0])
      (cond
        [(> r $draw-matrix-rows$) (printf "...\n") (void)]
        [(= r (matrix-rows m)) (printf "\n") (void)]
        [(> c $draw-matrix-cols$) (printf "... \n") (^ (+ r 1) 0)]
        [(= c (matrix-cols m)) (printf "\n") (^ (+ r 1) 0)]
        [else
          (printf
            (let ([s (format "~a" (matrix-ref m r c))])
              (if (>= (string-length s) $draw-matrix-width$)
                  (string-append (substring s 0 (- $draw-matrix-width$ 1)) "~~ ")
                  (format
                    (string-append "~"
                      (number->string $draw-matrix-width$) "a ")
                    s))))
          (^ r (+ c 1))])))

  ; generate a matrix using a generating function based on row and column
  (define (matrix-generator rs cs p)
    (let ([m (make-matrix rs cs)])
      (let ^ ([r 0] [c 0])
        (cond
          [(= r (matrix-rows m)) m]
          [(= c (matrix-cols m)) (^ (+ r 1) 0)]
          [else
            (matrix-set! m r c (p r c))
            (^ r (+ c 1))]))))
  )