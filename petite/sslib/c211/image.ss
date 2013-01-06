; Provides the C211 image library

; License: source-license.txt
; If this code is used independently, copy the license here.

#|
Constructors:
  (color r g b) {from (c211 color)}
    - create a pixel
  (make-image rs cs [i])
    - create an rs x cs image
    - if i is a color it's the default
    - if it's a procedure, set each to (i r c rs cs)
  (image-map i p)
    - map the procedure p (color -> color) over the image i

Predicates:
  (color? c) {from (c211 color)}
    - tests if c is a color
  (image? i)
    - tests if i is an image

Accessors:
  (image-rows i)
    - how many rows are in an image
  (image-cols i)
    - how many columns are in an image
  (image-ref i r c [b])
    - get the color at r,c of i; if b is specified pass that color to color-ref
  (color-ref c b) {from (c211 color)}
    - with b as 'red 'green or 'blue, access that band of a pixel

Mutators:
  (image-set! i r c v)
    - set the pixel in image i at r,c to the color v
  (color-set! c b v) {from (c211 color)}
    - set the value of band b in color c to value v

Other:
  (read-image [file])
    - if file is given, read that image; otherwise display a dialog (uses Java)
  (write-image i [file])
    - write the image i to a file; if not specified display a dialog (uses Java)
  (draw-image i)
    - display the image to the user (uses Java)
  (color-equal? c1 c2) {from (c211 color)}
    - tests if two colors are equal
  (image-equal? i1 i2)
    - tests if two images are equal
  black darkgray gray lightgray white red green blue yellow cyan magenta orange pink {from (c211 color)}
    - predefined colors
|#

(library
 (c211 image)
 (export
   make-image image-map
   image?
   image-rows image-cols image-ref
   image-set!
   read-image write-image draw-image
   image-equal?
   image->list list->image
   base64->image image->base64)

  (import (except (chezscheme) lambda define))

  (import (wombat base64))
  (import (wombat define))
  (import (wombat java))

  (import (c211 color))
  (import (c211 matrix))

  (export (import (c211 color)))

  ; create the datatype (image data is stored as a matrix of colors)
  (define :image (make-record-type "image" '(data)))

  ; make an image
  (define make-image
    (case-lambda
      [(rs cs)
       (make-image rs cs (color 0 0 0))]
      [(rs cs v)
       (let ([i ((record-constructor :image) (make-matrix rs cs))])
         (let ^ ([r 0] [c 0])
           (cond
             [(= r (image-rows i)) i]
             [(= c (image-cols i)) (^ (+ r 1) 0)]
             [else
               (cond
                 [(color? v) (image-set! i r c v)]
                 [(procedure? v) (image-set! i r c (v r c))]
                 [else (error 'make-image
                         (format "~a is neither a color nor a procedure of 2 arguments" v))])
               (^ r (+ c 1))])))]))

  ; map an image to another image
  (define (image-map p i)
    (!check-image 'image-map i)
    (!check-procedure 'image-map p)
    (let ([i* (make-image (image-rows i) (image-cols i))])
      (let ^ ([r 0] [c 0])
        (cond
          [(= r (image-rows i)) i*]
          [(= c (image-cols i)) (^ (+ r 1) 0)]
          [else
            (image-set! i* r c (p (image-ref i r c)))
            (^ r (+ c 1))]))))

  ; check if something is an image
  (define image? (record-predicate :image))

  ; get the size of the image
  (define image-data (record-accessor :image 0))
  (define (image-rows i) (matrix-rows (image-data i)))
  (define (image-cols i) (matrix-cols (image-data i)))

  ; verifiers
  (define (make-check pred? name)
    (lambda (proc v)
      (when (not (pred? v))
        (error proc (format "~a is not the correct type, expected ~a" v name)))))

  (define !check-integer (make-check integer? "integer"))
  (define !check-procedure (make-check procedure? "procedure"))
  (define !check-color (make-check color? "color"))
  (define !check-image (make-check image? "image"))

  (define !check-band
    (make-check
      (lambda (b) (or (and (integer? b) (<= 0 b 2))
                      (member b '(red green blue))))
      "a band number, 'red, 'green, or 'blue"))

  (define (!check-bounds proc i r c)
    (!check-integer proc r)
    (!check-integer proc c)
    (when (or (< r 0) (>= r (image-rows i))
              (< c 0) (>= c (image-cols i)))
      (error proc
        (format "(~a, ~a) is not a valid index for ~a" r c i))))

  ; get a pixel from an image
  (define image-ref
    (case-lambda
      [(i r c)
       (!check-bounds 'image-ref i r c)
       (matrix-ref (image-data i) r c)]
      [(i r c b)
       (!check-bounds 'image-ref i r c)
       (!check-band 'image-ref b)
       (color-ref (matrix-ref (image-data i) r c) b)]))

  ; change a pixel in an image
  (define image-set!
    (case-lambda
      [(i r c v)
       (!check-image 'image-set! i)
       (!check-bounds 'image-set! i r c)
       (matrix-set! (image-data i) r c v)]
      [(i r c b v)
       (!check-image 'image-set! i)
       (!check-bounds 'image-set! i r c)
       (!check-band 'image-set! b)
       (color-set! (matrix-ref (image-data i) r c) b v)]
      [others
        (error 'image-set! "invalid argument count")]))

  ; create an image from base64 data
  (define (base64->image width height data)
    (let ([img (make-image height width)]
          [data (base64->string data)])
      (define (ref i) (char->integer (string-ref data i)))
      (let ^ ([i 0] [r 0] [c 0])
        (cond
          [(= r height) img]
          [(= c width) (^ i (+ r 1) 0)]
          [else
           (image-set! img r c (color
                                 (ref (+ i 2))
                                 (ref (+ i 1))
                                 (ref (+ i 0))))
           (^ (+ i 4) r (+ c 1))]))))

  ; create a base64 encoding from image data
  (define (image->base64 img)
    (let ([data (make-string (* 4 (image-rows img) (image-cols img)))])
      (define (set i v) (string-set! data i (integer->char v)))
      (let ^ ([i 0] [r 0] [c 0])
        (cond
          [(= r (image-rows img)) (string->base64 data)]
          [(= c (image-cols img)) (^ i (+ r 1) 0)]
          [else
           (set (+ i 2) (image-ref img r c 'red))
           (set (+ i 1) (image-ref img r c 'green))
           (set (+ i 0) (image-ref img r c 'blue))
           (set (+ i 3) 255) ; alpha channel
           (^ (+ i 4) r (+ c 1))]))))

  ; add " around a string
  (define (restring str)
    (format "~s" str))

  ; read an image from a file
  (define (read-image . args)
    (let ([response
            (cond
              [(null? args) (call-to-java read-image)]
              [(null? (cdr args)) (call-to-java read-image (restring (cd)) (restring (car args)))]
              [else (error 'read-image "incorrect argument count")])])
      (if (= (length response) 3)
          (apply base64->image response)
          (error 'read-image "invalid data returned"))))

  ; write an image to a file
  (define (write-image img . args)
    (let ([base64 (image->base64 img)])
      (cond
        [(null? args)
         (call-to-java write-image
           (image-cols img) (image-rows img) base64)]
        [(null? (cdr args))
         (call-to-java write-image
           (restring (cd)) (image-cols img) (image-rows img) base64 (restring (car args)))]
        [else
         (error 'read-image "incorrect argument count")])
      (void)))

  ; display the image in a Java window
  (define (draw-image img)
    (if (and (positive? (image-rows img))
             (positive? (image-cols img)))
        (begin
          (call-to-java draw-image
            (image-cols img) (image-rows img) (image->base64 img))
          (void))
        (error 'draw-image (format "~s cannot be drawn (invalid size)" img))))

  ; tests if images are equal
  (define image-equal?
    (lambda (img1 img2)
      (let ([rows (image-rows img1)]
            [cols (image-cols img1)])
        (and (= rows (image-rows img2))
             (= cols (image-cols img2))
             (let loop ([r 0])
               (or (= r rows)
                   (and (let loop ([c 0])
                          (or (= c cols)
                              (and (color-equal? (image-ref img1 r c)
                                     (image-ref img2 r c))
                                   (loop (+ c 1)))))
                        (loop (+ r 1)))))))))

  ; convert between images and lists
  (define (image->list image)
    (let loop ([r (sub1 (image-rows image))] [acc '()])
      (if (negative? r)
          acc
          (loop (- r 1)
            (let loop ([c (sub1 (image-cols image))] [acc acc])
              (if (negative? c)
                  acc
                  (loop (- c 1) (cons (image-ref image r c) acc))))))))
  (define (list->image num-cols ls)
    (make-image (quotient (length ls) num-cols) num-cols
      (lambda (r c)
        (list-ref ls (+ (* r num-cols) c)))))

  ; custom writer for images (show size, hide data)
  (record-writer :image
    (lambda (r p wr)
      (display "#[image " p)
      (wr (image-rows r) p)
      (display " " p)
      (wr (image-cols r) p)
      (display "]" p)))
  )