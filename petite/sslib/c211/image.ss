; Provides the C211 image library

#|
Constructors:
  (color r g b)
    - create a pixel
  (make-image rs cs [i])
    - create an rs x cs image
    - if i is a color it's the default
    - if it's a procedure, set each to (i r c rs cs)
  (image-map i p)
    - map the procedure p (color -> color) over the image i

Predicates:
  (color? c)
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
  (color-ref c b)
    - with b as 'red 'green or 'blue, access that band of a pixel

Mutators:
  (image-set! i r c v)
    - set the pixel in image i at r,c to the color v

Other:
  (read-image [file])
    - if file is given, read that image; otherwise display a dialog (uses Java)
  (write-image i [file])
    - write the image i to a file; if not specified display a dialog (uses Java)
  (draw-image i)
    - display the image to the user (uses Java)
|#

(library
 (c211 image)
 (export
   color make-image image-map
   color? image?
   image-rows image-cols image-ref color-ref
   image-set!
   read-image write-image draw-image
   image-data)

  (import (except (chezscheme) lambda define))

  (import (wombat define))
  (import (wombat java))

  (import (c211 matrix))
  (import (c211 base64))

  ; create the datatype (image data is stored as a matrix of colors)
  (define :color (make-record-type "color" '(r g b)))
  (define :image (make-record-type "image" '(data)))

  ; make colors
  (define (color r g b)
    (if (or (not (integer? r)) (< r 0) (> r 255)
            (not (integer? g)) (< g 0) (> g 255)
            (not (integer? b)) (< b 0) (> b 255))
        (error 'color
          (format "#[color ~a ~a ~a] is not a valid color, range is 0-255"
            r g b))
        ((record-constructor :color) r g b)))

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
                 [(procedure? v) (image-set! i r c (v r c rs cs))]
                 [else (error 'make-image
                         (format "~a is neither a color nor a procedure" v))])
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

  ; check if something is a color
  (define color? (record-predicate :color))

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
    (let ([cr (color-ref c 'red)]
          [cg (color-ref c 'green)]
          [cb (color-ref c 'blue)])
      (cond
        [(memq b '(0 red))
         (color v cg cb)]
        [(memq b '(1 green))
         (color cr v cb)]
        [(memq b '(2 blue))
         (color cr cg v)])))

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
                                 (ref (+ i 0))
                                 (ref (+ i 1))
                                 (ref (+ i 2))))
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
           (set (+ i 0) (image-ref img r c 'red))
           (set (+ i 1) (image-ref img r c 'green))
           (set (+ i 2) (image-ref img r c 'blue))
           (set (+ i 3) 255) ; alpha channel
           (^ (+ i 4) r (+ c 1))]))))

  ; read an image from a file
  (define (read-image . args)
    (let ([response
            (cond
              [(null? args) (call-to-java read-image)]
              [(null? (cdr args)) (call-to-java read-image (car args))]
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
           (image-cols img) (image-rows img) base64 (car args))]
        [else
         (error 'read-image "incorrect argument count")])
      (void)))

  ; display the image in a Java window
  (define (draw-image img)
    (call-to-java draw-image
      (image-cols img) (image-rows img) (image->base64 img))
    (void))

  ; custom writer for images (show size, hide data)
  (record-writer :image
    (lambda (r p wr)
      (display "#[image " p)
      (wr (image-rows r) p)
      (display " " p)
      (wr (image-cols r) p)
      (display "]" p)))
  )