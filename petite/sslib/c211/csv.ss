(library
  (c211 csv)

  (export
    read-csv
    write-csv)

  (import (except (chezscheme) lambda define))
  (import (wombat define))

  ; try catch
  (define (try thunk default)
    (call/cc
      (lambda (return)
        (with-exception-handler
          (lambda (x)
            (return default))
          thunk))))

  ; read from csv into a list of lists
  (define read-csv
    (lambda (filename)
      (with-input-from-file filename
        (lambda ()
          (let loop ()
            (let ([line (read-line (current-input-port))])
              (if (eof-object? line)
                  '()
                  (cons
                    (map
                      (lambda (each)
                        (try
                          (lambda ()
                            (with-input-from-string each
                              (lambda ()
                                (read))))
                          each))
                      (split line #\,))
                    (loop)))))))))

  ; write a list of lists as a csv file
  (define write-csv
    (lambda (lol filename)
      (define print display)
      (with-output-to-file filename
        (lambda ()
          (let loop ([lol lol])
            (unless (null? lol)
              (let loop ([line (car lol)])
                (cond
                  [(null? line) (newline)]
                  [(null? (cdr line)) (print (car line)) (newline)]
                  [else (print (car line)) (print ",") (loop (cdr line))]))
              (loop (cdr lol))))))))

  ;; read chars from ip until either eof or the next character
  ;; satisfies the predicate. If the port is initially at eof,
  ;; then return the eof character. Otherwise, return the string
  ;; formed from character in op up to, but not including, eof
  ;; or the break character.
  (define read-until
    (lambda (char-pred? ip)
      (define chug
        (lambda ()
          (let ([c (peek-char ip)])
            (cond
              [(or (eof-object? c) (char-pred? c)) '()]
              [else (read-char ip) (cons c (chug))]))))
      (let ([c (peek-char ip)])
        (if (eof-object? c)
            c
            (list->string (chug))))))

  ; read a line from the current-input-port
  (define read-line
    (lambda (ip)
      (let ([line (read-until (lambda (c) (char=? c #\newline)) ip)])
        (read-char ip)
        line)))

  ; split a string based on a given seperator
  (define split
    (lambda (s separator)
      (let loop ([start 0] [cursor 0])
        (if (= cursor (string-length s))
            (list (substring s start cursor))
            (if (char=? separator (string-ref s cursor))
                (cons (substring s start cursor)
                  (loop (+ cursor 1) (+ cursor 1)))
              (loop start (+ cursor 1))))))))