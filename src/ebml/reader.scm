(define-module (ebml reader)
  #:use-module (rnrs bytevectors)
  #:use-module (rnrs arithmetic bitwise)
  #:use-module (ice-9 binary-ports)
  #:use-module (ice-9 receive))

(define (ebml-read in)
  (cond [(bytevector? in)
         'bytevector]
        [(port? in)
         'port]
        [else
         (error "don't know how to open")]))


(define (parse-elements port)
  (cond [(eof-object? (lookahead-u8 port))]
        [else
         (cons (parse-element port)
               (parse-elements port))]))

(define (parse-element port)
  (define first-byte (get-u8 port))

  ;(receive (cond
  ;          [(not (= 0 (bitwise-and #x80 first-byte)))]))
  port)
