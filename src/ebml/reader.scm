(define-module (ebml reader)
  #:use-module (rnrs bytevectors))

(define (ebml-read in)
  (cond [(bytevector? in)
         'bytevector]
        [(port? in)
         'port]
        [else
         (error "don't know how to open")]))
