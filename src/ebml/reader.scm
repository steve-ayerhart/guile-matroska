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

(define (encoded-size byte)
  (cond [(not (= 0 (bitwise-and #x80 byte)))
         (values (bitwise-and #x7F byte) 1)]
        [(not (= 0 (bitwise-and #x40 byte)))
         (values (bitwise-and #x3F byte) 2)]
        [(not (= 0 (bitwise-and #x20 byte)))
         (values (bitwise-and #x1F byte) 3)]
        [(not (= 0 (bitwise-and #x10 byte)))
         (values (bitwise-and #x0F byte) 4)]
        [(not (= 0 (bitwise-and #x08 byte)))
         (values (bitwise-and #x07 byte) 5)]
        [(not (= 0 (bitwise-and #x04 byte)))
         (values (bitwise-and #x03 byte) 6)]
        [(not (= 0 (bitwise-and #x02 byte)))
         (values (bitwise-and #x01 byte) 7)]
        [(not (= 0 (bitwise-and #x01 byte)))
         (values (bitwise-and #x00 byte) 8)]
        [else (error 'parse-data-len
                     "not a legal data-len beginning byte: ~s" byte)]))

(define (parse-element port)
  (define first-byte (get-u8 port))

  (receive (header-id-first-byte header-len)
      (encoded-size first-byte)
    (define header-followup-bytes (get-bytevector-n port (- header-len 1)))

    (when (< (bytevector-length header-followup-bytes) (- header-len 1))
      (error 'parse-element
             "port containing complete header id"
             0 port))


    (let* ((other-bits (bytevector->u8-list header-followup-bytes))
          (header-id (+ (bitwise-arithmetic-shift header-id-first-byte
                                          (* (length other-bits) 8))
                        (bytes->uint other-bits 0))))

      (when (= header-id (- (expt 2 (* header-len 7)) 1))
        (error 'parse-element
               "non-reserved header id"
               header-id))

      (parse-data-len port header-id))))

(define (parse-data-len port header-id)
  (define first-byte (get-u8 port))

  (receive (data-len-first-byte data-len-len)
      (encoded-size first-byte)

    (define followup-data-len-bytes (get-bytevector-n port (- data-len-len 1)))

    (when (< (bytevector-length followup-data-len-bytes) (- data-len-len 1))
      (error 'parse-data-len 
             "port containing complete data length"
             0 port header-id))

    (let* ((other-bits (bytevector->u8-list followup-data-len-bytes))
          (data-len (+ (bitwise-arithmetic-shift data-len-first-byte
                                         (* (length other-bits) 8))
                       (bytes->uint other-bits 0))))
      (when (= data-len (- (expt 2 (* data-len-len 7)) 1))
        (error "reserved ata length id: ~s for data length len: ~s"
               data-len data-len-len))

      (let ((data-bytes (get-bytevector-n port data-len)))
        (when (< (bytevector-length data-bytes) (- data-len 1))
          (error 'parse-data-len
                 "port containing complete data"
                 0 port header-id))

        (list header-id data-bytes)))))

(define (bytes->uint l accum)
  (if (null? l)
      accum
      (bytes->uint (cdr l)
                   (+ (car l) (bitwise-arithmetic-shift accum 8)))))
