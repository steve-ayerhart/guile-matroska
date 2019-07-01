(define-module (ebml reader)
  #:use-module (rnrs bytevectors)
  #:use-module (rnrs arithmetic bitwise)
  #:use-module (ice-9 binary-ports)
  #:use-module (ice-9 receive))

(define (read-element-id port)
  (define first-byte (lookahead-u8 port))

  (cond ((not (= 0 (bitwise-and #x80 first-byte)))
         (get-u8 port))
        ((not (= 0 (bitwise-and #x40 first-byte)))
         (bytevector-uint-ref (get-bytevector-n port 2) 0 (endianness big) 2))
        ((not (= 0 (bitwise-and #x20 first-byte)))
         (let ((bv (get-bytevector-n port 3)))
           (+ (* (bytevector-u8-ref bv 0) (expt 2 16))
              (bytevector-uint-ref bv 1 (endianness big) 2))))
        ((not (= 0 (bitwise-and #x10 first-byte)))
         (bytevector-uint-ref (get-bytevector-n port 4) 0 (endianness big) 4))
        (else
         (error 'read-element-id
                "not an Element ID"))))

(define (read-element-length port)
  (define first-byte (lookahead-u8 port))

  (cond ((not (= 0 (bitwise-and #x80 first-byte)))
         (bitwise-xor (get-u8 port) #x80))

        ((not (= 0 (bitwise-and #x40 first-byte)))
         (let ((bv (get-bytevector-n port 2)))
           (bytevector-u8-set! bv 0 (bitwise-xor #x40 first-byte))
           (bytevector-uint-ref bv 0 (endianness big) 2)))

        ((not (= 0 (bitwise-and #x20 first-byte)))
         (let ((bv (get-bytevector-n port 3)))
           (+ (* (bitwise-xor #x20 (bytevector-u8-ref bv 0)) (expt 2 16))
              (bytevector-uint-ref bv 1 (endianness big) 2))))

        ((not (= 0 (bitwise-and #x10 first-byte)))
         (let ((bv (get-bytevector-n port 4)))
           (bytevector-u8-set! bv 0 (bitwise-xor #x10 first-byte))
           (bytevector-uint-ref bv 0 (endianness big) 4)))

        ((not (= 0 (bitwise-and #x08 first-byte)))
         (let ((bv (get-bytevector-n port 5)))
           (+ (* (bitwise-xor #x08 (bytevector-u8-ref bv 0)) (expt 2 32))
              (bytevector-uint-ref bv 1 (endianness big) 4))))

        ((not (= 0 (bitwise-and #x04 first-byte)))
         (let ((bv (get-bytevector-n port 6)))
           (+ (* (bitwise-xor #x04 (bytevector-uint-ref bv 0 (endianness big) 2)) (expt 2 32))
              (bytevector-uint-ref bv 2 (endianness big) 4))))

        ((not (= 0 (bitwise-and #x02 first-byte)))
         (let ((bv (get-bytevector-n port 7)))
           (+ (* (bitwise-xor #x02 (bytevector-u8-ref bv 0)) (expt 2 48))
              (* (bytevector-uint-ref bv 1 (endianness big) 2) (expt 2 32))
              (bytevector-uint-ref bv 3 (endianness big) 4))))

        ((not (= 0 (bitwise-and #x01 first-byte)))
         (let ((bv (get-bytevector-n port 8)))
           (bytevector-u8-set! bv 0 (bitwise-xor #x01 first-byte))
           (bytevector-uint-ref bv 0 (endianness big) 8)))

        (else
         (error 'read-element-length
                "not an Element length"))))

(define (read-element port)
  (let ((element-id (read-element-id port))
        (element-length (read-element-length port)))
    (format #t "length: ~a\n" element-length)
    (cons element-id (get-bytevector-n port element-length))))
