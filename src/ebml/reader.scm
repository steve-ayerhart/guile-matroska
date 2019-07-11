(define-module (ebml reader)
  #:use-module (rnrs bytevectors)
  #:use-module (rnrs arithmetic bitwise)
  #:use-module (ice-9 binary-ports)
  #:use-module (ice-9 receive)
  #:use-module (sxml simple)
  #:use-module (sxml xpath)
  #:use-module (sxml match)

  #:export (parse-element-id
            read-matroska-file))

(define (get-element-id port)
  (define first-byte (lookahead-u8 port))
  (define id-length
    (cond ((logtest #x80 first-byte) 1)
          ((logtest #x40 first-byte) 2)
          ((logtest #x20 first-byte) 3)
          ((logtest #x10 first-byte) 4)
          (else
           (error 'parse-element-id
                  "not an Element ID"))))
  (bytevector-uint-ref
   (get-bytevector-n port id-length) 0 (endianness big) id-length))

(define (get-element-data-length port)
  (define first-byte (lookahead-u8 port))
  (define data-length
    (cond ((logtest #x80 first-byte) 1)
          ((logtest #x40 first-byte) 2)
          ((logtest #x20 first-byte) 3)
          ((logtest #x10 first-byte) 4)
          ((logtest #x08 first-byte) 5)
          ((logtest #x04 first-byte) 6)
          ((logtest #x02 first-byte) 7)
          ((logtest #x01 first-byte) 8)
          (else
           (error 'parse-element-length
                  "not a Element length"))))

  (bit-extract
   (bytevector-uint-ref
    (get-bytevector-n port data-length) 0 (endianness big) data-length)
   0
   (- (* 8 data-length) 1)))

(define (parse-element-data bv type length)
  (case type
    ((uinteger)
     (bytevector-uint-ref bv 0 (endianness big) length))
    ((float)
     #f)
    ((binary)
     #f)
    ((utf-8)
     #f)
    ((string)
     (utf8->string bv))
    ((date)
     #f)))

(define (parse-element port seed)
  (display (format #f "~a\n" seed))
  (if (eof-object? port)
      (reverse seed)
      (let ((element-id (get-element-id port))
            (element-data-length (get-element-data-length port)))
        (receive (element-name element-type)
            (lookup-element element-id)
          (if (equal? element-type 'master)
              (parse-element port (append (list element-name) seed))
              (let ((element-data (parse-element-data
                                   (get-bytevector-n port element-data-length)
                                   element-type
                                   element-data-length)))
                (parse-element port (cons (list element-name element-data) seed))))))))

(define matroska-elements
  (call-with-input-file "matroska.xml"
    (λ (port)
      (xml->sxml port #:trim-whitespace? #t))))

(define (lookup-element id)
  (define element (car ((sxpath `(// (element (@ id (equal? ,(format #f "0x~:@(~x~)" id)))))) matroska-elements)))
  (sxml-match element
   [(element (@ (type ,type) (name ,name) (multiple ,multiple) (minver ,minver)
                (mandatory ,mandatory) (level ,level) (id ,id))
      ,doc)
    (values (string->symbol name)
            (string->symbol type))]
   [(element (@ (type ,type) (name ,name) (minver ,minver)
                (mandatory ,mandatory) (level ,level) (id ,id) (default ,default))
      ,doc)
    (values (string->symbol name)
            (string->symbol type))]))

(define (read-matroska-file filename)
  (call-with-input-file filename get-bytevector-all))
