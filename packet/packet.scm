(module system racket/base
	(require
		(only-in rnrs/base-6 mod)
		racket/contract
		racket/list
		racket/math
		"../library/extension.scm"
		"../library/geometry.scm"
	)
	(provide (contract-out
		(scramble (bytes? . -> . bytes?))
		(checksum ((and/c bytes? longer-then-4? multiple-of-4?) . -> . bytes?))
		(heading->angle (integer? . -> . rational?))
		(angle->heading (rational? . -> . integer?))
		(parse-item (list? . -> . list?))
		(parse-abnormal-effects (integer? . -> . list?))
		(read-point (input-port? . -> . point/3d?))
		(write-point (point/3d? output-port? . -> . void?))
		(read-int16 (boolean? input-port? . -> . integer?))
		(write-int16 (integer? boolean? output-port? . -> . void?))
		(read-int32 (boolean? input-port? . -> . integer?))
		(write-int32 (integer? boolean? output-port? . -> . void?))
		(read-float (input-port? . -> . rational?))
		(write-float (rational? output-port? . -> . void?))
		(read-double (input-port? . -> . rational?))
		(write-double (rational? output-port? . -> . void?))
		(read-ascii (input-port? . -> . string?))
		(write-ascii (string? output-port? . -> . void?))
		(read-utf16 (input-port? . -> . string?))
		(write-utf16 (string? output-port? . -> . void?))
		(get-packet-id (bytes? . -> . byte?))
	))

	(define (scramble data)
		(define (step1 i)
			(when (< i 64)
				(let ((b1 (bytes-ref data i)) (b2 (bytes-ref data (+ i 64))))
					(bytes-set! data (+ i 64) (bitwise-xor b1 b2))
					(step1 (+ i 1))
				)
			)
		)
		(define (step2 i)
			(when (< i 4)
				(let ((b1 (bytes-ref data (+ i 52))) (b2 (bytes-ref data (+ i 13))))
					(bytes-set! data (+ i 13) (bitwise-xor b1 b2))
					(step2 (+ i 1))
				)
			)
		)
		(define (step3 i)
			(when (< i 64)
				(let ((b1 (bytes-ref data (+ i 64))) (b2 (bytes-ref data i)))
					(bytes-set! data i (bitwise-xor b1 b2))
					(step3 (+ i 1))
				)
			)
		)
		(define (step4 i)
			(when (< i 4)
				(let ((b1 (bytes-ref data i)) (b2 (bytes-ref data (+ i 77))))
					(bytes-set! data (+ i 77) b1)
					(bytes-set! data i b2)
					(step4 (+ i 1))
				)
			)
		)
		(begin
			(step1 0)
			(step2 0)
			(step3 0)
			(step4 0)
			data
		)
	)

	(define (checksum data)
		(define (r data sum)
			(if (null? data)
				sum
				(r (drop data 4) (map bitwise-xor sum (take data 4)))
			)
		)
		(list->bytes (r (bytes->list data) (make-list 4 0)))
	)

	; (define (to-multiple num base)
	; 	(let ((mod (modulo num base)))
	; 		(if (> mod 0) (- base mod) 0)
	; 	)
	; )

	(define (read-int size signed? port)
		(integer-bytes->integer (read-bytes size port) signed?)
	)
	(define (write-int n size signed? port)
		(void (write-bytes (integer->integer-bytes n size signed?) port))
	)
	(define (read-int32 signed? port)
		(read-int 4 signed? port)
	)
	(define (write-int32 n signed? port)
		(write-int n 4 signed? port)
	)
	(define (read-int16 signed? port)
		(read-int 2 signed? port)
	)
	(define (write-int16 n signed? port)
		(write-int n 2 signed? port)
	)

	(define (read-float port)
		(floating-point-bytes->real	(read-bytes 4 port))
	)
	(define (write-float n port)
		(void (write-bytes (real->floating-point-bytes n 4) port))
	)
	(define (read-double port)
		(floating-point-bytes->real	(read-bytes 8 port))
	)
	(define (write-double n port)
		(void (write-bytes (real->floating-point-bytes n 8) port))
	)

	(define (read-ascii port)
		(define (r l)
			(let ((b (read-byte port)))
				(if (= b 0) l (r (cons b l)))
			)
		)
		(bytes->string/latin-1 (list->bytes (reverse (r (list)))))
	)
	(define (write-ascii s port)
		(write-bytes (string->bytes/latin-1	s) port)
		(write-byte 0 port)
		(void)
	)
	(define (read-utf16 port)
		(define (r l)
			(let ((b1 (read-byte port)) (b2 (read-byte port)))
				(if (= b1 b2 0) l (r (cons b2 (cons b1 l))))
			)
		)

		(bytes->string/utf-16le (list->bytes (reverse (r (list)))))
	)
	(define (write-utf16 s port)
		(write-bytes (string->bytes/utf-16le s) port)
		(write-bytes (bytes 0 0) port)
		(void)
	)

	(define (read-point port)
		(point/3d
			(read-int32 #t port)
			(read-int32 #t port)
			(read-int32 #t port)
		)
	)
	(define (write-point p port)
		(write-int32 (exact-round (point/3d-x p)) #t port)
		(write-int32 (exact-round (point/3d-y p)) #t port)
		(write-int32 (exact-round (point/3d-z p)) #t port)
		(void)
	)

	(define (heading->angle heading) ; Конвертированный угол соответсвует радианной шкале, если смотреть направление по карте
		(simple-angle (revert-angle (mod (degrees->radians (/ heading 182.044444444)) (* 2 pi))))
	)
	(define (angle->heading angle)
		(exact-round (* (radians->degrees (simple-angle (revert-angle angle))) 182.044444444))
	)

	(define (parse-item item)
		(let ((type1 (cdr (assq 'type1 item))) (type2 (cdr (assq 'type2 item))))
			(filter pair? (append item (list
				(and (= type1 4) (= type2 3) (cons 'quest? #t))
			)))
		)
	)
	(define (parse-abnormal-effects data)
		(list
			(cons 'bleeding? (bitwise-bit-set? data 0))
			(cons 'poisoned? (bitwise-bit-set? data 1))
			(cons 'stunned? (bitwise-bit-set? data 6))
			(cons 'sleeping? (bitwise-bit-set? data 7))
			(cons 'silenced? (bitwise-bit-set? data 8))
			(cons 'rooted? (bitwise-bit-set? data 9))
			; TODO paralyzed?
				; (cons 'hold_1 (bitwise-bit-set? data 10))
				; (cons 'hold_2 (bitwise-bit-set? data 11))
			(cons 'burning? (bitwise-bit-set? data 14))
		)
	)

	(define (longer-then-4? data)
		(> (bytes-length data) 4)
	)
	(define (multiple-of-4? data)
		(= (modulo (bytes-length data) 4) 0)
	)

	(define (get-packet-id buffer)
		(bytes-ref buffer 0)
	)
)
