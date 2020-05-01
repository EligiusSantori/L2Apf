(module library racket/base ; https://www.openssl.org/docs/crypto/rsa.html
	(require (rename-in racket/contract (-> ->/c)) openssl/libcrypto ffi/unsafe)
	(provide (contract-out
		(rsa-encrypt (bytes? bytes? . ->/c . (or/c false/c bytes?)))
		;(rsa-decrypt (bytes? bytes? . ->/c . (or/c false/c bytes?)))
	))


	(define RSA_PKCS1_PADDING 1)
	(define RSA_SSLV23_PADDING 2)
	(define RSA_NO_PADDING 3)
	(define RSA_PKCS1_OAEP_PADDING 4)
	(define RSA_X931_PADDING 5)

	(define _RSA-pointer _pointer)
	(define _BIGNUM-pointer _pointer)

	(define ERR_peek_error
		(get-ffi-obj 'ERR_peek_error libcrypto (_fun -> _ulong))
	)
	(define ERR_get_error
		(get-ffi-obj 'ERR_get_error libcrypto (_fun -> _ulong))
	)
	(define ERR_error_string
		(get-ffi-obj 'ERR_error_string libcrypto (_fun _ulong _pointer -> _bytes))
	)

	(define BN_num_bits
		(get-ffi-obj 'BN_num_bits libcrypto (_fun _BIGNUM-pointer -> _int32))
	)
	(define (BN_num_bytes a)
		(quotient (+ (BN_num_bits a) 7) 8)
	)
	(define BN_bn2bin
		(get-ffi-obj 'BN_bn2bin libcrypto (_fun
			(a : _BIGNUM-pointer) (to : _pointer = (malloc (BN_num_bytes a))) ->
			(r : _int32) ->
			(values (make-sized-byte-string to r)))
		)
	)
	(define BN_bin2bn
		(get-ffi-obj 'BN_bin2bn libcrypto (_fun _bytes _int32 _pointer -> _BIGNUM-pointer))
	)

	(define RSA_new
		(get-ffi-obj 'RSA_new libcrypto (_fun -> _RSA-pointer))
	)
	(define RSA_free
		(get-ffi-obj 'RSA_free libcrypto (_fun _RSA-pointer -> _void))
	)
	(define RSA_size
		(get-ffi-obj 'RSA_size libcrypto (_fun _RSA-pointer -> _int32))
	)
	(define RSA_set0_key
		(get-ffi-obj 'RSA_set0_key libcrypto (_fun
			(r : _RSA-pointer) (n : _BIGNUM-pointer) (e : _BIGNUM-pointer) (d : _BIGNUM-pointer) ->
			(v : _int32) ->
			(values (= v 1))
		))
	)
	(define RSA_public_encrypt
		(get-ffi-obj 'RSA_public_encrypt libcrypto (_fun
			(flen : _int32) (from : _bytes) (to : _pointer = (malloc (RSA_size rsa))) (rsa : _RSA-pointer) (padding : _int32) ->
			(v : _int32) ->
			(values (if (= v -1) #f (make-sized-byte-string to v))))
		)
	)
	(define RSA_private_decrypt
		(get-ffi-obj 'RSA_private_decrypt libcrypto (_fun
			(flen : _int32) (from : _bytes) (to : _pointer = (malloc (RSA_size rsa))) (rsa : _RSA-pointer) (padding : _int32) ->
			(v : _int32) ->
			(values (if (= v -1) #f (make-sized-byte-string to v))))
		)
	)

	(define (rsa-print-errors)
		(let loop ()
			(when (> (ERR_peek_error) 0)
				(display (ERR_error_string (ERR_get_error) #f))
				(newline)
				(loop)
			)
		)
	)

	(define (rsa-create-key key)
		(let ((rsa-key (RSA_new)) (n (BN_bin2bn key (bytes-length key) #f)) (e (BN_bin2bn (bytes #x1 #x0 #x1) 3 #f)))
			(and
				(not (or (ptr-equal? rsa-key #f) (ptr-equal? n #f) (ptr-equal? e #f)))
				(RSA_set0_key rsa-key n e #f)
				rsa-key
			)
		)
	)

	(define (rsa-encrypt data key)
		(let ((rsa-key (rsa-create-key key)))
			(if (not (ptr-equal? rsa-key #f))
				(let ((fl (bytes-length data)) (tl (RSA_size rsa-key)))
					(let ((r (RSA_public_encrypt tl (if (> tl fl) (bytes-append (make-bytes (- tl fl)) data) data) rsa-key RSA_NO_PADDING)))
						(begin
							;(when (not r) (rsa-print-errors))
							(RSA_free rsa-key)
							r
						)
					)
				)
				#f
			)
		)
	)

	;(define (rsa-decrypt data key)
	;	RSA_private_decrypt
	;)
)
