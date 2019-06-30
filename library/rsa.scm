; https://www.openssl.org/docs/crypto/rsa.html

(module rsa racket/base
	(require (rename-in racket/contract (-> ->/c)) openssl/libcrypto ffi/unsafe)
	
	(define RSA_PKCS1_PADDING 1)
	(define RSA_SSLV23_PADDING 2)
	(define RSA_NO_PADDING 3)
	(define RSA_PKCS1_OAEP_PADDING 4)
	(define RSA_X931_PADDING 5)
	
	(define-cstruct _BIGNUM (
		(d _pointer)
		(top _int32)
		(dmax _int32)
		(neg _int32)
		(flags _int32)
	))
	(define-cstruct _CRYPTO_EX_DATA (
		(sk _pointer)
		(dummy _int32)
	))
	(define-cstruct _RSA (
		(pad _int32)
		(version _long)
		(meth _pointer)

		(engine _pointer)
		(n _BIGNUM-pointer)
		(e _BIGNUM-pointer)
		(d _BIGNUM-pointer)
		(p _BIGNUM-pointer)
		(q _BIGNUM-pointer)
		(dmp1 _BIGNUM-pointer)
		(dmq1 _BIGNUM-pointer)
		(iqmp _BIGNUM-pointer)

		(ex_data _CRYPTO_EX_DATA)
		(references _int32)
		(flags _int32)
		
		(_method_mod_n _pointer)
		(_method_mod_p _pointer)
		(_method_mod_q _pointer)
		
		(bignum_data _bytes)
		(blinding _pointer)
		(mt_blinding _pointer)
	))
	
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
	(define RSA_public_encrypt 
		(get-ffi-obj 'RSA_public_encrypt libcrypto (_fun
			(flen : _int32) (from : _bytes) (to : _pointer = (malloc (RSA_size rsa))) (rsa : _RSA-pointer) (padding : _int32) ->
			(r : _int32) ->
			(values (if (= r -1) #f (make-sized-byte-string to r))))
		)
	)
	
	(define RSA_private_decrypt 
		(get-ffi-obj 'RSA_private_decrypt libcrypto (_fun
			(flen : _int32) (from : _bytes) (to : _pointer = (malloc (RSA_size rsa))) (rsa : _RSA-pointer) (padding : _int32) ->
			(r : _int32) ->
			(values (if (= r -1) #f (make-sized-byte-string to r))))
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
			(if (not (or (ptr-equal? rsa-key #f) (ptr-equal? n #f) (ptr-equal? e #f)))
				(begin
					(set-RSA-n! rsa-key n)
					(set-RSA-e! rsa-key e)
					;(set-RSA-d! rsa-key e)
					rsa-key
				)
				#f
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
	
	(provide (contract-out
		(rsa-encrypt (bytes? bytes? . ->/c . (or/c false/c bytes?)))
		;(rsa-decrypt (bytes? bytes? . ->/c . (or/c false/c bytes?)))
	))
)