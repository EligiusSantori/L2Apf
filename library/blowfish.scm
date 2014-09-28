; http://www.openssl.org/docs/crypto/blowfish.html

(module blowfish racket/base
	(require (rename-in racket/contract (-> ->/c)) openssl/libcrypto ffi/unsafe)
	
	(define BF_BLOCK 8)
	(define BF_ROUNDS 16)
	(define BF_LONG _uint32)
	(define BF_LONG-pointer (_cpointer BF_LONG))
	
	(define-cstruct _BF_KEY (
		(P (_array BF_LONG (+ BF_ROUNDS 2)))
		(S (_array BF_LONG (* 4 256)))
	))
	
	(define BF_set_key
		(get-ffi-obj 'BF_set_key libcrypto (_fun _BF_KEY-pointer _int _bytes -> _void))
	)
	(define BF_encrypt 
		(get-ffi-obj 'BF_encrypt libcrypto (_fun BF_LONG-pointer _BF_KEY-pointer -> _void))
	)
	(define BF_decrypt 
		(get-ffi-obj 'BF_decrypt libcrypto (_fun BF_LONG-pointer _BF_KEY-pointer -> _void))
	)
	
	(define (blowfish-create-key key)
		(let ((bf-key (ptr-ref (malloc _BF_KEY) _BF_KEY)) (key (bytes-append key (bytes #x0))))
			(begin
				(BF_set_key bf-key (bytes-length key) key)
				bf-key
			)
		)
	)
	
	(define (blowfish-process data key fn)
		(define (r bf-data bf-key i l)
			(if (< i l)
				(begin
					(fn (ptr-add bf-data i) bf-key)
					(r bf-data bf-key (+ i BF_BLOCK) l)
				)
				(void)
			)
		)
		(let* ((mod (modulo (bytes-length data) 8)) (data (if (> mod 0) (bytes-append data (make-bytes (- 8 mod))) data)))
			(let ((bf-key (if (bytes? key) (blowfish-create-key key) key)) (bf-data (malloc (bytes-length data) data)))
				(begin
					(cpointer-push-tag! bf-data BF_LONG)
					(r bf-data bf-key 0 (bytes-length data))
					(make-sized-byte-string bf-data (bytes-length data))
				)
			)
		)
	)
	
	(define (blowfish-encrypt data key)
		(blowfish-process data key BF_encrypt)
	)
	
	(define (blowfish-decrypt data key)
		(blowfish-process data key BF_decrypt)
	)
	
	(define (make-blowfish-crypter key)
		(define blowfish-key (blowfish-create-key key))
		(lambda (data encrypt?)
			(if encrypt?
				(blowfish-encrypt data blowfish-key)
				(blowfish-decrypt data blowfish-key)
			)
		)
	)
	
	(provide (contract-out
		(make-blowfish-crypter (bytes? . ->/c . procedure?))
		;(blowfish-create-key (bytes? . ->/c . BF_KEY?))
		;(blowfish-encrypt (bytes? (or/c bytes? BF_KEY?) . ->/c . bytes?))
		;(blowfish-decrypt (bytes? (or/c bytes? BF_KEY?) . ->/c . bytes?))
	))
)