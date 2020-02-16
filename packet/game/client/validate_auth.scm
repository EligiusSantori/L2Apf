(module system racket/base
	(provide game-client-packet/validate-auth)
	(require srfi/1 "../../packet.scm")

	(define (game-client-packet/validate-auth struct)
		(let ((s (open-output-bytes)) (lk (or (cdr (assoc 'login-key struct)) (make-bytes 8))) (gk (cdr (assoc 'game-key struct))))
			(begin
				(write-byte #x08 s)
				(write-utf16 (cdr (assoc 'login struct)) s)
				(write-bytes (subbytes gk 4 8) s)
				(write-bytes (subbytes lk 0 4) s)
				(write-bytes (subbytes gk 0 4) s)
				(write-bytes (subbytes lk 4 8) s)
				(write-int32 1 #f s)
				(get-output-bytes s)
			)
		)
	)
)
