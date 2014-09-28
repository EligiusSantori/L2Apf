(module packet racket/base
	(provide game-client-packet/validate-auth)
	(require srfi/1 "../../packet.scm")

	(define (game-client-packet/validate-auth struct)
		(define login (cdr (assoc 'login struct)))
		(define login-key (bytes->list (cdr (assoc 'login-key struct))))
		(define game-key (bytes->list (cdr (assoc 'game-key struct))))
	
		(let ((s (open-output-bytes)))
			(begin
				(write-byte #x08 s)
				(write-utf16 login s)
				(write-bytes (list->bytes (drop game-key 4)) s)
				(write-bytes (list->bytes (take login-key 4)) s)
				(write-bytes (list->bytes (take game-key 4)) s)
				(write-bytes (list->bytes (drop login-key 4)) s)
				(write-int32 1 #f s)
				(get-output-bytes s)
			)
		)
	)
)