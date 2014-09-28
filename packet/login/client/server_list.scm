(module packet racket/base
	(provide login-client-packet/server-list)
	(require "../../packet.scm")

	(define (login-client-packet/server-list struct)
		(let ((s (open-output-bytes)))
			(begin
				(write-byte #x05 s)
				(write-bytes (cdr (assoc 'login-key struct)) s)
				(write-byte #x04 s)
				(write-bytes (make-bytes 6) s)
				(write-bytes (checksum (get-output-bytes s)) s)
				(write-bytes (make-bytes 4) s)
				(get-output-bytes s)
			)
		)
	)
)