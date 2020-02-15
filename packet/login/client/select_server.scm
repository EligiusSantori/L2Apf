(module system racket/base
	(provide login-client-packet/select-server)
	(require "../../packet.scm")

	(define (login-client-packet/select-server struct)
		(let ((s (open-output-bytes)))
			(begin
				(write-byte #x02 s)
				(write-bytes (cdr (assoc 'login-key struct)) s)
				(write-byte (cdr (assoc 'server-id struct)) s)
				(write-bytes (make-bytes 6) s)
				(write-bytes (checksum (get-output-bytes s)) s)
				(write-bytes (make-bytes 4) s)
				(get-output-bytes s)
			)
		)
	)
)