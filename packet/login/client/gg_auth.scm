(module packet racket/base
	(provide login-client-packet/gg-auth)
	(require "../../packet.scm")

	(define (login-client-packet/gg-auth struct)
		(let ((s (open-output-bytes)))
			(begin
				(write-byte #x07 s)
				(write-int32 (cdr (assoc 'session-id struct)) #f s)
				(write-bytes (bytes
					#x23 #x92 #x90 #x4d
					#x18 #x30 #xb5 #x7c
					#x96 #x61 #x41 #x47
					#x05 #x07 #x96 #xfb
					#x00 #x00 #x00
				) s)
				(write-bytes (checksum (get-output-bytes s)) s)
				(write-bytes (make-bytes 4) s)
				(get-output-bytes s)
			)
		)
	)
)