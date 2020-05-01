(module system racket/base
	(provide login-client-packet/login-auth)
	(require "../../../library/rsa.scm")
	(require "../../packet.scm")

	(define (login-client-packet/login-auth struct)
		(let ((s (open-output-bytes)) (rsa-key (cdr (assoc 'rsa-key struct))))
			(if rsa-key
				(begin
					(write-bytes (make-bytes 128))
					(file-position s (- 128 16 14))
					(write-ascii (cdr (assoc 'login struct)) s)
					(file-position s (- 128 16))
					(write-ascii (cdr (assoc 'password struct)) s)

					(let ((buffer (get-output-bytes s #t)))
						(write-byte #x00 s)
						(write-bytes (rsa-encrypt buffer rsa-key) s)
						(write-int32 (cdr (assoc 'session-id struct)) #f s)
						(write-bytes (bytes
							#x23 #x92 #x90 #x4d
							#x18 #x30 #xb5 #x7c
							#x96 #x61 #x41 #x47
							#x05 #x07 #x96 #xfb

							#x08
							#x00 #x00 #x00 #x00
							#x00 #x00 #x00 #x00

							#x00 #x00
						) s)
					)
				)
				(begin
					(write-byte #x00 s)
					(write-ascii (cdr (assoc 'login struct)) s)
					(file-position s (+ 1 14))
					(write-ascii (cdr (assoc 'password struct)) s)
					(file-position s (+ 1 14 16))
					(write-bytes (bytes
						#x08
						#x00 #x00 #x00 #x00
						#x00 #x00 #x00 #x00
					) s)
				)
			)
			(write-bytes (checksum (get-output-bytes s)) s)
			(write-bytes (make-bytes 4) s)
			(get-output-bytes s)
		)
	)
)
