(module packet racket/base
	(provide game-client-packet/protocol-version)
	(require "../../packet.scm")

	(define (game-client-packet/protocol-version struct)
		(let ((s (open-output-bytes)))
			(begin
				(write-byte #x00 s)
				(write-int32 (cdr (assoc 'protocol struct)) #f s)
				(get-output-bytes s)
			)
		)
	)
)