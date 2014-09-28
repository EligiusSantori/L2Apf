(module packet racket/base
	(provide game-client-packet/refresh-manor-list)
	(require "../../packet.scm")
	
	(define (game-client-packet/refresh-manor-list)
		(let ((s (open-output-bytes)))
			(begin
				(write-byte #xd0 s)
				(write-int16 #x08 #f s)
				(get-output-bytes s)
			)
		)
	)
)