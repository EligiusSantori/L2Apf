(module packet racket/base
	(provide game-client-packet/restart)
	(require "../../packet.scm")
	
	(define (game-client-packet/restart)
		(let ((s (open-output-bytes)))
			(begin
				(write-byte #x46 s)
				(get-output-bytes s)
			)
		)
	)
)