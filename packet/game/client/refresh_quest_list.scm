(module system racket/base
	(provide game-client-packet/refresh-quest-list)
	(require "../../packet.scm")
	
	(define (game-client-packet/refresh-quest-list)
		(let ((s (open-output-bytes)))
			(begin
				(write-byte #x63 s)
				(get-output-bytes s)
			)
		)
	)
)