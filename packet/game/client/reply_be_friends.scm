(module packet racket/base
	(require "../../packet.scm")
	(provide game-client-packet/reply-be-friends)
	
	(define (game-client-packet/reply-be-friends accept?)
		(let ((s (open-output-bytes)))
			(begin
				(write-byte #x5f s)
				(write-int32 (if accept? 1 0) #f s)
				(get-output-bytes s)
			)
		)
	)
)