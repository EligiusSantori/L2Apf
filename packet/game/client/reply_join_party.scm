(module packet racket/base
	(require "../../packet.scm")
	(provide game-client-packet/reply-join-party)
	
	(define (game-client-packet/reply-join-party accept?)
		(let ((s (open-output-bytes)))
			(begin
				(write-byte #x2a s)
				(write-int32 (if accept? 1 0) #f s)
				(get-output-bytes s)
			)
		)
	)
)