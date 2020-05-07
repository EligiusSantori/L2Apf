(module system racket/base
	(require "../../packet.scm")
	(provide game-client-packet/stop-moving)

	(define (game-client-packet/stop-moving position angle)
		(let ((s (open-output-bytes)))
			(begin
				(write-byte #x36 s)
				(write-point position s)
				(write-int32 (angle->heading angle) #t s)
				(get-output-bytes s)
			)
		)
	)
)
