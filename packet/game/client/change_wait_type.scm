(module system racket/base
	(require "../../packet.scm")
	(provide game-client-packet/change-wait-type)
	
	(define (game-client-packet/change-wait-type sit?)
		(let ((s (open-output-bytes)))
			(begin
				(write-byte #x1d s)
				(write-int32 (if sit? 0 1) #f s)
				(get-output-bytes s)
			)
		)
	)
)