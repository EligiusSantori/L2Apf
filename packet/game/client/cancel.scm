(module packet racket/base
	(provide game-client-packet/cancel)
	(require "../../packet.scm")
	
	(define (game-client-packet/cancel)
		(let ((s (open-output-bytes)))
			(begin
				(write-byte #x37 s)
				(write-int16 0 #f s)
				(get-output-bytes s)
			)
		)
	)
)