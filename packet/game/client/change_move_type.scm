(module system racket/base
	(require "../../packet.scm")
	(provide game-client-packet/change-move-type)
	
	(define (game-client-packet/change-move-type run?)
		(let ((s (open-output-bytes)))
			(begin
				(write-byte #x1c s)
				(write-int32 (if run? 1 0) #f s)
				(get-output-bytes s)
			)
		)
	)
)