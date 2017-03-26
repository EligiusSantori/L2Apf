(module packet racket/base
	(require "../../packet.scm")
	(provide game-client-packet/request-item-use)
	
	(define (game-client-packet/request-item-use object-id)
		(let ((s (open-output-bytes)))
			(begin
				(write-byte #x14 s)
				(write-int32 object-id #f s)
				(write-int32 0 #f s)
				(get-output-bytes s)
			)
		)
	)
)