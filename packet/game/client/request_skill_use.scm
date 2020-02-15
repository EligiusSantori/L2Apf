(module system racket/base
	(require "../../packet.scm")
	(provide game-client-packet/request-skill-use)
	
	(define (game-client-packet/request-skill-use skill-id control? shift?)
		(let ((s (open-output-bytes)))
			(begin
				(write-byte #x2f s)
				(write-int32 skill-id #f s)
				(write-int32 (if control? 1 0) #f s)
				(write-byte (if shift? 1 0) s)
				(get-output-bytes s)
			)
		)
	)
)