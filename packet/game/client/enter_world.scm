(module system racket/base
	(provide game-client-packet/enter-world)
	(require "../../packet.scm")

	(define (game-client-packet/enter-world)
		(let ((s (open-output-bytes)))
			(begin
				(write-byte #x03 s)
				(write-bytes (bytes
					#x45 #x00 #x01 #x00
					#x1e #x37 #xa2 #xf5
					#x00 #x00 #x00 #x00
					#x00 #x00 #x00 #x00
				) s)
				(get-output-bytes s)
			)
		)
	)
)