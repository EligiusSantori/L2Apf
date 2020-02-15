(module system racket/base
	(provide game-server-packet/key-packet)
	(require "../../packet.scm")
	
	(define (game-server-packet/key-packet buffer)
		(let ((s (open-input-bytes buffer)))
			(list
				(cons 'id (read-byte s))
				(cons 'key (begin (read-byte s) (read-bytes 8 s)))
			)
		)
	)
)