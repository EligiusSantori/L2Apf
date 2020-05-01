(module system racket/base
	(provide game-server-packet/key-packet)
	(require "../../packet.scm")

	(define (game-server-packet/key-packet buffer)
		(let ((s (open-input-bytes buffer)))
			(list
				(cons 'id (read-byte s))
				(cons 'protocol-match? (not (zero? (read-byte s))))
				(cons 'key (read-bytes 4 s))
			)
		)
	)
)
