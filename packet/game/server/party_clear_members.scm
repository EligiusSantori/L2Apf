(module packet racket/base
	(require "../../packet.scm")
	(provide game-server-packet/party-clear-members)
	
	(define (game-server-packet/party-clear-members buffer)
		(let ((s (open-input-bytes buffer)))
			(list
				(cons 'id (read-byte s))
			)
		)
	)
)
