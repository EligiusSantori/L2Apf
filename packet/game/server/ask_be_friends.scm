(module packet racket/base
	(require "../../packet.scm")
	(provide game-server-packet/ask-be-friends)
	
	(define (game-server-packet/ask-be-friends buffer)
		(let ((s (open-input-bytes buffer)))
			(list
				(cons 'id (read-byte s))
				(cons 'name (read-utf16 s))
			)
		)
	)
)
