(module system racket/base
	(provide game-server-packet/chat-message)
	(require "../../packet.scm" "../channel.scm")
	
	(define (game-server-packet/chat-message buffer)
		(let ((s (open-input-bytes buffer)))
			(list
				(cons 'id (read-byte s))
				(cons 'object-id (read-int32 #f s))
				(cons 'channel (cdr (assoc (read-int32 #t s) channels =)))
				(cons 'author (read-utf16 s))
				(cons 'text (read-utf16 s))
			)
		)
	)
)