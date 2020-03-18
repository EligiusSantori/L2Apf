; l2j/gameserver/clientpackets/Logout.java
(module system racket/base
	(provide game-client-packet/logout)
	(require "../../packet.scm")

	(define (game-client-packet/logout)
		(let ((s (open-output-bytes)))
			(begin
				(write-byte #x09 s)
				(get-output-bytes s)
			)
		)
	)
)
