; l2j/gameserver/network/clientpackets/RequestItemList.java
(module system racket/base
	(provide game-client-packet/refresh-inventory)
	(require "../../packet.scm")

	(define (game-client-packet/refresh-inventory)
		(let ((s (open-output-bytes)))
			(begin
				(write-byte #x0f s)
				(get-output-bytes s)
			)
		)
	)
)
