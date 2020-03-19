; l2j/gameserver/clientpackets/RequestEnchantItem.java
(module system racket/base
	(require "../../packet.scm")
	(provide game-client-packet/enchant-item)

	(define (game-client-packet/enchant-item object-id)
		(let ((s (open-output-bytes)))
			(begin
				(write-byte #x58 s)
				(write-int32 object-id #f s)
				(get-output-bytes s)
			)
		)
	)
)
