; l2j/gameserver/clientpackets/RequestDropItem.java
(module system racket/base
	(require "../../packet.scm")
	(provide game-client-packet/drop-item)

	(define (game-client-packet/drop-item object-id count position)
		(let ((s (open-output-bytes)))
			(begin
				(write-byte #x12 s)
				(write-int32 object-id #f s)
				(write-int32 count #f s)
				(write-point position s)
				(get-output-bytes s)
			)
		)
	)
)
