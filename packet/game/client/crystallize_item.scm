; l2j/gameserver/clientpackets/RequestCrystallizeItem.java
(module system racket/base
	(require "../../packet.scm")
	(provide game-client-packet/crystallize-item)

	(define (game-client-packet/crystallize-item object-id count)
		(let ((s (open-output-bytes)))
			(begin
				(write-byte #x72 s)
				(write-int32 object-id #f s)
				(write-int32 count #f s)
				(get-output-bytes s)
			)
		)
	)
)
