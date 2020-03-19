; l2j/gameserver/clientpackets/UseItem.java
(module system racket/base
	(require "../../packet.scm")
	(provide game-client-packet/use-item)

	(define (game-client-packet/use-item object-id control?)
		(let ((s (open-output-bytes)))
			(begin
				(write-byte #x14 s)
				(write-int32 object-id #f s)
				(write-int32 (if control? 1 0) #f s)
				(get-output-bytes s)
			)
		)
	)
)
