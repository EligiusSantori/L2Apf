; l2j/gameserver/clientpackets/DlgAnswer.java
(module system racket/base
	(require "../../packet.scm")
	(provide game-client-packet/confirm)

	(define (game-client-packet/confirm message-id accept?)
		(let ((s (open-output-bytes)))
			(begin
				(write-byte #xc5 s)
				(write-int32 message-id #f s)
				(write-int32 (if accept? 1 0) #f s)
				(get-output-bytes s)
			)
		)
	)
)
