; l2j/gameserver/clientpackets/RequestMagicSkillUse.java
(module system racket/base
	(require "../../packet.scm")
	(provide game-client-packet/use-skill)

	(define (game-client-packet/use-skill skill-id control? shift?)
		(let ((s (open-output-bytes)))
			(begin
				(write-byte #x2f s)
				(write-int32 skill-id #f s)
				(write-int32 (if control? 1 0) #f s)
				(write-byte (if shift? 1 0) s)
				(get-output-bytes s)
			)
		)
	)
)
