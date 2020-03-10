; l2j/gameserver/serverpackets/MagicSkillLaunched.java
(module system racket/base
	(require "../../packet.scm")
	(provide game-server-packet/skill-launched)

	(define (game-server-packet/skill-launched buffer)
		(let ((s (open-input-bytes buffer)))
			(list
				(cons 'id (read-byte s))
				(cons 'object-id (read-int32 #f s))
				(cons 'skill-id (read-int32 #f s))
				(cons 'level (read-int32 #f s))
				(cons 'target-id (begin ; TODO AoE
					(read-int32 #f s) ; TODO targets length or failed??
					(read-int32 #f s)
				))
			)
		)
	)
)
