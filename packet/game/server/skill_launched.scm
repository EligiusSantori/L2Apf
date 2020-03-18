; l2j/gameserver/serverpackets/MagicSkillLaunched.java
(module system racket/base
	(require "../../packet.scm")
	(provide game-server-packet/skill-launched)

	(define (read-targets s c [l (list)])
		(if (> c 0)
			(let ((target-id (read-int32 #f s)))
				(read-targets s (- c 1) (cons target-id l))
			)
			l
		)
	)

	(define (game-server-packet/skill-launched buffer)
		(let ((s (open-input-bytes buffer)))
			(list
				(cons 'id (read-byte s))
				(cons 'object-id (read-int32 #f s))
				(cons 'skill-id (read-int32 #f s))
				(cons 'level (read-int32 #f s))
				(let ((count (read-int32 #f s)))
					(cons 'targets (read-targets s count))
				)
			)
		)
	)
)
