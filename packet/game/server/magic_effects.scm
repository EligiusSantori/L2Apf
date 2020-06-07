; l2j/gameserver/serverpackets/MagicEffectIcons.java
(module system racket/base
	(require "../../packet.scm")
	(provide game-server-packet/magic-effects)

	(define (read-effects s c [l (list)])
		(if (> c 0)
			(read-effects s (- c 1) (cons (list
				(cons 'skill-id (read-int32 #f s))
				(cons 'level (read-int16 #f s))
				(cons 'duration (read-int32 #f s))
			) l))
			l
		)
	)

	(define (game-server-packet/magic-effects buffer)
		(let ((s (open-input-bytes buffer)))
			(list
				(cons 'id (read-byte s))
				(cons 'effects (read-effects s (read-int16 #f s)))
			)
		)
	)
)
