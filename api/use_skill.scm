(module logic racket/base
	(require
		"../packet/game/client/use_skill.scm"
		"../model/skill.scm"
		"../model/world.scm"
		"../system/structure.scm"
		"../system/connection.scm"
	)
	(provide use-skill)

	(define (use-skill cn skill-id [control? #f] [shift? #f])
		(let* ((wr (connection-world cn)) (skill (skill-ref wr skill-id)))
			(when (and (alive? (world-me wr)) (skill-ready? skill))
				(send-packet cn (game-client-packet/use-skill skill-id control? shift?))
			)
		)
	)
)
