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
			(and skill (skill-ready? skill) (alive? (world-me wr))
				(begin (send-packet cn (game-client-packet/use-skill skill-id control? shift?)) #t)
			)
		)
	)
)
