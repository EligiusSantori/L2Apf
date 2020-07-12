(module logic racket/base
	(require
		racket/contract
		"../packet/game/client/use_skill.scm"
		"../model/skill.scm"
		"../model/world.scm"
		"../system/structure.scm"
		"../system/connection.scm"
	)
	(provide (contract-out
		(use-skill (->* (connection? (or/c skill? false/c)) (boolean? boolean?) boolean?))
	))

	(define (use-skill cn skill [control? #f] [shift? #f])
		(let ((wr (connection-world cn)))
			(and skill (skill-ready? skill) (alive? (world-me wr))
				(begin (send-packet cn (game-client-packet/use-skill (skill-id skill) control? shift?)) #t)
			)
		)
	)
)
