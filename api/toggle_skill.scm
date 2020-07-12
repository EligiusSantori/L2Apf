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
		(toggle-skill (-> connection? (or/c skill? false/c) boolean? boolean?))
	))

	(define (toggle-skill cn skill is?)
		(let ((wr (connection-world cn)))
			(and skill (not (eq? (ref skill 'enabled?) is?)) (alive? (world-me wr))
				(begin (send-packet cn (game-client-packet/use-skill (skill-id skill) #f #f)) #t)
			)
		)
	)
)
