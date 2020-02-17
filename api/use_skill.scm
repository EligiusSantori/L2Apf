(module logic racket/base
	(require
		srfi/1
		"../packet/game/client/request_skill_use.scm"
		"../model/skill.scm"
		"../model/world.scm"
		"../system/structure.scm"
		"../system/connection.scm"
	)
	(provide use-skill)

	(define (use-skill cn skill-id . tail)
		(define control? (if (> (length tail) 0) (first tail) #t))
		(define shift? (if (> (length tail) 1) (second tail) #f))

		(let* ((wr (connection-world cn)) (skill (skill-ref wr skill-id)))
			(when (and (alive? (world-me wr)) (skill-ready? skill))
				(send-packet cn (game-client-packet/request-skill-use skill-id control? shift?))
			)
		)
	)
)
