(module api racket/base
	(require
		srfi/1
		"../library/structure.scm"
		"../library/network.scm"
		"../model/skill.scm"
		"../model/world.scm"
		"../packet/game/client/request_skill_use.scm"
	)
	(provide use-skill)
	
	(define (use-skill connection skill-id . tail)
		(define control? (if (> (length tail) 0) (first tail) #t))
		(define shift? (if (> (length tail) 1) (second tail) #f))
		
		(let* ((world (@: connection 'world)) (skill (skill-ref world skill-id)))
			(when (and (not (@: world 'me 'died?)) (skill-ready? skill))
				(send connection (game-client-packet/request-skill-use skill-id control? shift?))
			)
		)
	)
)