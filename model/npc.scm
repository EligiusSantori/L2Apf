(module logic racket/base
	(require
		(rename-in racket/contract (any all/c))
		srfi/1
		"../library/structure.scm"
		"../_logic.scm"
		"object.scm"
		"creature.scm"
	)
	(provide (contract-out
		(npc? ((or/c box? false/c) . -> . boolean?))
		(create-npc (list? . -> . box?))
		(update-npc! (box? list? . -> . void?))
	))
	
	(define (npc? object)
		(if (and object (member 'npc (@: object 'type))) #t #f)
	)
	
	(define (create-npc struct)
		(let ((creature (create-creature struct)))
			(let ((type (cons 'npc (@: creature 'type))))
				(box (append (alist-delete 'type creature) (list
					(cons 'type type)
					
					(cons 'npc-id (@: struct 'npc-id))
					(cons 'show-name? (@: struct 'show-name?))
					(cons 'attackable? (@: struct 'attackable?))
					(cons 'spoiled? (@: struct 'spoiled?))
					(cons 'summoned? (@: struct 'summoned?))
				)))
			)
		)
	)
	
	(define (update-npc! npc struct)
		(set-box! npc
			(let ((npc (update-creature (unbox npc) struct)))
				(struct-transfer npc struct
					'npc-id
					'show-name?
					'attackable?
					'spoiled?
					'summoned?
				)
			)
		)
	)
)
