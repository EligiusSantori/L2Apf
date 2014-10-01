; ‡десь все функции игровой логики
(module system racket/base
	(require
		racket/contract
		"structure.scm"
	)
	(provide (contract-out
		(create-world (list? . -> . hash?))
		(create-protagonist (list? . -> . box?))
		(create-antagonist (list? . -> . box?))
		(create-npc (list? . -> . box?))
		;( ( . -> . ))
	))
	
	(define (create-creature struct)
		(append struct (list
			;... TODO
		))
	)
	(define (update-creature creature struct)
		(void) ; TODO
	)
	
	(define (create-npc struct)
		(let ((creature (create-creature struct)))
			(box (append creature (list
				;... TODO
			)))
		)
	)
	(define (update-npc npc struct)
		(void) ; TODO
	)
	;(define (create-citizen struct)
	;	...
	;)
	;(define (create-monster struct)
	;	...
	;)
	(define (create-character struct)
		(let ((creature (create-creature struct)))
			(append creature (list
				;... TODO
			))
		)
	)
	(define (update-character character struct)
		(void) ; TODO
	)
	
	(define (create-antagonist struct)
		(let ((character (create-character struct)))
			(box (append character (list
				;... TODO
			)))
		)
	)
	(define (update-antagonist antagonist struct)
		(void) ; TODO
	)
	
	(define (create-protagonist struct)
		(let ((character (create-character struct)))
			(box (append character (list
				;... TODO
			)))
		)
	)
	(define (update-protagonist protagonist struct)
		(void) ; TODO
	)
	
	(define (create-world me)
		(make-hash (list
			(cons 'me me)
			(cons (get-box-field me 'object-id) me)
		))
	)
)