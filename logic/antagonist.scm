(module logic racket/base
	(require
		(rename-in racket/contract (any all/c))
		srfi/1
		"../library/structure.scm"
		"main.scm"
		"character.scm"
	)
	(provide (contract-out
		(antagonist? (box? . -> . boolean?))
		(create-antagonist (list? . -> . box?))
		(update-antagonist! (box? list? . -> . void?))
	))

	(define (antagonist? object)
		(if (member 'antagonist (@: object 'type)) #t #f)
	)

	(define (create-antagonist struct)
		(let ((character (create-character struct)))
			(let ((type (cons 'antagonist (@: character 'type))))
				(box (append (alist-delete 'type character) (list
					(cons 'type type)
				)))
			)
		)
	)
	
	(define (update-antagonist! antagonist struct)
		(set-box! antagonist
			(let ((antagonist (update-character (unbox antagonist) struct)))
				(struct-transfer antagonist struct
					; ...
				)
			)
		)
	)
)