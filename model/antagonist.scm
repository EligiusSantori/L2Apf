(module logic racket/base
	(require
		(rename-in racket/contract (any all/c))
		srfi/1
		"../system/structure.scm"
		"character.scm"
	)
	(provide (contract-out
		(antagonist? (any/c . -> . boolean?))
		(create-antagonist (list? . -> . box?))
		(update-antagonist! (box? list? . -> . void?))
	))

	(define (antagonist? object)
		(if (and object (member 'antagonist (@: object 'type))) #t #f)
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
