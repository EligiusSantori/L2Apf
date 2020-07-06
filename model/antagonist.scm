(module logic racket/base
	(require
		(only-in srfi/1 alist-delete)
		racket/contract
		"../system/structure.scm"
		"object.scm"
		"character.scm"
	)
	(provide (contract-out
		(antagonist? (-> any/c boolean?))
		(make-antagonist (-> list? any/c box?))
		(update-antagonist! (-> box? list? list?))
	))

	(define (antagonist? object)
		(object-of-type? object 'antagonist)
	)

	(define (make-antagonist data db)
		(let ((character (make-character data db)))
			(let ((type (cons 'antagonist (ref character 'type))))
				(box (cons (cons 'type type) (alist-delete 'type character)))
			)
		)
	)

	(define (update-antagonist! object data)
		(let-values (((rest updated changes) (update-character (unbox object) data)))
			(set-box! object (append rest updated))
			changes
		)
	)
)
