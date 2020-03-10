(module logic racket/base
	(require
		srfi/1
		(rename-in racket/contract (any all/c))
		"../system/structure.scm"
		"character.scm"
	)
	(provide (contract-out
		(antagonist? (-> any/c boolean?))
		(make-antagonist (-> list? box?))
		(update-antagonist! (-> box? list? list?))
	))

	(define (antagonist? object)
		(if (and object (member 'antagonist (ref object 'type))) #t #f)
	)

	(define (make-antagonist data)
		(let ((character (make-character data)))
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
