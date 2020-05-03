(module ai racket/base
	(require
		srfi/1
		racket/undefined
		"program.scm"
		(relative-in "../."
			; "library/extension.scm"
			"system/structure.scm"
			"system/connection.scm"
			"system/event.scm"
			"model/object.scm"
			"model/item.scm"
			"model/world.scm"
			"api/pick_up.scm"
		)
	)
	(provide program-pickup)

	(define (program-error message . args)
		(apply raise-program-error 'program-pickup message args)
	)

	(define-program program-pickup
		(lambda (cn event config state)
			(let ((pickup-id (car config)) (me (world-me (connection-world cn))))
				(and
					(member (event-name event) (list 'object-delete 'item-pick))
					(= (second event) pickup-id)
					eof
				)

				; Wouldn't work because object-delete comes before item-pick.
				; (case-event event
				; 	(item-pick (id subject-id . rest)
				; 		(if (= id pickup-id)
				; 			(if (eq? subject-id (object-id me))
				; 				eof ; Item has been picked by me.
				; 				(program-error "Item has been picked by someone else." subject-id)
				; 			)
				; 			(void)
				; 		)
				; 	)
				; 	(object-delete (id)
				; 		(if (= id pickup-id)
				; 			(program-error "Item has been deleted." pickup-id)
				; 			(void)
				; 		)
				; 	)
				; )
			)
		)

		#:constructor (lambda (cn config)
			(let* ((pickup-id (car config)) (item (object-ref (connection-world cn) pickup-id)))
				(when (not item) (program-error "Don't see the item." pickup-id))
				(when (not (item? item)) (program-error "Object is not item." pickup-id))
				(when (not (on-ground? item)) (program-error "Item is not on the ground." pickup-id))

				(pick-up cn pickup-id)
			)
		)

		#:defaults (list
			undefined ; object-id (required)
		)
	)
)
