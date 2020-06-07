(module ai racket/base
	(require
		(only-in srfi/1 fold)
		racket/undefined
		data/queue
		"program.scm"
		(relative-in "../."
			"library/extension.scm"
			"system/structure.scm"
			"system/connection.scm"
			"system/event.scm"
			"model/object.scm"
			"model/party.scm"
			"model/world.scm"
			"api/party_invite.scm"
			"api/party_crown.scm"
		)
	)
	(provide program-make-party)

	(define (program-error message . args)
		(apply raise-program-error 'program-make-party message args)
	)

	(define (next cn q loot)
		(if (not (queue-empty? q))
			(let ((me (world-me (connection-world cn))) (name (dequeue! q)))
				(if (not (string-ci=? (ref me 'name) name))
					(begin (party-invite cn name loot) q)
					(next cn q loot)
				)
			)
			eof
		)
	)

	(define-program program-make-party
		(lambda (cn ev config q)
			(let-values (((names loot delay) (list->values config)))
				(case-event ev
					('party-join args
						(timeout! #:id 'program-make-party-next cn delay)
						q
					)
					('party-memeber-join args
						(timeout! #:id 'program-make-party-next cn delay)
						q
					)
					('system-message (message-id arguments)
						(when (member message-id (list 152 160)) ; Player not found or already in party.
							(timeout! #:id 'program-make-party-next cn delay)
						)
						q
					)
					('program-make-party-next ()
						(next cn q loot)
					)
					(else q)
				)
			)
		)

		#:constructor (lambda (cn config)
			(let-values (((names loot delay) (list->values config)))
				(when (or (not names) (null? names) (< (length names) 2))
					(program-error "Party is empty.")
				)
				(let ((q (fold (lambda (name q) (enqueue! q name) q) (make-queue) names)))
					(next cn q loot)
				)
			)
		)

		#:destructor (lambda (cn config state)
			(let-values (((names loot delay) (list->values config)))
				(let ((wr (connection-world cn)))
					(when (eq? (party-leader (world-party wr)) (object-id (world-me wr)))
						(party-crown cn (car names))
					)
				)
			)
		)

		#:defaults (list
			undefined ; names (required)
			'finder ; loot type
			1 ; delay
		)
	)
)
