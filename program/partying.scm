(module ai racket/base
	(require
		(only-in srfi/1 fold car+cdr)
		racket/undefined
		data/queue
		"program.scm"
		(relative-in "../."
			"system/structure.scm"
			"system/connection.scm"
			"system/event.scm"
			"model/object.scm"
			"model/character.scm"
			"model/party.scm"
			"model/world.scm"
			"api/party_invite.scm"
			"api/party_crown.scm"
			"api/reply.scm"
		)
	)
	(provide make-program-partying)

	(define (program-error message . args)
		(apply raise-program-error 'program-partying message args)
	)

	(define (exclude-party wr names)
		(fold (lambda (id r)
			(let ((creature (object-ref wr id)))
				(if creature (remove (ref creature 'name) r string-ci=?) r)
			)
		) names (party-members (world-party wr)))
	)
	(define (next wr names)
		(let ((todo (exclude-party wr names)))
			(if (null? todo) #f (car todo))
		)
	)

	; TODO Auto-invite next members if leader is not present then leave when leader is on sight.
	(define (make-program-partying [names #f] [loot 'finder] [delay 5])
		(make-program 'program-partying
			(lambda (cn ev timer)
				(let* ((wr (connection-world cn)) (me (world-me wr)))
					(case-event ev
						('creature-create (id)
							(or (and names (not (world-party wr)) (string-ci=? (car names) (ref me 'name))
								(let ((creature (object-ref wr id)))
									(and (character? creature) (not (object=? creature me)) (member (ref creature 'name) names string-ci=?)
										; (fold (lambda (n q) (enqueue! q n)) (make-queue) names)
										(timeout! cn delay)
									)
								)
							) timer)
						)
						('ask/join-party (from loot)
							(when (not (world-party (connection-world cn)))
								(reply cn (event-name ev) (or (not names) (member from names string-ci=?)))
							)
							timer
						)
						('party-join args
							(if timer (timeout! #:id timer cn 1) timer)
						)
						('party-memeber-join args
							(if timer (timeout! #:id timer cn 1) timer)
						)
						; ('system-message (message-id arguments) ; TODO make queue from names on creature-create
						; 	(when (member message-id (list 152 160)) ; Player not found or already in party.
						; 		(timeout! #:id 'program-make-party-next cn delay)
						; 	)
						; 	q
						; )
						(timer ()
							(let ((name (next wr (cdr names))))
								(if name (begin (party-invite cn name loot) timer) eof)
							)
						)
						(else timer)
					)
				)
			)

			#:constructor (lambda (cn)
				(when (and names (< (length names) 2))
					(program-error "Party is empty.")
				)
				#f
			)
		)
	)
)
