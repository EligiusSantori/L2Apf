(module ai racket/base
	(require
		racket/undefined
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
	(provide program-partying)

	(define-program program-partying ; TODO Invite on start, create party or change leader to me.
		(lambda (cn ev config . args)
			(let* ((names (car config)) (wr (connection-world cn)) (me (world-me wr)))
				(case-event ev
					('creature-create (id)
						(when (eq? (party-leader (world-party wr)) (object-id me))
							(let ((creature (object-ref wr id)))
								(and
									(character? creature)
									(not (object=? creature me))
									(member (ref creature 'name) names string-ci=?)
									(party-invite cn (ref creature 'name))
								)
							)
						)
					)
					; ('party-memeber-join (id) ; Conflict with program-make-party.
					; 	(when (eq? (party-leader (world-party wr)) (object-id me))
					; 		(let ((character (object-ref wr id)))
					; 			(when (string-ci=? (ref character 'name) (car names))
					; 				(party-crown cn (ref character 'name))
					; 			)
					; 		)
					; 	)
					; )
					('ask/join-party (from loot)
						(when (not (world-party (connection-world cn)))
							(reply cn (event-name ev) (or (not names) (member from names string-ci=?)))
						)
					)
				)
			)

			(void)
		)

		#:defaults (list
			#f ; leader names filter
		)
	)
)
