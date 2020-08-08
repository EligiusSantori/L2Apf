(module ai racket/base
	(require
		racket/string
		racket/undefined
		"program.scm"
		(relative-in "../."
			"library/extension.scm"
			"system/structure.scm"
			"system/event.scm"
			"system/connection.scm"
			"model/object.scm"
			"model/npc.scm"
			"model/character.scm"
			"model/world.scm"
			"api/say.scm"
		)
	)
	(provide make-program-report)

	(define (make-program-report [low-hp 1/3] [support-low-mp 1/4] [under-attack #t] [channel 'chat-channel/all])
		(make-program 'program-report
			(lambda (cn event . rest)
				(let* ((wr (connection-world cn)) (me (world-me wr)))
					(case-event event
						('creature-update (id changes)
							(when (= id (object-id me))
								(when low-hp
									(let ((change (ref changes 'hp)) (max-hp (ref me 'max-hp)))
										(and
											change
											(> (/ (or (cdr change) max-hp) max-hp) low-hp)
											(<= (/ (or (car change) max-hp) max-hp) low-hp)
											(say cn "HELP!" channel)
										)
									)
								)
								(when (and support-low-mp (support-class? me))
									(let ((change (ref changes 'mp)) (max-mp (ref me 'max-mp)))
										(and
											change
											(> (/ (or (cdr change) max-mp) max-mp) support-low-mp)
											(<= (/ (or (car change) max-mp) max-mp) support-low-mp)
											(say cn "Low MP!" channel)
										)
									)
								)
							)
						)
						('change-target (subject-id target-id . rest)
							(let ((creature (object-ref wr subject-id)))
								(when (and under-attack (eq? target-id (object-id me)) (npc? creature) (or (boss? creature) (minion? creature)))
									(say cn (format "Attaked by ~a!" (ref creature 'name)) channel)
								)
							)
						)
					)
				)
			)
		)
	)
)
