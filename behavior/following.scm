; follow [me]
; follow {player}


(module script racket/base
	(require
		srfi/1
		;racket/string
		(relative-in "../.."
			"library/structure.scm"
			"model/world.scm"
			"api/sit.scm"
			"api/target.scm"
			"api/move_to.scm"
		)
	)
	(provide make-follower)

	(define (make-follower connection)	
		(define queue (list))
		
		(define (iterate)
			(let ((leader (object-ref (@: connection 'world) (@: connection 'world 'me 'target-id))))
				(if leader (begin (move-to connection (or (@: leader 'destination) (@: leader 'position))) #t) #f)
			)
		)
		
		(lambda arguments
			(if (not (null? arguments))
				(let ((leader (object-ref (@: connection 'world) (first arguments))))
					(if leader
						(begin
							(target connection (@: leader 'object-id))
							(when (@: connection 'world 'me 'sitting?) (sit connection #f) (sleep 1/3))
							(move-to connection (or (@: leader 'destination) (@: leader 'position)))
						)
						#f
					)
				)
				(iterate)
			)
		)
	)
)
