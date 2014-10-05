(module packet racket/base
	(require
		"../../packet.scm"
		"../social_action.scm"
	)
	(provide game-server-packet/social-action)
	
	(define (game-server-packet/social-action buffer)
		(let ((s (open-input-bytes buffer)))
			(list
				(cons 'id (read-byte s))
				(cons 'object-id (read-int32 #f s))
				(cons 'action (cdr (assoc (read-int32 #f s) actions)))
			)
		)
	)
)