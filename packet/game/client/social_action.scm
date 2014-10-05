(module packet racket/base
	(require
		srfi/1
		"../../../library/extension.scm"
		"../../packet.scm"
		"../social_action.scm"
	)
	(provide game-client-packet/social-action)
	
	(define (game-client-packet/social-action action)
		(let ((s (open-output-bytes)))
			(let ((id (cdr (assoc action (alist-flip actions)))))
				(begin
					(write-byte #x1b s)
					(write-int32 id #f s)
					(get-output-bytes s)
				)
			)
		)
	)
)