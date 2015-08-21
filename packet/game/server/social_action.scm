(module packet racket/base
	(require
		"../../../library/extension.scm"
		"../../packet.scm"
		"../gesture.scm"
	)
	(provide game-server-packet/social-action)
	
	(define (game-server-packet/social-action buffer)
		(let ((s (open-input-bytes buffer)))
			(list
				(cons 'id (read-byte s))
				(cons 'object-id (read-int32 #f s))
				(cons 'action (alist-ref actions (read-int32 #f s)))
			)
		)
	)
)