(module system racket/base
	(require
		srfi/1
		"../../../library/extension.scm"
		"../../packet.scm"
		"../return_point.scm"
	)
	(provide game-client-packet/request-return)
	
	(define (game-client-packet/request-return point)
		(let ((s (open-output-bytes)))
			(let ((id (cdr (assoc point (alist-flip return-points)))))
				(begin
					(write-byte #x6d s)
					(write-int32 id #f s)
					(get-output-bytes s)
				)
			)
		)
	)
)
