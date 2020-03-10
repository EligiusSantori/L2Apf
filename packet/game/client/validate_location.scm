(module system racket/base
	(require
		racket/math
		"../../../library/geometry.scm"
		"../../packet.scm"
	)
	(provide game-client-packet/validate-location)

	(define (game-client-packet/validate-location point angle)
		(let ((s (open-output-bytes)))
			(begin
				(write-byte #x48 s)
				(write-point point s)
				(write-int32 (angle->heading angle) #t s)
				(get-output-bytes s)
			)
		)
	)
)
