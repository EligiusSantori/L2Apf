(module packet racket/base
	(require
		racket/math
		"../../../library/geometry.scm"
		"../../packet.scm"
	)
	(provide game-client-packet/validate-position)
	
	(define (game-client-packet/validate-position point angle)
		(let ((s (open-output-bytes)))
			(begin
				(write-byte #x48 s)
				(write-int32 (exact-round (point/3d-x point)) #t s)
				(write-int32 (exact-round (point/3d-y point)) #t s)
				(write-int32 (exact-round (point/3d-z point)) #t s)
				(write-int32 (angle->heading angle) #t s)
				(get-output-bytes s)
			)
		)
	)
)
