(module packet racket/base
	(require
		racket/math
		"../../../library/geometry.scm"
		"../../packet.scm"
	)
	(provide game-client-packet/attack-request)
	
	(define (game-client-packet/attack-request object-id origin shift?)
		(let ((s (open-output-bytes)))
			(begin
				(write-byte #x0a s)
				(write-int32 object-id #f s)
				(write-int32 (exact-round (point/3d-x origin)) #t s)
				(write-int32 (exact-round (point/3d-y origin)) #t s)
				(write-int32 (exact-round (point/3d-z origin)) #t s)
				(write-byte (if shift? 1 0) s)
				(get-output-bytes s)
			)
		)
	)
)