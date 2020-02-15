(module system racket/base
	(require
		racket/math
		"../../../library/geometry.scm"
		"../../packet.scm"
	)
	(provide game-client-packet/action)
	
	(define (game-client-packet/action object-id origin shift?)
		(let ((s (open-output-bytes)))
			(begin
				(write-byte #x04 s)
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