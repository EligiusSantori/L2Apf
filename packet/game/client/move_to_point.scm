(module system racket/base
	(require
		racket/math
		"../../../library/extension.scm"
		"../../../library/geometry.scm"
		"../../packet.scm"
	)
	(provide game-client-packet/move-to-point)
	
	(define devices (list
		(cons #x0 'keyboard)
		(cons #x1 'mouse)
	))
	
	(define (game-client-packet/move-to-point origin target [device 'mouse])
		(let ((s (open-output-bytes)))
			(begin
				(write-byte #x01 s)
				(write-int32 (exact-round (point/3d-x target)) #t s)
				(write-int32 (exact-round (point/3d-y target)) #t s)
				(write-int32 (exact-round (point/3d-z target)) #t s)
				(write-int32 (exact-round (point/3d-x origin)) #t s)
				(write-int32 (exact-round (point/3d-y origin)) #t s)
				(write-int32 (exact-round (point/3d-z origin)) #t s)
				(write-int32 (cdr (assoc device (alist-flip devices))) #f s)
				(get-output-bytes s)
			)
		)
	)
)
