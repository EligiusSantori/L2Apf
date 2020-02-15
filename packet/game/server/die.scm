(module system racket/base
	(require "../../packet.scm")
	(provide game-server-packet/die)
	
	(define (get-return town clanhall castle siege-hq fixed)
		(filter symbol? (list
			(if (zero? town) #f 'return-point/town)
			(if (zero? clanhall) #f 'return-point/clanhall)
			(if (zero? castle) #f 'return-point/castle)
			(if (zero? siege-hq) #f 'return-point/siege-hq)
			(if (zero? fixed) #f 'return-point/fixed)
		))
	)
	
	(define (game-server-packet/die buffer)
		(let* ((s (open-input-bytes buffer)) (id (read-byte s)) (object-id (read-int32 #f s)))
			(let ((town (read-int32 #f s)) (clanhall (read-int32 #f s)) (castle (read-int32 #f s)) (siege-hq (read-int32 #f s)))
				(list
					(cons 'id id)
					(cons 'object-id object-id)
					(cons 'spoiled? (not (zero? (read-int32 #f s))))
					(cons 'return (get-return town clanhall castle siege-hq (read-int32 #f s)))
				)
			)
		)
	)
)
