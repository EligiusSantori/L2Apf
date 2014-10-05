(module system racket/base
	(require
		racket/async-channel
		"../library/structure.scm"
		"../library/network.scm"
	)
	(provide read-thread)
	
	(define (read-thread connection)
		(define channel (@: connection 'input-channel))
		
		(let loop ()
			(async-channel-put channel (receive connection))
			(loop)
		)
	)
)