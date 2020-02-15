(module system racket/base
	(require
		racket/async-channel
		"structure.scm"
		"network.scm"
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
