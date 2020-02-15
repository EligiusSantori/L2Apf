(module system racket/base
	(require
		racket/async-channel
		"structure.scm"
		"network.scm"
	)
	(provide send-thread)

	(define (send-thread connection)
		(define channel (@: connection 'output-channel))

		(let loop ()
			(send connection (async-channel-get channel))
			(loop)
		)
	)
)
