(module api racket/base
	(require
		racket/async-channel
		"../library/structure.scm"
		"../library/network.scm"
	)
	(provide send-thread)
	
	(define (send-thread connection)
		(define channel (get-box-field connection 'output-channel))
		
		(let loop ()
			(send connection (async-channel-get channel))
			(loop)
		)
	)
)