(module logic racket/base
	(require
		racket/contract
		racket/async-channel
		racket/tcp
		(relative-in "../."
			"library/blowfish.scm"
			"library/extension.scm"
			"system/structure.scm"
			"system/connection.scm"
			(only-in "packet/packet.scm" get-packet-id)
			"packet/login/server/init.scm"
			"packet/login/client/gg_auth.scm"
			"packet/login/server/gg_reply.scm"
		)
	)
	(provide (contract-out
		(connect (->* (string?) (integer? string? integer?) connection?))
	))

	(define (connect host [port 2106] [token #"_;5.]94-31==-%xT!^[$"] [protocol 660])
		(let ((cn (connection protocol)) (pc (make-async-channel)) (crypter (make-blowfish-crypter token)))
			(let-values (((input-port output-port) (tcp-connect host port)))
				(let loop ()
					(let ((buffer (if (connection-session-id cn) (read-packet cn) (read-buffer input-port))))
						(case (get-packet-id buffer)
							((#x00) (let ((packet (login-server-packet/init buffer)))
								(set-connection-packet-channel! cn pc)
								(set-connection-read-thread! cn (thread (bind read-thread input-port crypter pc)))
								(set-connection-send-thread! cn (thread (bind send-thread output-port crypter)))
								(set-connection-session-id! cn (ref packet 'session-id))
								(set-connection-session-key! cn (ref packet 'rsa-key))
								(send-packet cn (login-client-packet/gg-auth (list
									(cons 'session-id (connection-session-id cn))
								)))
								(loop)
							))
							((#x0b) (let ((packet (login-server-packet/gg-reply buffer)))
								cn
							))
							(else #f) ; TODO Raise error.
						)
					)
				)
			)
		)
	)
)
