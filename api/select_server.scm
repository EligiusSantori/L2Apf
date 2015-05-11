(module api racket/base
	(require
		racket/contract
		racket/tcp
		"../library/crypter.scm"
		"../library/structure.scm"
		"../library/network.scm"
		"../packet/login/client/select_server.scm"
		"../packet/login/server/play_fail.scm"
		"../packet/login/server/play_ok.scm"
		"../packet/game/client/protocol_version.scm"
		"../packet/game/server/key_packet.scm"
		"../packet/game/client/validate_auth.scm"
		"../packet/game/server/character_list.scm"
	)
	(provide select-server)

	(define (select-server connection server)
		(set-box-field! connection 'world server)
		(let () ; login-server interaction
			(define input-port (@: connection 'input-port))
			(define output-port (@: connection 'output-port))
			(define crypter (@: connection 'crypter))
			
			(send connection (login-client-packet/select-server (list
				(cons 'server-id (hash-ref server 'id))
				(cons 'login-key (@: connection 'login-key))
			)))
			
			(let loop ()
				(let ((buffer (receive connection)))
					(case (get-packet-id buffer)
						((#x06) (let ((packet (login-server-packet/play-fail buffer)))
							(disconnect connection)
							(error (string-append "Connection failed: " (symbol->string (@: packet 'reason))))					
						))
						((#x07) (let ((packet (login-server-packet/play-ok buffer)))
							(begin
								(set-box-field! connection 'game-key (@: packet 'game-key))
								(set-box-field! connection 'session-id #f)
								(set-box-field! connection 'crypter #f)
								(set-box-field! connection 'rsa-key #f)
								(disconnect connection)
							)
						))
						(else #f)
					)
				)
			)
		)
		
		(if (@: connection 'game-key) ; game-server interaction
			(let ((host (@: server 'address)) (port (@: server 'port)))
				(let-values (((input-port output-port) (tcp-connect host port)))
					
					(set-box-field! connection 'input-port input-port)
					(set-box-field! connection 'output-port output-port)
				
					(send connection (game-client-packet/protocol-version (list
						(cons 'protocol (@: connection 'protocol))
					)))
				
					(let loop ()
						(let ((buffer (receive connection)))
							(case (get-packet-id buffer)
								((#x00) (let ((packet (game-server-packet/key-packet buffer)))
									(begin
										(let ((crypter (make-crypter (@: packet 'key))))
											(set-box-field! connection 'crypter crypter)
										)
										
										(send connection (game-client-packet/validate-auth (list
											(cons 'login (@: connection 'account))
											(cons 'login-key (@: connection 'login-key))
											(cons 'game-key (@: connection 'game-key))
										)))
										(loop)
									)
								))
								((#x13) (let ((packet (game-server-packet/character-list buffer)))
									(define (transform i) (cons (cdr (assoc 'name i)) (box (cdr (assoc 'id i)))))
									
									(set-box-field! connection 'account #f)
									(set-box-field! connection 'protocol #f)
									(set-box-field! connection 'login-key #f)
									(set-box-field! connection 'game-key #f)
									
									(map transform (@: packet 'list))
								))
								(else #f)
							)
						)
					)
				)
			)
			#f
		)
	)
)