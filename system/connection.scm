(module system racket/base
	(require
		(only-in racket/function const)
		racket/port
		racket/async-channel
		racket/contract
		(only-in "../packet/packet.scm" get-packet-id)
		(only-in "crypter.scm" crypter?)
		"log.scm"
	)
	(provide
		(struct-out connection)
		(contract-out
			(read-buffer (-> input-port? bytes?))
			(read-thread (-> input-port? crypter? async-channel? void?))
			(read-packet (-> connection? bytes?))
			(write-buffer (-> output-port? bytes? void?))
			(send-thread (-> output-port? crypter? void?))
			(send-packet (-> connection? bytes? void?))
			(disconnect (-> connection? void?))
		)
	)

	(struct connection (
		protocol
		[account #:auto]
		[session-id #:auto]
		[session-key #:auto]

		[tick-channel #:auto]
		[packet-channel #:auto]
		[event-channel #:auto]

		[read-thread #:auto]
		[send-thread #:auto]
		[timer-thread #:auto]

		[world #:auto]
		[db #:auto]
	) #:mutable)

	(define (read-buffer port)
		(let loop ()
			(let ((size (integer-bytes->integer (read-bytes 2 port) #f)))
				(if (> size 2)
					(read-bytes (- size 2) port)
					(loop) ; Skip empty packets.
				)
			)
		)
	)

	(define (read-thread port crypter channel)
		(let ((evt (choice-evt (wrap-evt (thread-receive-evt) (const #f)) (read-bytes-evt 2 port))))
			(do ((buffer (sync evt) (sync evt))) ((or (not buffer) (eof-object? buffer)))
				(let ((size (integer-bytes->integer buffer #f)))
					(if (> size 2)
						(let ((buffer (read-bytes (- size 2) port)))
							; (print-dump "buffer <-: " buffer)
							(let ((buffer (crypter buffer #f)))
								(apf-debug "Packet <- ~v" buffer)
								(async-channel-put channel buffer)
							)
						)
						(void)
					)
				)
			)

			(close-input-port port)
		)
	)

	(define (read-packet connection)
		(async-channel-get (connection-packet-channel connection))
	)

	(define (write-buffer port buffer)
		(let ((size (+ (bytes-length buffer) 2)))
			(write-bytes (integer->integer-bytes size 2 #f) port)
			(write-bytes buffer port)
			(flush-output port)
		)
	)

	(define (send-thread port crypter)
		(do ((buffer (thread-receive) (thread-receive))) ((not buffer))
			(apf-debug "Packet -> ~v" buffer)
			(let ((buffer (crypter buffer #t)))
				;(print-dump "buffer ->: " buffer)
				(write-buffer port buffer)
			)
		)

		(close-output-port port)
	)

	(define (send-packet connection buffer)
		(thread-send (connection-send-thread connection) buffer)
	)

	(define (disconnect connection)
		(let ((read-thread (connection-read-thread connection)))
			(when read-thread (thread-send read-thread #f #f))
		)
		(let ((send-thread (connection-send-thread connection)))
			(when send-thread (thread-send send-thread #f #f))
		)
		(let ((timer-thread (connection-timer-thread connection)))
			(when timer-thread (thread-send timer-thread #f #f))
		)
		(void)
	)
)
