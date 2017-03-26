(module script racket/base
	(require
		srfi/1
		racket/string
		(relative-in "../.."
			"system/uri_scheme.scm"
			"library/extension.scm"
			"library/structure.scm"
			;"library/geometry.scm"
			"model/world.scm"
			;"api/move_to.scm"
		)
	)
	(provide
		parse-protocol
		parse-command
		format-chat-message
		parse-targets
	)
	
	(define (parse-protocol command-line)
		(if (> (vector-length command-line) 0)
			(let ((uri (parse-uri (vector-ref command-line 0))))
				(if uri
					(apply values uri)
					(error "Authentication failed because URI is broken")
				)
			)
			(error "Authentication failed because URI is missed")
		)
	)

	(define (parse-command text)
		(let ((t (string-trim (string-downcase text))))
			(if (string-starts? t "/")
				(let ((l (map string-trim (string-split t " "))))
					(cons (substring (car l) 1) (cdr l))
				)
				#f
			)
		)
	)

	(define (format-chat-message object-id channel author text)
		(let ((channel (string-titlecase (last (string-split (symbol->string channel) "/")))))
			(string-append "[" channel "] " author ": " text)
		)
	)
	
	(define (parse-targets connection author . arguments)
		(define (parse-target token)
			(case token
				((#f "me") (@: (or author (list)) 'object-id))
				(("self") (@: connection 'world 'me 'object-id))
				(("my") (@: (or author (list)) 'target-id))
				(("party")
					(displayln (@: connection 'world 'party))
					(list) ; TODO
				)
				(else (@: (or (find-character-by-name (@: connection 'world) token) (list)) 'object-id))
			)
		)
		
		(if (not (null? arguments))
			(fold (lambda (s p)
				(let ((t (parse-target s)))
					(append p (cond
						((integer? t) (list t))
						((not t) (list))
						(else t)
					))
				)
			) (list) arguments)
			(list (parse-target #f))
		)
	)
)
