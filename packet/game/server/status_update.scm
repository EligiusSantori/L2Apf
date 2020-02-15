(module system racket/base
	(require srfi/1 "../../packet.scm")
	(provide game-server-packet/status-update)
	
	(define attribute (vector-immutable ; performance critical
		#f ; #x00
		'level ; #x01
		'xp ; #x02
		(cons 'statements 'STR) ; #x03
		(cons 'statements 'DEX) ; #x04
		(cons 'statements 'CON) ; #x05
		(cons 'statements 'INT) ; #x06
		(cons 'statements 'WIT) ; #x07
		(cons 'statements 'MEN) ; #x08
		'hp ; #x09
		'max-hp ; #x0a
		'mp ; #x0b
		'max-mp ; #x0c
		'sp ; #x0d
		'load ; #x0e
		'max-load ; #x0f
		#f ; #x10
		'physical-attack-power ; #x11
		'physical-attack-speed ; #x12
		'physical-defense ; #x13
		'evasion ; #x14
		'accuracy ; #x15
		'focus ; #x16
		'magical-attack-power ; #x17
		'magical-attack-speed ; #x18
		'magical-defense ; #x19
		'pvp? ; #x1a
		'karma ; #x1b
		#f ; #x1c
		#f ; #x1d
		#f ; #x1e
		#f ; #x1f
		#f ; #x20
		'cp ; #x21
		'max-cp ; #x22
	))
	
	(define (read-attributes count port)
		(define (r d s c)
			(if (> c 0)
				(let ((k (vector-ref attribute (read-int32 #f port))) (v (read-int32 #f port)))
					(r
						(if (symbol? k) (alist-cons k v d) d)
						(if (pair? k) (alist-cons (cdr k) v s) s)
						(- c 1)
					)
				)
				(values d s)
			)
		)
		(let-values (((data statements) (r (list) (list) count)))
			(if (not (null? statements))
				(alist-cons 'statements statements data)
				data
			)
		)
	)
	
	(define (game-server-packet/status-update buffer)
		(let ((s (open-input-bytes buffer)))
			(append (list
					(cons 'id (read-byte s))
					(cons 'object-id (read-int32 #f s))
				)
				(read-attributes (read-int32 #f s) s)
			)
		)
	)
)