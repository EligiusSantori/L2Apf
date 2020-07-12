(module system racket/base
	(require
		(only-in srfi/1 fold)
		racket/string
		racket/set
		racket/contract
		"../library/extension.scm"
		"../library/cache.scm"
		"../model/item.scm"
		"../model/skill.scm"
		"../model/npc.scm"
		"../model/character.scm"
		"../model/world.scm"
		"structure.scm"
		"connection.scm"
		"../program/program.scm"
	)
	(provide (contract-out
		(apf-print-handler (-> any/c output-port? any))
	))

	(define (apf-print-handler value port)
		(cond
			((bytes? value) ; Custom (hex) printer for byte string.
				(display "[" port)
				(display (string-join (map byte->hex (bytes->list value)) " ") port)
				(display "]" port)
			)
			((cache-set? value)
				(display "#<cache-set:" port)
				(fold (lambda (sv p)
					(display " " port)
					(display sv port)
				) #f (set->list (cache-set-all value)))
				(display ">" port)
			)
			((connection? value)
				(display "#<connection:" port)
				(display (connection-protocol value) port)
				(display ":" port)
				(display (connection-account value) port)
				(display ">" port)
			)
			((world? value)
				(display "#<world:[" port)
				(display (hash-count (world-objects value)) port)
				(display "]" port)
				(display (ref (world-me value) 'name) port)
				(display ">" port)
			)
			((program? value)
				(display "#<program:" port)
				(display (program-id value) port)
				(display ">" port)
			)
			((character? value)
				(display "#<character:" port)
				(display (ref value 'name) port)
				(display ">" port)
			)
			((npc? value)
				(display "#<npc:[" port)
				(display (ref value 'npc-id) port)
				(display "]" port)
				(display (ref value 'object-id) port)
				(display ">" port)
			)
			((item? value)
				(display "#<item:[" port)
				(display (ref value 'item-id) port)
				(display "×" port)
				(display (ref value 'count) port)
				(display "]" port)
				(display (ref value 'object-id) port)
				(display ">" port)
			)
			((skill? value)
				(display "#<skill:" port)
				(display (ref value 'id) port)
				(display "[" port)
				(display (ref value 'level) port)
				(display "]>" port)
			)
			(else (write value port))
		)
	)
)
