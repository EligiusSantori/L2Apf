(module system racket/base
	(provide login-server-packet/server-list)
	(require "../../packet.scm")
	
	(define (r s c l)
		(if (> c 0)
			(let ((i (list
				(cons 'id (read-byte s))
				(cons 'address (format "~s.~s.~s.~s"
					(read-byte s)
					(read-byte s)
					(read-byte s)
					(read-byte s)
				))
				(cons 'port (read-int32 #f s))
				(cons 'age (read-byte s))
				(cons 'pvp (> (read-byte s) 0))
				(cons 'online (read-int16 #f s))
				(cons 'maximum (read-int16 #f s))
				(cons 'state (> (read-byte s) 0))
			)))
				(begin
					(read-bytes (+ 4 1) s)
					(r s (- c 1) (cons i l))
				)
			)
			l
		)
	)
	
	(define (login-server-packet/server-list buffer)
		(let ((s (open-input-bytes buffer)))
			(let ((id (read-byte s)) (count (read-byte s)) (last (read-byte s)))
				(list
					(cons 'id id)
					(cons 'count count)
					(cons 'last last)
					(cons 'list (reverse (r s count (list))))
				)
			)
		)
	)
)