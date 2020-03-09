; l2j/gameserver/serverpackets/PartySmallWindowAll.java
(module system racket/base
	(require
		"../../../library/extension.scm"
		"../../packet.scm"
		"../party_loot.scm"
	)
	(provide game-server-packet/party-all-members)

	(define (read-member s)
		(list
			(cons 'object-id (read-int32 #f s))
			(cons 'name (read-utf16 s))
			(cons 'cp (read-int32 #f s))
			(cons 'max-cp (read-int32 #f s))
			(cons 'hp (read-int32 #f s))
			(cons 'max-hp (read-int32 #f s))
			(cons 'mp (read-int32 #f s))
			(cons 'max-mp (read-int32 #f s))
			(cons 'level (read-int32 #f s))
			(cons 'class-id (let ((class-id (read-int32 #f s)))
				(read-int32 #f s) ; skip
				(read-int32 #f s) ; skip
				class-id
			))
		)
	)

	(define (read-members s c n l)
		(if (< n c)
			(let ((i (read-member s)))
				(read-members s c (+ n 1) (cons i l))
			)
			l
		)
	)

	(define (game-server-packet/party-all-members buffer)
		(let ((s (open-input-bytes buffer)))
			(list
				(cons 'id (read-byte s))
				(cons 'leader-id (read-int32 #f s))
				(cons 'loot-mode (alist-ref loot-types (read-int32 #f s) #f =))
				(cons 'members (read-members s (read-int32 #f s) 0 (list)))

			)
		)
	)
)
