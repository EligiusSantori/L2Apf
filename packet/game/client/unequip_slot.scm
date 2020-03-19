; l2j/gameserver/clientpackets/RequestUnEquipItem.java
(module system racket/base
	(require
		"../../../library/extension.scm"
		"../../packet.scm"
	)
	(provide game-client-packet/unequip-slot)

	(define paperdoll (list
		(cons 'underwear #x01)
		(cons 'right-ear #x02)
		(cons 'left-ear #x04)
		(cons 'neck #x08)
		(cons 'right-finger #x10)
		(cons 'left-finger #x20)
		(cons 'head #x40)
		(cons 'right-hand #x80)
		(cons 'left-hand #x100)
		(cons 'gloves #x200)
		(cons 'chest #x400)
		(cons 'legs #x800)
		(cons 'feet #x1000)
		(cons 'shoulders #x2000)
		(cons 'both-hands #x4000)
		(cons 'full-body #x8000)
		(cons 'hair #x10000)
		(cons 'wolf #x20000)
		(cons 'dragon #x40000)
		(cons 'strider #x80000)
	))

	(define (game-client-packet/unequip-slot slot)
		(let ((s (open-output-bytes)))
			(begin
				(write-byte #x11 s)
				(write-int32 (assoc slot paperdoll eq?) #f s)
				(get-output-bytes s)
			)
		)
	)
)
