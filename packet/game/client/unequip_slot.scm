; l2j/gameserver/clientpackets/RequestUnEquipItem.java
(module system racket/base
	(require
		"../../../library/extension.scm"
		"../../packet.scm"
	)
	(provide game-client-packet/unequip-slot)

	(define paperdoll (list
		(cons #x01 'underwear)
		(cons #x02 'right-ear)
		(cons #x04 'left-ear)
		(cons #x08 'neck)
		(cons #x10 'right-finger)
		(cons #x20 'left-finger)
		(cons #x40 'head)
		(cons #x80 'right-hand)
		(cons #x100 'left-hand)
		(cons #x200 'gloves)
		(cons #x400 'chest)
		(cons #x800 'legs)
		(cons #x1000 'feet)
		(cons #x2000 'shoulders)
		(cons #x4000 'both-hands)
		(cons #x8000 'full-body)
		(cons #x10000 'hair)
		(cons #x20000 'wolf)
		(cons #x40000 'dragon)
		(cons #x80000 'strider)
	))

	(define (game-client-packet/unequip-slot slot)
		(let ((s (open-output-bytes)))
			(begin
				(write-byte #x11 s)
				(write-int32 (assoc slot (alist-flip paperdoll) eq?) #f s)
				(get-output-bytes s)
			)
		)
	)
)
