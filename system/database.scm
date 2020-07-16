(module system racket/base
	(require
		racket/string
		racket/contract
		db/base
		"../library/extension.scm"
	)
	(provide (contract-out
		(db-level (-> connection? integer? rational?))
		(db-class (-> connection? integer? symbol?))
		(db-item (-> connection? integer? list?))
		(db-skill (-> connection? integer? integer? list?))
		(db-character (-> connection? integer? list?))
		(db-npc (-> connection? integer? list?))
	))

	(define (db-level db exp)
		(if db
			(let ((rows (query-rows db "SELECT level, value FROM experience WHERE value >= $1 LIMIT 2" exp)))
				(let ((level (vector-ref (car rows) 0)) (base (vector-ref (car rows) 1)) (next (vector-ref (list-try-ref rows 1 (vector #f #f)) 1)))
					(if next (/ (floor (* (+ level (/ (- exp base) (- next base))) 100)) 100) level)
				)
			)
			#f
		)
	)
	(define (db-class db class-id)
		(if db (string->symbol (query-value db "SELECT name FROM class WHERE id = $1" class-id)) #f)
	)

	(define (db-item db item-id)
		(if db
			(let ((row (query-maybe-row db "SELECT name, type, grade, stack FROM item WHERE id = $1" item-id)))
				(if row
					(list
						(cons 'name (vector-ref row 0))
						(cons 'item-type (case (vector-ref row 1)
							; ((-1) 'quest)
							((1) 'potion) ((2) 'scroll) ((3) 'recipe)
							((50) 'enchant/weapon) ((51) 'enchant/armor)
							((100) 'sword) ((101) 'blunt) ((102) 'dagger) ((103) 'spear) ((104) 'bow) ((105) 'duals) ((106) 'fists) ((107) 'wand)
							((150) 'arrow)
							((200) 'helmet) ((201) 'gloves) ((202) 'boots) ((203) 'hair)
							((210) 'full/heavy) ((211) 'body/heavy) ((212) 'legs/heavy)
							((220) 'full/light) ((221) 'body/light) ((222) 'legs/light)
							((230) 'full/robe) ((231) 'body/robe) ((232) 'legs/robe)
							((300) 'shield)
							((400) 'necklace) ((401) 'earring) ((402) 'ring)
							((500) 'wolf)
							((510) 'dragon) ((511) 'strider) ((512) 'wyvern) ((520) 'kookaburra) ((530) 'buffalo) ((540) 'cougar) ((550) 'redeemer)
							(else 'other)
						))
						(cons 'grade (vector-ref row 2))
						(cons 'stack (vector-ref row 3))
					)
					(list)
				)
			)
			(list)
		)
	)

	(define (db-skill db id level)
		(if db
			(let ((row (query-maybe-row db "SELECT name, type, type = 0 and harmful <> 0, mp_cost, hp_cost, item_id, item_cost FROM skill WHERE id = $1 AND level = $2" id level)))
				(if row
					(filter pair? (list
						(cons 'name (vector-ref row 0))
						(and (= (vector-ref row 1) 1) (cons 'toggle? #t))
						(and (not (zero? (vector-ref row 2))) (cons 'harmful? #t))
						(let ((v (vector-ref row 3))) (and (not (sql-null? v)) (cons 'mp-cost v)))
						(let ((v (vector-ref row 4))) (and (not (sql-null? v)) (cons 'hp-cost v)))
						(let ((v (vector-ref row 5))) (and (not (sql-null? v)) (cons 'item-id v)))
						(let ((v (vector-ref row 6))) (and (not (sql-null? v)) (cons 'item-cost v)))
					))
					(list)
				)
			)
			(list)
		)
	)

	(define (db-character db class-id)
		(if db
			(let ((row (query-maybe-row db "SELECT name, grade, mystic FROM class WHERE id = $1" class-id)))
				(if row
					(list
						(cons 'class (string->symbol (vector-ref row 0)))
						(cons 'specialty (vector-ref row 1))
						(cons 'mystic? (not (zero? (vector-ref row 2))))
					)
					(list)
				)
			)
			(list)
		)
	)

	(define (db-npc db npc-id)
		(if db
			(let ((row (query-maybe-row db "SELECT name, level, type, aggro, clan FROM npc WHERE id = $1" npc-id)))
				(if row
					(filter pair? (list
						(cons 'name (vector-ref row 0))
						(cons 'level (vector-ref row 1))
						(cons 'npc-type (case (vector-ref row 2)
							; ((-1) 'static)
							((0) 'person)
							((1) 'monster) ; TODO bug, beast, ...
							((2) 'minion)
							((3) 'boss)
							(else 'static)
						))
						(cons 'aggro (sql-null->false (vector-ref row 3)))
						(cons 'groups (let ((s (vector-ref row 4)))
							(if (sql-null? s) (list) (string-split s ","))
						))
					))
					(list)
				)
			)
			(list)
		)
	)

	; Geo data.
	#|(define (ground-z x y)
		(void) ; => integer?
	)
	(define (floor-z x y)
		(void) ; => integer?
	)
	(define (geo-random-in-cricle center radius)
		(void) ; => point/3d?
	)
	(define (geo-random-in-rectangle corner1 corner2)
		(void) ; => point/3d?
	)|#
)
