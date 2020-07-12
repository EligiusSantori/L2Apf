(module system racket/base
	(require
		racket/contract
		db/base
		"../library/extension.scm"
	)
	(provide (contract-out
		(db-level (-> connection? integer? rational?))
		(db-class (-> connection? integer? symbol?))
		(db-item (-> connection? integer? list?))
		(db-skill (-> connection? integer? list?))
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
			(let ((row (query-maybe-row db "SELECT name, type, grade FROM item WHERE id = $1" item-id)))
				(if row
					(list
						(cons 'name (vector-ref row 0))
						(cons 'item-type (string->symbol (vector-ref row 1)))
						(cons 'grade (list-ref (list 'ng 'd 'c 'b 'a 's) (vector-ref row 0)))
					)
					(list)
				)
			)
			(list)
		)
	)

	(define (db-skill db skill-id [level 1])
		(if db
			(let ((row (query-maybe-row db "SELECT name, type, harmful, mp_cost, hp_cost, item_id, item_cost FROM skill WHERE id = $1 AND level = $2" skill-id level)))
				(if row
					(list
						(cons 'name (vector-ref row 0))
						(cons 'toggle? (= (vector-ref row 1) 1))
						(cons 'harmful? (if (zero? (vector-ref row 2)) #f #t))
						(cons 'mp-cost (sql-null->false (vector-ref row 3)))
						(cons 'hp-cost (sql-null->false (vector-ref row 4)))
						(cons 'item-id (sql-null->false (vector-ref row 5)))
						(cons 'item-cost (sql-null->false (vector-ref row 6)))
					)
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
						(cons 'mystic? (if (zero? (vector-ref row 2)) #f #t))
					)
					(list)
				)
			)
			(list)
		)
	)

	(define (db-npc db npc-id)
		(if db
			(let ((row (query-maybe-row db "SELECT name, level, aggressive, CASE WHEN type = 1 THEN 1 ELSE 0 END AS monster, CASE WHEN type = 2 THEN 1 ELSE 0 END AS minion, CASE WHEN type = 3 THEN 1 ELSE 0 END AS boss, clan FROM npc WHERE id = $1" npc-id)))
				(if row
					(list
						(cons 'name (vector-ref row 0))
						(cons 'level (vector-ref row 1))
						(cons 'aggressive? (if (zero? (vector-ref row 2)) #f #t))
						(cons 'monster? (if (zero? (vector-ref row 3)) #f #t))
						(cons 'minion? (if (zero? (vector-ref row 4)) #f #t))
						(cons 'boss? (if (zero? (vector-ref row 5)) #f #t))
						(cons 'group (sql-null->false (vector-ref row 6)))
					)
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
