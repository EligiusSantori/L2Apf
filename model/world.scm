(module logic racket/base
	(require
		srfi/1
		racket/set
		racket/math
		(rename-in racket/contract (any all/c))
		(relative-in "../."
			"library/extension.scm"
			"library/geometry.scm"
			"system/structure.scm"
		)
		"object.scm"
		"creature.scm"
		"npc.scm"
		"character.scm"
		"protagonist.scm"
		"item.scm"
		"skill.scm"
	)
	(provide
		(struct-out world)
		(contract-out
			(make-world ((listof pair?) . -> . world?))
			(register-object! (world? object? . -> . void?))
			(discard-object! (world? integer? . -> . void?))
			(objects (->* (world?) (procedure?) list?))
			(object-ref (world? (or/c integer? false/c) . -> . (or/c object? false/c)))
			(skill-ref (world? (or/c integer? false/c) . -> . (or/c skill? false/c)))
			(find-character-by-name (world? string? . -> . (or/c character? false/c))) ; FIXME rename
			(get-target (world? creature? . -> . (or/c creature? false/c))) ; FIXME move to creature?
			(get-level (creature? . -> .  (or/c integer? false/c))) ; FIXME move to creature
			(attackable? (any/c . -> . boolean?)) ; FIXME move to creature
			(aimed-to? (creature? creature? . -> . boolean?)) ; FIXME move to creature
			(behind? (->* (creature? creature?) (rational?) boolean?)) ; FIXME move to map
			(alive? (creature? . -> . boolean?)) ; FIXME
		)
	)

	(struct world (
		server-id
		server-host
		server-port
		objects
		me
		inventory
		shortcuts
		skills
		quests
		party
		clans
		alliances
	) #:mutable)

	(define (make-world server)
		(world
			(ref server 'id)
			(ref server 'address)
			(ref server 'port)
			(make-hash)
			#f
			(mutable-set)
			(make-vector (* 12 10))
			(make-hash)
			(make-hash)
			(list)
			(make-hash)
			(make-hash)
		)
	)

	(define (register-object! wr object)
		(let ((object-id (ref object 'object-id)))
			(hash-set! (world-objects wr) object-id object)
			; TODO set 'characters name object-id
			(void)
		)
	)
	(define (discard-object! wr object-id)
		(if (hash-has-key? (world-objects wr) object-id)
			(hash-remove! (world-objects wr) object-id)
			(void)
		)
	)

	(define (objects wr [predicate always?])
		(map cdr (hash-filter (world-objects wr) (lambda (k v)
			(and (integer? k) (predicate v))
		)))
	)

	(define (object-ref wr object-id)
		(hash-ref (world-objects wr) object-id #f)
	)

	(define (skill-ref wr skill-id)
		(hash-ref (world-skills world) skill-id #f)
	)

	;(define (inventory-ref world object-id)

	;)

	(define (find-character-by-name wr name)
		(define (test? k v)
			(and (integer? k) (character? v) (string-ci=? (@: v 'name) name))
		)

		(let ((found (hash-find (world-objects wr) test?)))
			(if found (cdr found) #f)
		)
	)

	(define (attackable? creature)
		(and
			creature
			(creature? creature)
			(alive? creature)
			(or
				(character? creature)
				(and
					(npc? creature)
					(ref creature 'attackable?)
				)
			)
		)
	)

	(define (aimed-to? subject object)
		(and
			(creature? subject)
			(object? object)
			(equal? (ref subject 'target-id) (ref object 'object-id))
		)
	)

	(define (behind? subject object [limit pi/4]) ; Проверка нахождения объекта сзади с погрешностью в обе стороны на 45/2 градусов
		(let ((sp (get-position subject)) (op (get-position object)))
			(in-sector?
				(point/3d->point/2d op)
				(point/3d->point/2d sp)
				(- (* 3/2 pi) (limit / 2))
				limit
			)
		)
	)

	(define (get-target wr creature)
		(object-ref world (ref creature 'target-id))
	)

	(define (get-level creature)
		(or
			(and (character? creature)
				(ref creature 'level)
			)
			(and (npc? creature)
				(let ((match (regexp-match (pregexp "(?i:Lv)\\s*(\\d+)") (or (@: creature 'title) ""))))
					(and match (string->number (last match)))
				)
			)
		)
	)

	(define (alive? creature)
		(if (protagonist? creature)
			(not (ref creature 'died?))
			(not (ref creature 'alike-dead?))
		)
	)
)
