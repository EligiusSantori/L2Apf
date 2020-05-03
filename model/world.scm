(module logic racket/base
	(require
		(except-in srfi/1 any)
		racket/set
		racket/math
		racket/contract
		(relative-in "../."
			"library/extension.scm"
			"library/geometry.scm"
			"system/structure.scm"
		)
		"map.scm"
		"object.scm"
		"creature.scm"
		"npc.scm"
		"character.scm"
		"protagonist.scm"
		"item.scm"
		"skill.scm"
		"party.scm"
	)
	(provide
		(struct-out world)
		(contract-out
			(make-world (-> (listof pair?) world?))
			(register-object! (-> world? (and/c box? object?) void?))
			(discard-object! (-> world? integer? void?))
			(objects (->* (world?) (procedure?) list?))
			(fold-objects (-> world? any/c procedure? any))
			(near (-> world? point/3d? integer? list?))
			(towards (-> world? point/3d? integer? list?))
			(object-ref (-> world? (or/c integer? false/c) (or/c object? false/c)))
			(skill-ref (-> world? (or/c integer? false/c) (or/c skill? false/c)))
			(inv-ref (-> world? (or/c integer? false/c) (or/c item? false/c)))
			(equipped (-> world? item? (or/c symbol? false/c)))
			(find-character-by-name (world? string? . -> . (or/c character? false/c))) ; FIXME rename
			(get-target (world? creature? . -> . (or/c creature? false/c))) ; FIXME move to creature?
			(get-level (creature? . -> .  (or/c integer? false/c))) ; FIXME move to creature
			(get-angle (world? creature? . -> . rational?))
			(get-destination (-> world? creature? (or/c point/3d? false/c)))
			(attackable? (any/c . -> . boolean?)) ; FIXME move to creature
			(aimed-to? (creature? creature? . -> . boolean?)) ; FIXME move to creature
			(behind? (->* (creature? creature?) (rational?) boolean?)) ; FIXME move to map
			(alive? (creature? . -> . boolean?)) ; FIXME
			(party-add! (world? integer? . -> . void?))
			(party-kick! (world? integer? . -> . void?))
			(party-leader! (world? integer? . -> . void?))
			(party-clear! (world? . -> . void?))
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
			(make-hash)
			(make-vector (* 12 10))
			(make-hash)
			(make-hash)
			#f
			(make-hash)
			(make-hash)
		)
	)

	(define (register-object! wr object)
		(let ((object-id (ref object 'object-id)))
			(hash-set! (world-objects wr) object-id object)
			; TODO world-characters[name] = object-id
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
		(define l (list))
		(hash-for-each (world-objects wr) (lambda (k v)
			(when (predicate v)
				(set! l (cons v l))
			)
		))
		l
	)
	(define (fold-objects wr init proc)
		(define r init)
		(hash-for-each (world-objects wr) (lambda (k v)
			(set! r (proc v r))
		))
		r
	)
	(define (near wr position radius)
		(objects wr (lambda (object)
			(let ((p (cond ((creature? object) (get-position object)) ((item? object) (ref object 'position)) (else #f))))
				(and p (<= (points-distance p position) radius))
			)
		))
	)
	(define (towards wr destination radius)
		(objects wr (lambda (object)
			(let ((d (if (creature? object) (ref object 'destination) #f)))
				(and d (<= (points-distance d destination) radius))
			)
		))
	)

	(define (object-ref wr object-id)
		(hash-ref (world-objects wr) object-id #f)
	)
	(define (skill-ref wr skill-id)
		(hash-ref (world-skills world) skill-id #f)
	)
	(define (inv-ref world object-id)
		(hash-ref (world-inventory world) object-id #f)
	)

	(define (equipped wr item)
		(let ((object-id (ref item 'object-id)))
			(fold (lambda (c p)
				(if (eq? object-id (cdr c)) (car c) p)
			) #f (ref (world-me wr) 'equipment))
		)
	)

	(define (find-character-by-name wr name)
		(define (test? k v)
			(and (integer? k) (character? v) (string-ci=? (ref v 'name) name))
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
		(object-ref wr (ref creature 'target-id))
	)

	(define (get-level creature)
		(or
			(and (character? creature)
				(ref creature 'level)
			)
			(and (npc? creature)
				(let ((match (regexp-match (pregexp "(?i:Lv)\\s*(\\d+)") (or (ref creature 'title) ""))))
					(and match (string->number (last match)))
				)
			)
		)
	)

	(define (get-angle wr creature)
		(if (casting? creature)
			(let ((target (get-target wr creature)))
				(or (and target (creatures-angle creature target)) (ref creature 'angle))
			)
			(ref creature 'angle)
		)
	)

	(define (get-destination wr creature)
		(let ((dot (ref creature 'destination)))
			(if (integer? dot)
				(ref (object-ref wr dot) 'position) ; TODO get-position & protect from inf-loop.
				dot
			)
		)
	)

	(define (alive? creature)
		(if (protagonist? creature)
			(not (ref creature 'died?))
			(not (ref creature 'alike-dead?))
		)
	)

	(define (party-add! wr object-id)
		(let ((party (world-party wr)))
			(set-world-party! wr (make-party
				(party-loot party)
				(party-leader party)
				(cons object-id (cdr (party-members party)))
			))
		)
	)
	(define (party-clear! wr)
		(set-world-party! wr #f)
	)
	(define (party-kick! wr object-id)
		(let* ((party (world-party wr)) (left (remove (lambda (id) (= id object-id)) (party-members party))))
			(if (null? left)
				(party-clear! wr)
				(set-world-party! wr (apply make-party (party-loot party) left))
			)
		)
	)
	(define (party-leader! wr object-id)
		(let ((party (world-party wr)))
			(set-world-party! wr (make-party
				(party-loot party)
				object-id
				(cdr (party-members party))
			))
		)
	)
)
