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
			(near (->* (world? point/3d? integer?) (procedure?) list?))
			; (towards (-> world? point/3d? integer? list?))
			(object-ref (-> world? (or/c integer? false/c) (or/c object? false/c)))
			(skill-ref (-> world? (or/c integer? false/c) (or/c skill? false/c)))
			(inv-ref (-> world? (or/c integer? false/c) (or/c item? false/c)))
			(find-character (-> world? string? (or/c character? false/c)))
			(find-skill (-> world? (or/c symbol? string?) (or/c skill? false/c)))
			(get-target (-> world? creature? (or/c creature? false/c))) ; FIXME move to creature?
			(get-angle (-> world? creature? rational?))
			(get-destination (-> world? creature? (or/c point/3d? false/c)))
			(attackable? (-> any/c boolean?)) ; FIXME move to creature
			(aimed-to? (-> creature? creature? boolean?)) ; FIXME move to creature
			(behind? (->* (creature? creature?) (rational?) boolean?)) ; FIXME move to map
			(alive? (-> creature? boolean?)) ; FIXME
			(party-add! (-> world? integer? void?))
			(party-kick! (-> world? integer? void?))
			(party-leader! (-> world? integer? void?))
			(party-clear! (-> world? void?))
			(get-weapon (-> world? (or/c item? false/c)))
			(get-shield (-> world? (or/c item? false/c)))
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

	(define (objects wr [predicate #f])
		(define l (list))
		(if predicate
			(begin
				(hash-for-each (world-objects wr) (lambda (k v)
					(when (predicate v)
						(set! l (cons v l))
					)
				))
				l
			)
			(hash-values (world-objects wr))
		)
	)
	(define (fold-objects wr init proc)
		(define r init)
		(hash-for-each (world-objects wr) (lambda (k v)
			(set! r (proc v r))
		))
		r
	)
	(define (near wr position radius [predicate #f])
		(objects wr (lambda (object)
			(let ((p (cond ((creature? object) (get-position object)) ((item? object) (ref object 'position)) (else #f))))
				(and p (<= (points-distance p position) radius) (or (not predicate) (predicate object)))
			)
		))
	)
	; (define (towards wr destination radius)
	; 	(objects wr (lambda (object)
	; 		(let ((d (if (creature? object) (ref object 'destination) #f)))
	; 			(and d (<= (points-distance d destination) radius))
	; 		)
	; 	))
	; )

	(define (object-ref wr object-id)
		(hash-ref (world-objects wr) object-id #f)
	)
	(define (skill-ref wr skill-id)
		(hash-ref (world-skills wr) skill-id #f)
	)
	(define (inv-ref wr object-id)
		(hash-ref (world-inventory wr) object-id #f)
	)

	(define (find-character wr name)
		(define (test? k v)
			(and (character? v) (string-ci=? (ref v 'name) name))
		)

		(let ((found (hash-find (world-objects wr) test?)))
			(if found (cdr found) #f)
		)
	)
	(define (find-skill wr name)
		(let ((fid (if (symbol? name) (symbol->string name) (string-id name))))
			(fold-skills (world-skills wr) #f (lambda (skill r)
				(or r (and (string=? (string-id (or (ref skill 'name) "")) fid) skill))
			))
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

	(define (get-angle wr creature)
		(or (and
			(casting? creature)
			(let ((target (get-target wr creature)))
				(and
					target
					(not (object=? creature target))
					(creatures-angle creature target)
				)
			)
		) (ref creature 'angle) 0)
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
			(not (ref creature 'dead?))
			(not (ref creature 'alike-dead?))
		)
	)

	(define (party-add! wr object-id)
		(let ((party (world-party wr)))
			(set-world-party! wr (apply make-party
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
		(let* ((party (world-party wr)) (left (list-except (party-members party) = object-id)))
			(if (null? left)
				(party-clear! wr)
				(set-world-party! wr (apply make-party (party-loot party) left))
			)
		)
	)
	(define (party-leader! wr object-id)
		(let ((party (world-party wr)))
			(set-world-party! wr (apply make-party
				(party-loot party)
				object-id
				(cdr (party-members party))
			))
		)
	)

	(define (get-weapon wr)
		(let ((item (inv-ref wr (ref (world-me wr) 'equipment 'right-hand))))
			(and item (weapon? item) item)
		)
	)
	(define (get-shield wr)
		(let ((item (inv-ref wr (ref (world-me wr) 'equipment 'left-hand))))
			(and item (shield? item) item)
		)
	)
)
