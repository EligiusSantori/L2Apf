(module ai racket/base
	(require
		(only-in srfi/1 car+cdr fold)
		racket/undefined
		"program.scm"
		(relative-in "../."
			"library/extension.scm"
			"library/date_time.scm"
			"system/structure.scm"
			"system/connection.scm"
			"system/event.scm"
			"model/map.scm"
			"model/object.scm"
			"model/skill.scm"
			"model/item.scm"
			"model/creature.scm"
			"model/npc.scm"
			"model/world.scm"
			"api/pick_up.scm"
			"api/target.scm"
			"api/use_skill.scm"
		)
	)
	(provide program-loot)

	(define (program-error message . args)
		(apply raise-program-error 'program-loot message args)
	)

	(define (find-sweep-skill wr) ; TODO Use spoil-festival if many spoiled corpses nearby.
		(find-skill wr 'sweeper)
	)
	(define (item-fit? range center item)
		(and (on-ground? item) (or (zero? range) (<= (points-distance (ref item 'position) center) range)))
	)
	(define (npc-fit? range center npc)
		(and (ref npc 'spoiled?) (or (zero? range) (<= (points-distance (get-position npc) center) range)))
	)
	(define (find-worth wr sweep? range center except-id)
		(car+cdr (fold-objects wr (cons (list) (list)) (lambda (object r)
			(let-values (((npcs items) (car+cdr r)))
				(cond
					((eq? (object-id object) except-id) r)
					((and sweep? (npc? object) (npc-fit? range center object))
						(cons (cons object npcs) items)
					)
					((and (item? object) (item-fit? range center object))
						(cons npcs (cons object items))
					)
					(else r)
				)
			)
		)))
	)
	(define (closest objects to)
		(cdr (fold (lambda (object result)
			(let* ((p (if (creature? object) (get-position object) (ref object 'position))) (d (points-distance p to)))
				(if (or (not (car result)) (< d (car result))) (cons d object) result)
			)
		) (cons #f #f) objects))
	)
	(define (next wr me sweep? range center strategy except-id)
		(let ((position (get-position me)))
			(let-values (((npcs items) (find-worth wr sweep? range (or center position) except-id)))
				(or
					(and (not (null? npcs)) (strategy npcs position))
					(and (not (null? items)) (strategy items position))
				)
			)
		)
	)
	(define (gather cn object)
		(let ((wr (connection-world cn)) (id (object-id object)))
			(if (npc? object)
				(if (eq? (ref (world-me wr) 'target-id) id)
					(let ((skill (find-sweep-skill wr)))
						(use-skill cn skill)
						(cons id #f) ; (+ (timestamp (or (ref skill 'reuse-delay) 0)))
					)
					(begin
						(target cn id)
						(cons id #f)
					)
				)
				(begin
					(pick-up cn id)
					(cons id (+ (timestamp) 1/5))
				)
			)
		)
	)
	(define delayed-gather (gensym))

	(define-program program-loot ; TODO Optimized default strategy.
		(lambda (cn event config state)
			(let-values (((sweep? range center strategy) (list->values config)) ((gather-id will-ready) (car+cdr state)))
				(let* ((wr (connection-world cn)) (me (world-me wr)) (gather-next (lambda ([except-id #f])
						(if (or (not will-ready) (>= (timestamp) will-ready))
							(let ((object (next wr me sweep? range center strategy except-id)))
								(if object (gather cn object) eof)
							)
							(begin
								(alarm! #:id delayed-gather cn will-ready (make-event delayed-gather except-id))
								state
							)
						)
					)))
					(case-event event
						('change-target (subject-id target-id . rest)
							(if (= subject-id (object-id me))
								(cond
									((not target-id) (gather-next gather-id)) ; Corpse has been swept or target broken.
									((eq? target-id gather-id) (gather cn (object-ref wr gather-id))) ; Spoiled corpse has been selected.
									(else (program-error "Unexpected target." target-id)) ; TODO
								)
								state
							)
						)
						('skill-reused (skill)
							(if (and sweep? (= (skill-id skill) (skill-id (find-sweep-skill wr))) (eq? (ref me 'target-id) gather-id))
								(gather-next) ; Sweep now available.
								state
							)
						)
						('item-pick (id . rest) (if (eq? id gather-id) (gather-next id) state))
						('object-delete (id) (if (eq? id gather-id) (gather-next id) state))
						(delayed-gather (except-id) (gather-next except-id))
						(else state)
					)
				)
			)
		)

		#:constructor (lambda (cn config)
			(let-values (((sweep? range center strategy) (list->values config)))
				(let ((wr (connection-world cn)))
					(when (and sweep? (not (find-sweep-skill wr)))
						(program-error "Can't find sweep skill.")
					)
					(let ((object (next wr (world-me wr) sweep? range center strategy #f)))
						(if object (gather cn object) (program-error "Nothing to gather."))
					)
				)
			)
		)

		#:defaults (list
			#t ; sweep?
			0 ; range
			#f ; center
			closest ; strategy
		)
	)
)
