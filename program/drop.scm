(module ai racket/base
	(require
		(only-in racket/math pi)
		racket/undefined
		"program.scm"
		(relative-in "../."
			"library/extension.scm"
			"system/structure.scm"
			"system/connection.scm"
			"system/event.scm"
			"model/map.scm"
			"model/object.scm"
			; "model/item.scm"
			"model/creature.scm"
			"model/inventory.scm"
			"model/world.scm"
			"model/item.scm"
			"model/protagonist.scm"
			"api/drop.scm"
		)
	)
	(provide make-program-drop)

	(define (program-error message . args)
		(apply raise-program-error 'program-drop message args)
	)

	(define (drop-one cn item center radius)
		(let ((angle (/ (random (inexact->exact (floor (* pi 2 1000000)))) 1000000)) (range (random (+ radius 1))))
			(drop cn (object-id item) (ref item 'count) (map-circle-point center range angle))
		)
	)

	(define (make-program-drop [selector #f] [radius 30] [center #f] [delay 1/2])
		(make-program 'program-drop (list selector radius center delay)
			(lambda (cn ev items)
				(let ((me (world-me (connection-world cn))))
					(case-event ev
						('item-spawn (id . rest) ; Auto loot if I'm looter.
							(if (= id (object-id (car items)))
								(if (not (null? (cdr items)))
									(begin
										(timeout! #:id 'program-drop-next cn delay)
										(cdr items)
									)
									eof
								)
								items
							)
						)
						('program-drop-next ()
							(drop-one cn (car items) (or center (get-position me)) radius)
						)
						(else items)
					)
				)
			)

			#:constructor (lambda (cn)
				(let* ((wr (connection-world cn)) (me (world-me wr)))
					(let ((items (items (world-inventory wr) (or selector (lambda (item) (not (equipped? me (object-id item))))))))
						(when (null? items) (program-error "Nothing to drop."))
						(drop-one cn (car items) (or center (get-position me)) radius)
						items
					)
				)
			)
		)
	)
)
