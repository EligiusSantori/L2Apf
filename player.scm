#lang racket
(require
	srfi/1
	racket/logging
	"bootstrap.scm"
	"library/extension.scm"
	; "system/structure.scm"
	; "library/geometry.scm"
	; "model/object.scm"
	; "model/creature.scm"
	; "model/npc.scm"
	; "model/character.scm"
	; "model/antagonist.scm"
	; "model/protagonist.scm"
	; "model/skill.scm"
	(only-in "program/program.scm" program-make)
	"program/brain.scm"
	"program/idle.scm"
	"program/print.scm"
	"program/command.scm"
	; "program/auto_reborn.scm"
	; "program/pickup.scm"
	; "program/escape.scm"
	; "program/travel.scm"
	; "program/bless.scm"
)

(with-logging-to-port (open-output-file "l2apf.log" #:mode 'text #:exists 'append)
	(let-values (((connection world me events) (call/wv parse-protocol bootstrap)))
		(define brain (make-brain program-idle))
		(brain-load! brain
			program-print
			(program-make program-command brain)
		)

		(let loop ()
			(let ((event (sync events)))
				; Triggers space.
				(case (car event)
					; Custom events.

					; Standard events.
					((logout)
						(exit)
					)
				)
				; Programs space.
				(brain-run! brain event connection)
			)
			(loop)
		)
	)
)
