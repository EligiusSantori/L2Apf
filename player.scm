#lang racket
(require
	srfi/1
	"bootstrap.scm"
	"library/extension.scm"
	; "library/structure.scm"
	; "library/geometry.scm"
	; "model/object.scm"
	; "model/creature.scm"
	; "model/npc.scm"
	; "model/character.scm"
	; "model/antagonist.scm"
	; "model/protagonist.scm"
	; "model/skill.scm"
	(only-in "ai/program/program.scm" ai-program-make)
	"ai/program/idle.scm"
	"ai/program/print.scm"
	"ai/program/command.scm"
	; "ai/program/auto_reborn.scm"
	; "ai/program/pickup.scm"
	; "ai/program/escape.scm"
	; "ai/program/travel.scm"
	; "ai/program/bless.scm"
	"ai/manager.scm"
)

(let-values (((connection world me events) (call/wv parse-protocol bootstrap)))
	(define manager (make-ai-manager ai-program-idle))
	(ai-manager-load! manager
		ai-program-print
		(ai-program-make ai-program-command manager)
	)

	(let loop ()
		(let ((event (sync events)))
			; Triggers space.
			(case (if event (car event) #f)
				; Custom events.

				; Standard events.
				((logout)
					(exit)
				)
			)
			; Programs space.
			(ai-manager-run! manager event connection)
		)
		(loop)
	)
)
