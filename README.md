# L2Apf
Lineage 2 C4 Artificial Player / Framework (alpha).


## Requirements
* Racket language (version 6 or newer).
	* Packages: srfi-lite-lib, r6rs-lib.
* L2J Chronicle 4 server (660 protocol version).

## Examples
Run script for solo player:  
`racket -O 'warning@l2apf' player.scm l2apf://login:password@host:port/player`.  

Run entire realm of players:  
`racket realm.scm config.yaml party.a fifth`.

###### player.scm
```scheme
#lang racket
(require
	srfi/1
	racket/logging
	"library/extension.scm"
	"system/event.scm"
	(only-in "program/program.scm" program-make)
	"program/brain.scm"
	"program/idle.scm"
	"program/print.scm"
	"bootstrap.scm"
)

(let-values (((connection world me events) (call/wv parse-protocol bootstrap)))
	(define brain (make-brain program-idle))
	(brain-load! brain
		program-print
		; (program-make program-command brain)
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

```

###### config.yaml
```yaml
password: "123456"
party:
  a: [first, second, third]
  b: [fourth, fifth ,sixth, seventh, eighth]

```

###### realm.scm
```scheme
#lang racket
TODO
```
