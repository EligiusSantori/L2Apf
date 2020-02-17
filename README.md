# L2Apf
Lineage 2 C4 artificial player / framework (alpha).

You can implement desired behavior right inside *entiry script* or use higher-level *programs interface* or connect actions/models/events to your *neural network*.

### Programs
Program is a potentially reusable algorithm that can be instantiated with *parameters* and shares *state* between iterations.

Iteration can be caused by a *server event*, *custom event* or *timer event*.

Program can be finite or not. Can be foreground or background. Only one foreground program can handle an iteration event but previous programs can be stacked on load.

## Requirements
* Racket language (version 6 or newer).
	* Packages: srfi-lite-lib, r6rs-lib.
* L2J Chronicle 4 server (660 protocol version).
	* Link: TODO

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
	"system/connection.scm"
	"system/event.scm"
	(only-in "program/program.scm" program)
	"program/brain.scm"
	"program/idle.scm"
	"program/print.scm"
	"program/command.scm"
	"bootstrap.scm"
)

(apply bootstrap (lambda (connection world me events)
	(define br (make-brain program-idle))
	(brain-load! br
		(program program-print)
		(program program-command br)
	)

	(do ((event (sync events) (sync events))) ((eq? (car event) 'disconnect))
		; Triggers space.

		(case (car event)
			; Custom events.

			; Standard events.
		)
		; Programs space.
		(brain-run! br event connection)
	)
) (values->list (parse-protocol)))
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

\* *Some pieces of code may be outdated or not fully implemented but I sustain operability of core and basic flow.*

## Extension
You can implement missed packets/actions/events or update the project to higher server version with ease.
You can even port this architecture to another game.

### How to write a program
Start with:
```scheme
(define-program my-program (lambda (event connection config state)
	; ...
))
```

Full syntax:
```scheme
(define-program my-program
	(lambda (event connection config state) ; Program iterator.
		; ...
	)
	#:constructor (lambda (config) ; On load callback.
		; ...
	)
	#:destructor (lambda (config state) ; On unload callback.
		; ...
	)
	#:defaults (list ; Default config.
		(cons 'name 'value)
		; ...
	)
)
```

---

For non-commertial use. In case of public use please indicate URL of original repo (https://github.com/EligiusSantori/L2Apf).

