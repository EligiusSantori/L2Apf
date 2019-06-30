# L2Apf
Lineage 2 C4 Artificial Player / Framework (alpha).


## Requirements
* Racket language (version 6 or newer).
* L2J Chronicle 4 server (660 protocol version).

## Examples
Run script for solo player:  
`Racket player.scm l2apf://login:password@host:port/player`.  

Run entire realm of players:  
`Racket realm.scm config.yaml party.a fifth`.

###### player.scm
```
#lang racket
(require
	; srfi/1
	"bootstrap.scm"
	; "library/structure.scm"
	; "model/object.scm"
	; "api/say.scm"
)

(bootstrap (lambda (connection world me events)
	(ai-manager-load! manager
		ai-program-print
		(ai-program-make ai-program-command manager)
	)

	(let loop ()
		(let ((event (sync events)))
			(case (if event (car event) #f)
				; Custom events.

				; Standard events.
				((logout)
					(exit)
				)
			)
		)
		(loop)
	)
))
```

###### config.yaml
```
password: "123456"
party:
  a: [first, second, third]
  b: [fourth, fifth ,sixth, seventh, eighth]

```

###### realm.scm
```
#lang racket
TODO
```
