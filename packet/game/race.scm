(module system racket/base
	(require racket/contract)
	(provide races)

	(define races (list
		(cons 0 'human)
		(cons 1 'light-elf)
		(cons 2 'dark-elf)
		(cons 3 'orc)
		(cons 4 'dwarf)
	))
)
