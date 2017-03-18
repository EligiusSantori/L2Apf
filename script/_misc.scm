(module script racket/base
	(require
		srfi/1
		racket/string
		(relative-in "../.."
			"system/uri_scheme.scm"
			"library/extension.scm"
			"library/structure.scm"
			"library/geometry.scm"
			"api/move_to.scm"
		)
	)
	(provide
		parse-protocol
		parse-command
		format-chat-message
		make-traveller
	)
	
	(define (parse-protocol command-line)
		(if (> (vector-length command-line) 0)
			(let ((uri (parse-uri (vector-ref command-line 0))))
				(if uri
					(apply values uri)
					(error "Authentication failed because URI is broken")
				)
			)
			(error "Authentication failed because URI is missed")
		)
	)

	(define (parse-command text)
		(let ((t (string-trim (string-downcase text))))
			(if (string-starts? t "/")
				(let ((l (map string-trim (string-split t " "))))
					(cons (substring (car l) 1) (cdr l))
				)
				#f
			)
		)
	)

	(define (format-chat-message object-id channel author text)
		(let ((channel (string-titlecase (last (string-split (symbol->string channel) "/")))))
			(string-append "[" channel "] " author ": " text)
		)
	)
	
	; TODO (travel route) : list -> int
	(define (make-traveller connection)
		(define places (list
			(cons "town/human" (point/3d -84261 243396 -3734))
			(cons "town/dwarf" (point/3d 115079 -178160 -891))
			(cons "town/orc" (point/3d -44768 -112493 -230))
			(cons "town/elf" (point/3d 46903 51417 -2972))
			(cons "town/dark-elf" (point/3d 9681 15594 -4569))
			
			(cons "town/floran" (point/3d 17611 170025 -3505))
			(cons "town/gludin" (point/3d -80845 149778 -3038))
			(cons "town/gludio" (point/3d -12578 122820 -3119))
			(cons "town/dion" (point/3d 18767 145077 -3121))
			(cons "town/giran" (point/3d 83371 147962 -3399))
			(cons "town/oren" (point/3d 80787 54589 -1519))
			(cons "town/hunter" (point/3d 116798 77017 -2709))
			
			(cons "tomb/sacrifice" (point/3d -41629 209358 -5086))
			; tomb/down
			; tomb/dusk
			; tomb/ascetics
			; tomb/dark-omens
			; tomb/patriots
			; tomb/branded
			; tomb/saints
			; tomb/worshipers
			; tomb/martyrs
			; tomb/forbidden-path
			; tomb/witch
			; tomb/apostate
			; tomb/disciples
			; tomb/pilgrims
			
			(cons "orc-barracks" (point/3d -85933 105987 -3536))
			(cons "execution-grounds" (point/3d 44528 148489 -3702))
			(cons "wasteland" (point/3d -16665 209369 -3659))
			(cons "forgotten-temple" (point/3d -52789 190578 -3490))
			(cons "cave-of-trials" (point/3d 9988 -112486 -2467))
			(cons "elven-ruins" (point/3d -113291 235384 -3648))
			(cons "cruma-tower" (point/3d 17217 114176 -3444))
			(cons "singing-waterfall" (point/3d -109487 242920 -3524))
			(cons "altar-of-rites" (point/3d -44235 79075 -3745))
		))
		
		(define interval 1000)
		(define queue (list))
		
		(define (iterate)
			(if (not (null? queue))
				(let ((point (car queue)) (count (length queue)))
					(set! queue (cdr queue))
					(move-to connection point)
					count
				)
				0
			)
		)
		
		(define (determine place)
			(cond
				((string? place) (let ((p (assoc place places string=?))) (and p (cdr p))))
				((point/3d? place) place)
				(else #f)
			)
		)
		
		(lambda (argument)
			(cond
				((symbol? argument) (case argument ; Managment operations
					((next) (iterate))
					((left) (length queue))
					;((menu) places)
					;((route) queue)
				))
				(else ; Travelling starter
					(let ((start (@: connection 'world 'me 'position)) (finish (determine argument)))
						(or (and start finish (not (equal? start finish))
								(begin
									(set! queue (cdr (split-line-segment/3d start finish ; Make route but skip first point
										(+ (ceiling (/ (distance/3d start finish) interval)) 1)))) ; Route length
									(iterate) ; Start travelling and return route length
								)
							)
							0
						)
					)
				)
			)
		)
	)
)
