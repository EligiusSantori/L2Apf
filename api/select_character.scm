(module api racket/base
	(require
		racket/contract
		racket/async-channel
		"../library/extension.scm"
		"../library/structure.scm"
		"../library/network.scm"
		"../library/system.scm"
		"../packet/game/client/select_character.scm"
		"../packet/game/server/player_character.scm"
		"../packet/game/client/enter_world.scm"
		"refresh_manor_list.scm"
		"refresh_quest_list.scm"
		"refresh_skill_list.scm"
		"read_thread.scm"
		"send_thread.scm"
		"main_thread.scm"
	)
	(provide select-character)
	
	(define (select-character connection character)
		(send connection (game-client-packet/select-character (list
			(cons 'id (get-field character 'id))
		)))
		
		(let loop ()
			(let ((buffer (receive connection)))
				(case (get-packet-id buffer)
					((#x15) (let ((packet (game-server-packet/player-character buffer)))
						(let ((me (create-protagonist (get-field packet 'me))))
							(let ((world (box (list (cons 'me me)))))
								(begin
									(set-box-field! connection 'world world)
									
									(refresh-manor-list connection)
									(refresh-quest-list connection)
									(send connection (game-client-packet/enter-world))
									(refresh-skill-list connection)
								)
							)
						)
					))
					(else (loop))
				)
			)
		)
		
		(if (get-box-field connection 'world)
			(begin
				(set-box-field! connection 'input-channel (make-async-channel))
				(let ((read (thread (bind read-thread connection))))
					(set-box-field! connection 'read-thread read)
				)
				(set-box-field! connection 'output-channel (make-async-channel))
				(let ((send (thread (bind send-thread connection))))
					(set-box-field! connection 'send-thread send)
				)
				(let ((channel (make-async-channel)))
					(set-box-field! connection 'events channel)
					(let ((main (thread (bind main-thread connection))))
						(set-box-field! connection 'thread main)
					)
					channel
				)
			)
			#f
		)
	)
)