(module api racket/base
	(require
		racket/contract
		"../library/network.scm"
		"../packet/game/client/logout.scm"
	)
	(provide logout)
	
	(define (logout connection) ; �� ����� ���� ����������, �.�. ����� ���������� ��������� ������ � �������� �����
		(send connection (game-client-packet/logout))
	)
)