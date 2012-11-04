%UTILS

%nth(Index to get, Input List, Output) 
nth(1,[IH|_],IH):- !.
nth(X,[_|OH],NTH) :- 
	LX is X-1, 
	nth(LX,OH,NTH).
	
%reverse list( input_list, output_list).
reverse(A,B):- reverse_recursion(A,[],B).
reverse_recursion([],L,L).
reverse_recursion([IH|IT],B,A) :-
	reverse_recursion(IT,[IH|B],A).

% Ler inteiros do teclado. (PFR)
read_int(X):-read(X),integer(X),!. % Cut usado para parar ao ler um inteiro.
read_int(X):-writeln('Erro, pretende-se um inteiro!'),read_int(X).

% read a y\n input (PFR) 
yesno(s).
yesno(n).
read_yn(X) :- read(X), yesno(X), !.   % Cut is used to stop when a yesno(X) char is read 
read_yn(X) :- writeln('Erro, apenas s ou n!'), read_yn(X).


% Createn a new board
new_board( X, Y, Board ):- create_board(X,Y,IBoard), blank_corners(IBoard,Board),
		print_board(Board), !.

create_board( _, 0, [] ):- !.
create_board( X, Y, [Line | Lines] ):- YN is Y-1, create_line(X,0, Line), 
		create_board(X, YN, Lines).
create_line( X, X, [] ):- !.
create_line( X, LX, [' ' | Ls] ):- LLX is LX+1, create_line( X, LLX, Ls).

blank_corners([Iline|Ilines],Board ):- blank_first_corners(Iline,Oline),
		blank_last_corners(Oline, Ilines, Board).

blank_first_corners( [_|IT], ['+'|OT] ):- blank_corner(IT,OT).

blank_corner( [_|[]], ['+'|[]] ):- !.
blank_corner( [IH|IT] ,[IH|OT] ):- blank_corner(IT,OT).

blank_last_corners(Oline, Ilines, [Oline|OT] ):- blank_last(Ilines,OT).

blank_last( [IH|[]], [OH|[]] ):- blank_first_corners(IH,OH), !.
blank_last( [IH|IT], [IH|OT] ):- blank_last(IT,OT).

%-----------------\\----------


%check if it's vacant...
vacant(X,Y,Board) :- 
	nth(Y,Board,Row),
	nth(X,Row,' ').

% set piece on X Y in Board 
set_piece( IBoard, Piece, X, Y, OBoard) :- 
	vacant(X,Y,IBoard),
	set_y_piece(IBoard, Piece, X, Y, 1, OBoard),!.

set_y_piece( [IH|IT], P, X, Y,  Y, [OH|IT]):- 
	set_x_piece(IH,P,X,1,OH),!.
set_y_piece( [IH|IT], P, X, Y,  LY, [IH|OT]) :- 
	LYY is LY+1, 
	set_y_piece(IT,P,X,Y,LYY,OT).

set_x_piece( [_|IT], P, X, X, [P|IT] ):- !.
set_x_piece( [IH|IT], P, X, LX, [IH|OT] ):- 
	XLX is LX+1, 
	set_x_piece(IT,P,X,XLX,OT).

% top_right(Board, PosX, PosY, TRX, TRY). (PFR)
top_right(_, X, 1, X, 1).
top_right([HB|_], X, Y, X, Y) :-
	length(HB, X).
top_right([HB|_], X, 2, X, 2) :-
	MaxX is X+1,
	length(HB, MaxX). 
top_right(_, X, Y, Ox, Oy) :-
	XX is X+1,
	YY is Y-1,
	top_right(_, XX, YY, Ox, Oy).
	
% START (PFR)
new_game :-
	print_help,
	setup_game,
	start_game.

% HELP (PFR)
print_help :-
	writeln('MOAI'),
	writeln('====\n'),
	writeln('Jogo para 2 jogadores, que v�o alternando entre si, at� que um'),
	writeln('deixe de conseguir mover o seu pe�o, perdendo o jogo.'),
	writeln('Cada jogador � representado por um pe�o no tabuleiro de jogo, comum a ambos'),
	writeln('os jogadores existem os bloqueadores, pe�as colocadas no tabuleiro pelos'),
	writeln('jogadores, um em cada jogada, uma vez colocados ficam fixos at� ao final do jogo.'),
	writeln('Joga-se num tabuleiro quadrado, originalmente de 8 por 8 casas.'),
	writeln('Esta vers�o permite qualquer dimens�o.'),
	writeln('As casas dos cantos s�o exclu�das do tabuleiro de jogo.'),
	writeln('Cada jogada � composta por 2 a��es:'),
	writeln('1 - Coloca��o no tabuleiro de um bloqueador;'),
	writeln('2 - Mover 1 pe�o, afastando ou aproximando-o do bloqueador acabado de colocar.'),
	writeln('Apenas se pode mover o pe�o quando est� em linha com o bloqueador, horizontal,'),
	writeln('vertical ou diagonal.'),
	writeln('Entre o bloqueador e o pe�o a mover tem de ser deixada uma casa vazia.'),
	writeln('O movimento do pe�o � sempre feito at� encontrar um obst�culo: bloqueador,'),
	writeln('pe�o ou fronteira do tabuleiro.'),
	writeln('Um jogador no inicio da sua vez tem de ser capaz de mover o seu pe�o de acordo'),
	writeln('com as regras, mesmo que n�o seja essa a jogada que pretende realizar, se tal n�o'),
	writeln('for poss�vel, perde o jogo.'),
	writeln('').
	
% SETUP (PFR)
setup_game :- 
	writeln('Quantas linhas pretende no tabuleiro (m�nimo 5): '),
	read_int(YSize),
	writeln('Quantas colunas pretende no tabuleiro  (m�nimo 5): '),
	read_int(XSize),
	new_board(XSize, YSize, Board),
	writeln('Jogador branco � humano (s/n)'),
	read_yn(JogBranco),
	writeln('Jogador preto � humano (s/n)'),
	read_yn(JogPreto),
	jogadores(JogBranco, JogBranco).
	
	
% Falta predicado para definir jogador como humano ou computador.
	jogadores(JogB, JogP) :- !.
		

% START GAME	
start_game :- 
	writeln('Come�ar a jogar').

		


print_board( [LastLine | [] ] ):- 
	print_line( LastLine), 
	write('\n\n'), !.
print_board( [Line | Lines ] ):- 
	print_line( Line), 
	print_separator(Line), 
	print_board(Lines).

print_line( [LastPiece | [] ] ):-  
	write(' '), 
	write(LastPiece), 
	write('\n'), !.
print_line( [Piece | Pieces ] ):- 
	write(' '), 
	write(Piece), 
	write(' |'), 
	print_line( Pieces ).

print_separator( [_|[]] ):- 
	write('---\n'), !.
print_separator( [_|T] ):- 
	write('---|'), 
	print_separator(T).

/*
[
 ['-',' ','C',' ','C',' ',' ','-'],
 ['C',' ',' ',' ',' ',' ',' ',' '],
 ['P',' ','C',' ',' ',' ',' ',' '],
 ['C',' ',' ',' ','B',' ',' ',' '],
 [' ',' ','C',' ',' ',' ',' ',' '], 
 [' ',' ',' ',' ',' ',' ','C',' '], 
 [' ',' ',' ',' ',' ',' ',' ',' '], 
 ['-',' ',' ',' ',' ',' ',' ','-']
]
*/

%search_piecce(Board,Piece_to_Search, XCounter,YCounter,XPiece,YPiece) =>output:xpiece,ypiece
search_piece(Board,P,XC,YC,XP,YP) :- 
	search_diagonal(Board, P, XC,YC,XP,YP).
search_piece(Board,P,XC,YC,XP,YP) :- 
	search_horizontal(Board, P, XC,YC,XP,YP).
search_piece(Board,P,XC,YC,XP,YP) :- 
	search_vertical(Board, P, XC,YC,XP,YP).


%search_diagonal(Board,Piece_to_Search, XCounter,YCounter,XPiece,YPiece) =>output:xpiece,ypiece
search_diagonal(Board, P, XC,YC,XP,YP) :- 
	search_diagonal_forward(Board, P, XC,YC,XP,YP).
search_diagonal(Board, P, XC,YC,XP,YP) :- 
	search_diagonal_backward(Board, P, XC,YC,XP,YP).
search_diagonal(Board, P, XC,YC,XP,YP) :- 
	search_reverse_diagonal_forward(Board, P, XC,YC,XP,YP).
search_diagonal(Board, P, XC,YC,XP,YP) :- 
	search_reverse_diagonal_backward(Board, P, XC,YC,XP,YP).

search_diagonal_forward(Board,_,_,YC,_,_) :- 
	length(Board,L), 
	L<YC, !, false.

search_diagonal_forward([H|_],_,XC,_,_,_) :- 
	length(H,L), 
	L<XC, !, false.

search_diagonal_forward([H|T], _, XC,YC,XC,YC) :- 
	length(H,LX), 
	length(T,LY),
	XC==LX,
	YC==LY+1,
	!, false.

search_diagonal_forward(Board, P, XC,YC,XC,YC) :- 
	nth(YC,Board,Row),
	nth(XC,Row,P).
					
search_diagonal_forward(Board, P, XC,YC,XP,YP) :- 
	TXC is XC + 1,
	TYC is YC + 1,
	search_diagonal_forward(Board, P, TXC,TYC,XP,YP).



search_diagonal_backward(_,_, XC,_,_,_) :- 
	XC<1, !, false.
 
search_diagonal_backward(_,_,_,YC,_,_) :- 
	YC<1, !, false.

search_diagonal_backward(_,_,XC,YC,_,_) :- 
	XC==1,YC==1, !, false.

search_diagonal_backward(Board, P, XC,YC,XC,YC) :- 
	nth(YC,Board,Row),
	nth(XC,Row,P).
						
search_diagonal_backward(Board, P, XC,YC,XP,YP) :- 
	TXC is XC - 1,
	TYC is YC - 1, 
	search_diagonal_backward(Board, P, TXC,TYC,XP,YP).
						
%search_reverse_diagonal(Board,Piece_to_Search, XCounter,YCounter,XPiece,YPiece) =>output:xpiece,ypiece
search_reverse_diagonal(Board, P, XC,YC,XP,YP) :- 
	search_reverse_diagonal_forward(Board, P, XC,YC,XP,YP).
search_reverse_diagonal(Board, P, XC,YC,XP,YP) :- 
	search_reverse_diagonal_backward(Board, P, XC,YC,XP,YP).

search_reverse_diagonal_forward(Board, _, _,YC,_,_) :- 
	length(Board,L), 
	L<YC, !, false.

search_reverse_diagonal_forward(_, _, XC,_,_,_) :- 
	XC<1, !, false.

search_reverse_diagonal_forward(Board, _, XC,YC,_,_) :- 
	length(Board,L),
	XC==0, YC==L, 
	!, false. 
							
search_reverse_diagonal_forward(Board, P, XC,YC,XC,YC) :- 
	nth(YC,Board,Row),
	nth(XC,Row,P),
	write('found').
						
search_reverse_diagonal_forward(Board, P, XC,YC,XP,YP) :- 
	TXC is XC - 1,
	TYC is YC + 1,
	search_reverse_diagonal_forward(Board, P, TXC,TYC,XP,YP).




search_reverse_diagonal_backward(_, _, _,YC,_,_) :- 
	YC<1, !, false.

search_reverse_diagonal_backward([H|_], _, XC,_,_,_) :- 
	length(H, L), 
	L<XC, !, false.

search_reverse_diagonal_backward([H|T], _, XC,YC,_,_) :- 
	length(H, XL),
	length(T,YL),
	XC==XL, YC==YL+1, 
	!, false.

search_reverse_diagonal_backward(Board, P, XC,YC,XC,YC) :- 
	nth(YC,Board,Row),
	nth(XC,Row,P).
						
search_reverse_diagonal_backward(Board, P, XC,YC,XP,YP) :- 
	TXC is XC + 1,
	TYC is YC - 1, 
	search_reverse_diagonal_backward(Board, P, TXC,TYC,XP,YP).

%search_horizontal(Board,Piece_to_Search, XCounter,YCounter,XPiece,YPiece) =>output:xpiece,ypiece
search_horizontal(Board, P, XC,YC,XP,YP) :- 
	search_horizontal_forward(Board, P, XC,YC,XP,YP).
search_horizontal(Board, P, XC,YC,XP,YP) :- 
	search_horizontal_backward(Board, P, XC,YC,XP,YP).

search_horizontal_forward(Board, _, XC,_,_,_) :- 
	length(Board, L), 
	L<XC, !, false.
search_horizontal_forward(Board, P, XC,YC,XC,YC) :- 
	nth(YC,Board,Row),
	nth(XC,Row,P).
						
search_horizontal_forward(Board, P, XC,YC,XP,YP) :- 
	TXC is XC + 1,
	search_horizontal_forward(Board, P, TXC,YC,XP,YP).
						
search_horizontal_backward(_, _, XC,_,_,_) :- 
	XC<1, !, false.	
search_horizontal_backward(Board, P, XC,YC,XC,YC) :- 
	nth(YC,Board,Row),
	nth(XC,Row,P).
					
search_horizontal_backward(Board, P, XC,YC,XP,YP) :- 
	TXC is XC - 1,
	search_horizontal_backward(Board, P, TXC,YC,XP,YP).


%search_vertical(Board,Piece_to_Search, XCounter,YCounter,XPiece,YPiece) =>output:xpiece,ypiece
%com o OR logico
%search_vertical(Board, P, XC,YC,XP,YP) :- search_vertical_forward(Board, P, XC,YC,XP,YP) ;
%					search_vertical_backward(Board, P, XC,YC,XP,YP).
search_vertical(Board, P, XC,YC,XP,YP) :- 
	search_vertical_forward(Board, P, XC,YC,XP,YP).

search_vertical(Board, P, XC,YC,XP,YP) :- 
	search_vertical_backward(Board, P, XC,YC,XP,YP).


search_vertical_forward(Board, _, _,YC,_,_) :- 
	length(Board,L), 
	L < YC, !, false.
	 
search_vertical_forward(Board, P, XC,YC,XC,YC) :- 
	nth(YC,Board,Row),
	nth(XC,Row,P).
						
search_vertical_forward(Board, P, XC,YC,XP,YP) :- 
	TYC is YC + 1,
	search_vertical_forward(Board, P, XC,TYC,XP,YP).

search_vertical_backward(_, _, _,YC,_,_) :- 
	YC < 1, !, false. 
search_vertical_backward(Board, P, XC,YC,XC,YC) :- 
	nth(YC,Board,Row),
	nth(XC,Row,P).
						
search_vertical_backward(Board, P, XC,YC,XP,YP) :- 
	TYC is YC - 1, 
	search_vertical_backward(Board, P, XC,TYC,XP,YP).


		
		
		

%move(Input_Board, XCounter, YCounter, Piece_To_move, PushPull, Output_Board).
move(IBoard, XC, YC, Piece, PushPull, OBoard) :- 
	move_diagonal(IBoard, XC,YC,Piece,PushPull,OBoard).
move(IBoard, XC, YC, Piece, PushPull, OBoard) :- 
	move_horizontal(IBoard, XC,YC,Piece,PushPull,OBoard).
move(IBoard, XC, YC, Piece, PushPull, OBoard) :- 
	move_vertical(IBoard, XC,YC,Piece,PushPull,OBoard).



move_horizontal(IBoard, XC,YC,Piece,'push',OBoard) :- 
	search_horizontal_forward(IBoard,Piece,XC,YC,_,YP),
	horizontal_forward(IBoard, Piece,YP,OBoard),
	write('moving push horizontal forward\n\n').

move_horizontal(IBoard, XC,YC,Piece,'pull',OBoard) :- 
	search_horizontal_backward(IBoard,Piece,XC,YC,_,YP),
	horizontal_forward(IBoard, Piece,YP,OBoard),
	write('moving pull horizontal forward\n\n').
	
move_horizontal(IBoard, XC,YC,Piece,'push',OBoard) :- 
	search_horizontal_backward(IBoard,Piece,XC,YC,_,YP),
	horizontal_backward(IBoard, Piece,YP,OBoard),
	write('moving push horizontal backward\n\n').

move_horizontal(IBoard, XC,YC,Piece,'pull',OBoard) :- 
	search_horizontal_forward(IBoard,Piece,XC,YC,_,YP),
	horizontal_backward(IBoard, Piece,YP,OBoard),
	write('moving pull horizontal backward\n\n').



%move_vertical(IBoard, XC,YC,Piece,PushPull,OBoard) :- 
%	search_vertical(IBoard,Piece,XC,YC,XP,YP),
%	write('moving vertical\n').	


	
%move_diagonal(IBoard, XC,YC,Piece,PushPull,OBoard) :- 
%	search_diagonal(IBoard,Piece,XC,YC,XP,YP),
%	write('moving diagonal\n').


/*
 * horizontal move predicates..
 */

horizontal_forward([IH|IT], Piece, 1,[OH|IT]) :-
	erase_horizontal_forward(IH,Piece,OH).
horizontal_forward([IH|IT], Piece, YPiece,[IH|OT]) :-
	L is YPiece - 1,
	horizontal_forward(IT, Piece, L, OT). 

horizontal_backward([IH|IT], Piece, 1,[OH|IT]) :-
	reverse(IH,REV),
	erase_horizontal_forward(REV,Piece,OREV), 
	reverse(OREV,OH).
horizontal_backward([IH|IT], Piece, YPiece,[IH|OT]) :-
	L is YPiece - 1,
	horizontal_backward(IT, Piece, L, OT). 

erase_horizontal_forward( [Piece|IT], Piece, [' '|OT] ) :-
	push_h_f(IT,Piece,OT).
erase_horizontal_forward( [IH|IT], Piece, [IH|OT] ) :-
	erase_horizontal_forward(IT,Piece,OT).


push_h_f( ' ', Piece, Piece):-
	!,true.
push_h_f( [' ',X|IT], Piece, [Piece,X|IT]):-
	not(white(X)),!,true.
push_h_f( [' ',' '],Piece,[' ',Piece]) :-
	!, true. 
push_h_f( [' ',' '|IT],Piece,[' '|OT]) :- 
	push_h_f( [' '|IT], Piece, OT). 


white('C') :-
	false.
white('P') :-
	false.
white('B') :-
	false.	
white(_) :-
	true.

%psf([IH|IT],Piece,[OH|OT])


/*
 * Vertical Move predicates
 */ 



vertical_up(IBoard,Piece,X,OBoard) :-
	extract_column(IBoard,X,IL,OL,OBoard),
	reverse(IL, REV_IL),
	erase_horizontal_forward(REV_IL,Piece,REV_OL),
	reverse(REV_OL,OL).

vertical_down(IBoard,Piece,X,OBoard) :-
	extract_column(IBoard,X,IL,OL,OBoard),
	erase_horizontal_forward(IL,Piece,OL).



/* extract_column(IBoard ,X , ILIST,OLIST, OBOARD).
 * 
 * IBoard -> input board
 * X -> index of the column to extract
 * ILIST -> input Column as a List
 * OList -> output Column to manipulate.
 * OBoard -> copy of IBoard but with the column X 
 *       with the elements of LIST
 */

extract_column([],_,[],[],[]).
extract_column([IH|IT], X, [ILH|ILT], [OLH|OLT], [OH|OT]) :- 
	line_extract(IH,X,ILH,OLH,OH),
	extract_column(IT,X,ILT,OLT,OT). 

line_extract([IH|IT],1,IH,OH,[OH|IT]).
line_extract([IH|IT],X,IL,OL,[IH|OT]) :-
	TX is X - 1,
	line_extract(IT,TX,IL,OL,OT).
	


test(B) :-test_vertical_down(B).

test_vertical_down(B) :-
	new_board(8,8,B),
	set_piece(B,'P', 2,2,OB),
	print_board(OB),
	vertical_down(OB,'P',2,BOB),
	print_board(BOB).


test_vertical_up(B) :-
	new_board(8,8,B),
	set_piece(B,'P', 2,6,OB),
	print_board(OB),
	vertical_up(OB,'P',2,BOB),
	print_board(BOB).

	

test_horizontal_move(B) :- 
	new_board(8,8,B), 
	test_pull_f(B),write('pull_f\n\n\n\n\n\n'),
	test_push_f(B),write('push_f\n\n\n\n\n\n'),
	test_pull_b(B),write('pull_b\n\n\n\n\n\n'),
	test_push_b(B),write('push_b\n\n\n\n\n\n').	
	
	

	
test_pull_f(B) :- 
	set_piece(B,'P', 2,1,OB),
	set_piece(OB,'C', 7,1,BOB),
	print_board(BOB),
	%pull_horizontal_forward(BOB,'P',1,BBOB),
	move_horizontal(BOB, 7,1,'P','pull',BBOB),
	print_board(BBOB).
	
test_push_f(B) :- 	 
	set_piece(B,'P', 5,1,OB),
	set_piece(OB,'C', 3,1,BOB),
	print_board(BOB),
	%push_horizontal_forward(BOB,'P',1,BBOB),
	move_horizontal(BOB, 3,1,'P','push',BBOB),
	print_board(BBOB).
	
test_pull_b(B) :- 
	set_piece(B,'P', 7,1,OB),
	set_piece(OB,'C', 3,1,BOB),
	print_board(BOB),
	%pull_horizontal_backward(BOB,'P',1,BBOB),
	move_horizontal(BOB, 3,1,'P','pull',BBOB),
	print_board(BBOB).
	
test_push_b(B) :- 
	set_piece(B,'P', 5,1,OB),
	set_piece(OB,'C', 6,1,BOB),
	print_board(BOB),
	%push_horizontal_backward(BOB,'P',1,BBOB),
	move_horizontal(BOB, 6,1,'P','push',BBOB),
	print_board(BBOB).

	
	
	


test_check(B):- new_board(8,8,B), 
	set_piece(B,'P', 2,1,OB),
	test_diagonal(OB),
	test_vertical(OB),
	test_horizontal(OB).


test_diagonal(B) :- 
	search_diagonal(B,'P',3,2,_,_), write('diagonal_backward check\n'),
	not(search_diagonal(B,'P',6,6,_,_)), write('diagonal_forward check\n'),
	search_diagonal(B,'P',1,2,_,_), write('diagonal_reverse_backward check\n'),
	not(search_diagonal(B,'P',2,6,_,_)), write('diagonal_reverse_forward check\n').
		


test_vertical(B) :- search_vertical(B,'P',2,2,_,_),write('vertival_backward check\n'),
	not(search_vertical(B,'P',4,6,_,_)),write('vertival_forward check\n').

test_horizontal(B) :- 
	search_horizontal(B,'P',4,1,_,_),write('horizontal_backward check\n'),
	not(search_horizontal(B,'P',2,4,_,_)),write('horizontal_forward check\n').

%test_horizontal()
