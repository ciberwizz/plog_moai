%UTILS

%nth(Index to get, Input List, Output) 
nth(1,[IH|_],IH):- !.
nth(X,[_|OH],NTH) :- 
	LX is X-1, 
	nth(LX,OH,NTH).
	

%search_piece(IBoard,Piece,X,Y):-
search_piece(IBoard,Piece,X,Y):-
	search_piece_y(IBoard,Piece,1,X,Y).

search_piece_y([],_,_,_,_):-
	!,false.
search_piece_y([IH|_],Piece,TY,X,TY):-
	search_piece_x(IH,Piece,1,X).

search_piece_y([_|IT],Piece,TY,X,Y):-
	TTY is TY + 1,
	search_piece_y(IT,Piece,TTY,X,Y).


search_piece_x([],_,_,_) :-
	!,false.

search_piece_x([Piece|_],Piece,TX,TX):-
	!,true.

search_piece_x([_|IT],Piece,TX,X) :-
	TTX is TX + 1,
	search_piece_x(IT,Piece,TTX,X).

	
%reverse list( input_list, output_list).
reverse(A,B):- reverse_recursion(A,[],B).
reverse_recursion([A|[]],L,[A|L]) :-
	!,true.
reverse_recursion([],L,L) :-
	!,true.
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

erase_piece(IBoard, X, Y, OBoard) :- 
	set_y_piece(IBoard, ' ', X, Y, 1, OBoard),!.
	
% START (PFR)
new_game :-
	print_help,
	setup_game(Board),
	start_game(Board).

% HELP (PFR)
print_help :-
	writeln('MOAI'),
	writeln('====\n'),
	writeln('Jogo para 2 jogadores, que vao alternando entre si, ate que um'),
	writeln('deixe de conseguir mover a sua peca, perdendo o jogo.'),
	writeln('Cada jogador e representado por uma peca no tabuleiro de jogo.'),
	writeln('Comum a ambos os jogadores, sao os bloqueadores, pecas colocadas no tabuleiro'),
	writeln(', uma em cada jogada. Uma vez colocados ficam fixos ate ao final do jogo.'),
	writeln('Joga-se num tabuleiro quadrado, originalmente de 8 por 8 casas.'),
	writeln('Esta versao permite qualquer dimensao.'),
	writeln('As casas dos cantos sao excluidas do tabuleiro de jogo.'),
	writeln('Cada jogada e composta por 2 acoes:'),
	writeln('1 - Colocacao de um bloqueador;'),
	writeln('2 - Mover 1 peca, afastando ou aproximando-o do bloqueador acabado de colocar.'),
	writeln('Apenas se pode mover peca quando esta em linha com o bloqueador, horizontal,'),
	writeln('vertical ou diagonal.'),
	writeln('O movimento do peca e sempre feito ate encontrar um obstaculo: bloqueador,'),
	writeln('peca ou fronteira do tabuleiro.'),
	writeln('Um jogador no inicio da sua vez tem de ser capaz de mover a sua peca de acordo'),
	writeln('com as regras, mesmo que nao seja essa a jogada que pretende realizar, se tal nao'),
	writeln('for possivel, perde o jogo.'),
	writeln('').
	
% SETUP (PFR)
setup_game(Board) :- 
	writeln('Quantas linhas pretende no tabuleiro (m�nimo 5, standard 8): '),
	read_int(YSize),
	writeln('Quantas colunas pretende no tabuleiro  (m�nimo 5, standard 8): '),
	read_int(XSize),
	new_board(XSize, YSize, Board).
		
init_posicoes(IBoard,OBoard):-
	writeln('Jogador Branco indique a sua posição inicial'),
	writeln('X: '), read_int(BX),
	writeln('Y: '), read_int(BY),
	set_piece( IBoard, 'B', BX, BY, OB),
	print_board(OB),
	writeln('Jogador Preto indique a sua posição inicial'),
	writeln('X: '), read_int(PX),
	writeln('Y: '), read_int(PY),
	set_piece( OB, 'P', PX, PY, OBoard),
	print_board(OBoard).


% START GAME	
start_game(Board) :- 
	init_posicoes(Board,OBoard),
	writeln('Come�ar a jogar'),
	main_loop(OBoard,'P').
	
endgame(Board,Piece) :-
	search_piece(Board,Piece,X,Y),
	not(movable(Board,Piece,X,Y)),
	!,true.
	
main_loop(Board,'P'):-
	endgame(Board,'P'),
	writeln('O Jogador B GANHOU o jogo!').
	
main_loop(Board,'B'):-
	endgame(Board,'B'),
	writeln('O Jogador P GANHOU o jogo!').

main_loop(Board,Piece) :-
	print_board(Board),
	write('Jogador '),
	write(Piece),
	writeln(' ,onde deseja colocar o Counter?'),
	writeln('X: '), read_int(CX),
	writeln('Y: '), read_int(CY),
	set_piece(Board,'C',CX,CY,OBoard),
	print_board(OBoard),
	writeln('Deseja fazer Push(s) ou Pull(n)?'),
	read_yn(Push),
	writeln('Deseja que seja aplicado ao jogador: P(s) ou B(n)?'),
	read_yn(Jog),
	make_a_move(OBoard,Push,Jog,CX,CY,FBoard),
	print_board(FBoard),
	next_piece(Piece,OPiece),
	main_loop(FBoard,OPiece).


next_piece('P','B').
next_piece('B','P').

%make_a_move(IBoard,Push,Jog,X,Y,OBoard).
make_a_move(IBoard,s,s,X,Y,OBoard):-
	move(IBoard,X,Y,'P','push',OBoard).
make_a_move(IBoard,s,n,X,Y,OBoard):-
	move(IBoard,X,Y,'P','pull',OBoard).
make_a_move(IBoard,n,s,X,Y,OBoard):-
	move(IBoard,X,Y,'B','push',OBoard).
make_a_move(IBoard,n,n,X,Y,OBoard):-
	move(IBoard,X,Y,'B','pull',OBoard).	
make_a_move(_,P,T,_,_,_):-	
	write('wrong move\n'),!,false.


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
	move_reverse_diagonal(IBoard, XC,YC,Piece,PushPull,OBoard).
move(IBoard, XC, YC, Piece, PushPull, OBoard) :- 
	move_horizontal(IBoard, XC,YC,Piece,PushPull,OBoard).
move(IBoard, XC, YC, Piece, PushPull, OBoard) :- 
	move_vertical(IBoard, XC,YC,Piece,PushPull,OBoard).

%HORIZONTAL

move_horizontal(IBoard, XC,YC,Piece,'push',OBoard) :- 
	search_horizontal_forward(IBoard,Piece,XC,YC,XP,YP),
	horizontal_forward(IBoard, Piece,YP,OBoard).

move_horizontal(IBoard, XC,YC,Piece,'pull',OBoard) :- 
	search_horizontal_backward(IBoard,Piece,XC,YC,XP,YP),
	horizontal_forward(IBoard, Piece,YP,OBoard).
	
move_horizontal(IBoard, XC,YC,Piece,'push',OBoard) :- 
	search_horizontal_backward(IBoard,Piece,XC,YC,XP,YP),
	horizontal_backward(IBoard, Piece,YP,OBoard).

move_horizontal(IBoard, XC,YC,Piece,'pull',OBoard) :- 
	search_horizontal_forward(IBoard,Piece,XC,YC,XP,YP),
	horizontal_backward(IBoard, Piece,YP,OBoard).

%VERTICAL

%counter is up fom piece
move_vertical(IBoard, XC,YC,Piece,'push',OBoard) :- 
	search_vertical_forward(IBoard,Piece,XC,YC,_,_), 
	vertical_down(IBoard,Piece,XC,OBoard).	
	
move_vertical(IBoard, XC,YC,Piece,'pull',OBoard) :- 
	search_vertical_forward(IBoard,Piece,XC,YC,_,_),
	vertical_up(IBoard,Piece,XC,OBoard).	
	
%counter is down fom piece
move_vertical(IBoard, XC,YC,Piece,'push',OBoard) :- 
	search_vertical_backward(IBoard,Piece,XC,YC,_,_),
	vertical_up(IBoard,Piece,XC,OBoard).	
	
move_vertical(IBoard, XC,YC,Piece,'pull',OBoard) :- 
	search_vertical_backward(IBoard,Piece,XC,YC,_,_),
	vertical_down(IBoard,Piece,XC,OBoard).

%DIAGONAL
	
move_diagonal(IBoard, XC,YC,Piece,'push',OBoard) :- 
	search_diagonal_forward(IBoard,Piece,XC,YC,_,_),
	top_left(XC,YC,TXC,TYC),
	diagonal_down(IBoard,Piece,TXC,TYC,OBoard).
	
move_diagonal(IBoard, XC,YC,Piece,'pull',OBoard) :- 
	search_diagonal_forward(IBoard,Piece,XC,YC,_,_),
	top_left(XC,YC,TXC,TYC),
	diagonal_up(IBoard,Piece,TXC,TYC,OBoard).
	
		
move_diagonal(IBoard, XC,YC,Piece,'push',OBoard) :- 
	search_diagonal_backward(IBoard,Piece,XC,YC,_,_),
	top_left(XC,YC,TXC,TYC),
	diagonal_up(IBoard,Piece,TXC,TYC,OBoard).
	
move_diagonal(IBoard, XC,YC,Piece,'pull',OBoard) :- 
	search_diagonal_backward(IBoard,Piece,XC,YC,_,_),
	top_left(XC,YC,TXC,TYC),
	diagonal_down(IBoard,Piece,TXC,TYC,OBoard).


%REVERSE DIAGONAL
	
move_reverse_diagonal(IBoard, XC,YC,Piece,'push',OBoard) :- 
	search_reverse_diagonal_forward(IBoard,Piece,XC,YC,_,_),
	nth(1,IBoard,Row), length(Row,N),
	top_right(N,XC,YC,TXC,TYC),
	diagonal_down(IBoard,Piece,TXC,TYC,OBoard).
	
move_reverse_diagonal(IBoard, XC,YC,Piece,'pull',OBoard) :- 
	search_reverse_diagonal_forward(IBoard,Piece,XC,YC,_,_),
	nth(1,IBoard,Row), length(Row,N),
	top_right(N,XC,YC,TXC,TYC),
	diagonal_up(IBoard,Piece,TXC,TYC,OBoard).
	
		
move_reverse_diagonal(IBoard, XC,YC,Piece,'push',OBoard) :- 
	search_reverse_diagonal_backward(IBoard,Piece,XC,YC,_,_),
	nth(1,IBoard,Row), length(Row,N),
	top_right(N,XC,YC,TXC,TYC),
	diagonal_up(IBoard,Piece,TXC,TYC,OBoard).
	
move_reverse_diagonal(IBoard, XC,YC,Piece,'pull',OBoard) :- 
	search_reverse_diagonal_backward(IBoard,Piece,XC,YC,_,_),
	nth(1,IBoard,Row), length(Row,N),
	top_right(N,XC,YC,TXC,TYC),
	diagonal_down(IBoard,Piece,TXC,TYC,OBoard).





move_piece(B, X, Y, NX, NY, OB):-
  nth(Y,B,ROW),nth(X,ROW,PX),
  set_piece(B,PX,NX,NY,OB1),
  erase_piece(OB1,X,Y,OB),
  true.


/*
 * horizontal move predicates..
 */

horizontal_forward(IB, Piece,_,OB) :-
	search_piece(IB,Piece,X,Y),
	horizontal_forward_(IB,X,Y,_,OB).


horizontal_forward_(IB,XP,YP,TB,IB) :-
	TX is XP + 1,write('xxxxxxxx1\n'),
	not(move_piece(IB,XP,YP,TX,YP,TB)),write('xxxxxxxx2\n').

horizontal_forward_(IB,XP,YP,TB,OB) :-
	TX is XP + 1,write('ffffffff1\n'),
	nth(YP,IB,Row),nth(TX,Row,' '),write('ffffffff2\n'),
	move_piece(IB,XP,YP,TX,YP,TB),write('ffffffff3\n'),
	horizontal_forward_(TB,XP,YP,_,OB),write('ffffffff4\n').


horizontal_backward(IB, Piece,_,OB) :-
	search_piece(IB,Piece,X,Y),
	horizontal_backward_(IB,Piece,X,Y,_,OB).
	
horizontal_backward_(IB,_,XP,YP,TB,IB) :-
	TX is XP - 1,
	not(move_piece(IB,XP,YP,TX,YP,TB)).
	
horizontal_backward_(IB,Piece,XP,YP,TB,OB) :-
	TX is XP - 1,
	move_piece(IB,XP,YP,TX,YP,TB),
	horizontal_backward_(TB,Piece,XP,YP,_,OB).







erase_horizontal_forward( [Piece|IT], Piece, [' '|OT] ) :-
	push_h_f(IT,Piece,OT),
	!,true.
erase_horizontal_forward( [IH|IT], Piece, [IH|OT] ) :-
	erase_horizontal_forward(IT,Piece,OT).


push_h_f( [' '], Piece, Piece):-
	!,true.
push_h_f( [' '|[]], Piece, Piece):-
	!,true.
push_h_f( [' ',X|IT], Piece, [Piece,X|IT]):-
	not(white(X)),!,true.
push_h_f( [' ',' '|IT],Piece,[' '|OT]) :-
	push_h_f( [' '|IT], Piece, OT). 


white(' ') :-
	true.	
white(_) :-
	false.


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
extract_column([IH|IT], X, IL, OL, [IH|OT]) :-
	line_extract(IH,X,'+',_,_),
	extract_column(IT,X,IL,OL,OT).
extract_column([IH|IT], X, [ILH|ILT], [OLH|OLT], [OH|OT]) :- 
	line_extract(IH,X,ILH,OLH,OH),
	extract_column(IT,X,ILT,OLT,OT). 

%line_extract(+ILine,+X,-IX,-OX,-OLine]).
line_extract([IH|IT],1,IH,OH,[OH|IT]) :-
	!,true.
line_extract([IH|IT],X,IL,OL,[IH|OT]) :-
	TX is X - 1,
	line_extract(IT,TX,IL,OL,OT).
	


/*
 *   Diagonal moves
 */

diagonal_down(IBoard,Piece,X,Y,OBoard):-
	top_left(X,Y,TX,TY),
	extract_diagonal(IBoard,TX,TY,IList,OList,OBoard),
	erase_horizontal_forward(IList,Piece,OList).


diagonal_up(IBoard,Piece,X,Y,OBoard):-
	top_left(X,Y,TX,TY),
	extract_diagonal(IBoard,TX,TY,IList,OList,OBoard),
	reverse(IList,RIList),
	erase_horizontal_forward(RIList,Piece,ROList),
	reverse(ROList,OList).


/* extract_diagonal(IBoard ,X,Y, ILIST,OLIST, OBOARD).
 * 
 * IBoard -> input board
 * X -> index of the top_left diagonal
 * Y -> index of the top_left diagonal
 * ILIST -> input Column as a List
 * OList -> output Column to manipulate.
 * OBoard -> copy of IBoard but with the column X 
 *       with the elements of LIST
 */

extract_diagonal([],_,_,[],[],[]) :-
	!, true.
extract_diagonal([IH|IT],X,_,[],[],[IH|IT]):-
	LX is X-1,
	length(IH,LX),
	!, true.
	
extract_diagonal([IH|IT],X,1,[ILH|ILT],[OLH|OLT],[OH|OT]):- 
	line_extract(IH,X,ILH,OLH,OH), 
	TX is X+1,
	extract_diagonal(IT,TX,1,ILT,OLT,OT).
extract_diagonal([IH|IT],X,Y,IL,OL,[IH|OT]):-
	TY is Y - 1,
	extract_diagonal(IT,X,TY,IL,OL,OT).


%top_left(Input_X, Intput_Y, Output_x,Output_Y
%given an XY it gives the top diagonal.
top_left(1,1,2,2).
top_left(1,Y,1,Y).
top_left(X,1,X,1).
top_left(IX,IY,OX,OY) :- 
	TX is IX - 1,
	TY is IY - 1,
	top_left(TX,TY,OX,OY). 







%reverse diagonal


%reverse_diag_down(IBoard, current_X, current_Y,OBoard)
reverse_diagonal_down(IB, X, Y,OB) :-
	reverse_diag_down_(IB, X, Y,_,OB).

reverse_diag_down_(IB, X, Y,TB,IB) :-
	LX is X - 1,
	LY is Y + 1,
	not(move_piece(IB,X,Y,LX,LY,TB)).
	
reverse_diag_down_(IB, X, Y,TB,OB) :-
	LX is X - 1,
	LY is Y + 1,
	move_piece(IB,X,Y,LX,LY,TB),
	reverse_diag_down_(TB,LX,LY,_,OB).


	
%reverse_diag_up(IBoard, current_X, current_Y,OBoard)
reverse_diagonal_up(IB, X, Y,OB) :-
	reverse_diag_up_(IB, X, Y,_,OB).
	
reverse_diag_up_(IB, X, Y,TB,IB) :-
	LX is X + 1,
	LY is Y - 1,
	not(move_piece(IB,X,Y,LX,LY,TB)).
	
reverse_diag_up_(IB, X, Y,TB,OB) :-
	LX is X + 1,
	LY is Y - 1,
	move_piece(IB,X,Y,LX,LY,TB),
	reverse_diag_up_(TB,LX,LY,_,OB).




/* reverse_extract_diagonal(IBoard ,X,Y, ILIST,OLIST, OBOARD).
 * 
 * IBoard -> input board
 * X -> index of the top_right diagonal
 * Y -> index of the top_right diagonal
 * ILIST -> input Column as a List
 * OList -> output Column to manipulate.
 * OBoard -> copy of IBoard but with the column X 
 *       with the elements of LIST
 */
 %y is bottom
extract_reverse_diagonal([],_,_,[],[],[]) :- 
	!,true.
	
extract_reverse_diagonal([IBH|_],1,1,[],[], [IBH|[]]) :- 
	line_extract(IBH,1,'+',_,_),
	!,true.

extract_reverse_diagonal([IBH|IBT],1,1,ILH,OLH, [OH|IBT]) :-
	line_extract(IBH,1,ILH,OLH,OH),
	!,true.

extract_reverse_diagonal( [IBH|IBT] ,X,1, [ILH|ILT],[OLH|OLT], [OH|OT]) :-
	line_extract(IBH,X,ILH,OLH,OH),
	TX is X - 1,
	extract_reverse_diagonal(IBT,TX,1,ILT,OLT,OT).

extract_reverse_diagonal( [IBH|IBT] ,X,Y, IList,OList, [IBH|OT]) :-
	TY is Y - 1,
	extract_reverse_diagonal(IBT,X,TY,IList,OList,OT).


%top_right(Length(Iboard),IBInput_X, Intput_Y, Output_x,Output_Y
%given an XY it gives the top diagonal.

top_right(N, N, 1, X, 2) :-
	X is N-1,
	!, true.
top_right(_, X, 1, X, 1) :-
	!,true.
top_right(X, X, Y, X, Y) :-
	!,true.
top_right(N, X, Y, Ox, Oy) :-
	XX is X+1,
	YY is Y-1,
	top_right(N, XX, YY, Ox, Oy).



%sequential_cell(List, Piece):-
%check if there is 2 valid cells for movement


sequential_cell(List,Piece) :-
	sequential_cell_to(List,Piece,0).


sequential_cell_to([Piece|LT], Piece, N):-
	sequential_cell_from(LT,Piece,N).
	
sequential_cell_to([' '|LT], Piece, N):-
	TN is N + 1,
	sequential_cell_to(LT,Piece,TN).
	
sequential_cell_to([_|LT], Piece, _):-
	sequential_cell_to(LT,Piece,0).



sequential_cell_from(_, _, 2):-
	!,true.
	
sequential_cell_from([], _, _):-
	!,false.
		
sequential_cell_from([' '|LT], Piece, N):-
	TN is N + 1,
	sequential_cell_from(LT,Piece,TN).

sequential_cell_from(_, _, _):-
	!,false.

%movable(IBoard, Piece, X,Y) :-
%horizontal

movable(IBoard, Piece, _,Y) :-
	nth(Y,IBoard,Row),
	sequential_cell(Row,Piece).
	
%vertical
movable(IBoard,Piece,X,_) :-
	extract_column(IBoard ,X , IList,_, _),
	sequential_cell(IList,Piece).
	
%diagonal
movable(IBoard,Piece,X,Y) :-
	top_left(X,Y,TX,TY),
	extract_diagonal(IBoard ,TX,TY, IList,_,_),
	sequential_cell(IList,Piece).

%reverse diagonal
movable(IBoard,Piece,X,Y) :-
	nth(1,IBoard,Row),
	length(Row,Len),
	top_right(Len,X,Y,TX,TY),
	extract_reverse_diagonal(IBoard ,TX,TY, IList,_,_),
	sequential_cell(IList,Piece).



test_search_piece(B):-
	new_board(8,8,B),
	set_piece(B,'P',4,5,BO),
	
	print_board(BO),
	search_piece(BO,'P',X,Y).




test_movable_reverse_diagonal(B):-

	new_board(8,8,B),
	set_piece(B,'P',4,4,BO),
	
	print_board(BO),
	movable(BO,'P',4,4),
	write('correct\n'),
	
	
	set_piece(BO,'C',3,5,BOB1),
	movable(BOB1,'P',4,4),
	print_board(BOB1),
	write('correct1\n'),
	
	
	set_piece(BO,'C',6,2,BOBx),
	set_piece(BOBx,'C',2,6,BOB2),
	movable(BOB2,'P',4,4),
	print_board(BOB2),
	write('correct2\n'),
		
	set_piece(BO,'C',1,7,BOBs),
	set_piece(BOBs,'C',5,3,BOB3),
	movable(BOB3,'P',4,4),
	print_board(BOB3),
	write('correct3\n').


test_movable_diagonal(B):-

	new_board(8,8,B),
	set_piece(B,'P',4,4,BO),
	
	print_board(BO),
	movable(BO,'P',4,4),
	write('correct\n'),
	
	
	set_piece(BO,'C',5,5,BOB1),
	movable(BOB1,'P',4,4),
	print_board(BOB1),
	write('correct1\n'),
	
	
	set_piece(BO,'C',2,2,BOBx),
	set_piece(BOBx,'C',6,6,BOB2),
	movable(BOB2,'P',4,4),
	print_board(BOB2),
	write('correct2\n'),
		
	set_piece(BO,'C',7,7,BOBs),
	set_piece(BOBs,'C',3,3,BOB3),
	movable(BOB3,'P',4,4),
	print_board(BOB3),
	write('correct3\n').




test_movable_vertical(B):-

	new_board(8,8,B),
	set_piece(B,'P',4,4,BO),
	
	print_board(BO),
	movable(BO,'P',4,4),
	write('correct\n'),
	
	
	set_piece(BO,'C',4,5,BOB1),
	movable(BOB1,'P',4,4),
	print_board(BOB1),
	write('correct1\n'),
	
	
	set_piece(BO,'C',4,2,BOBx),
	set_piece(BOBx,'C',4,6,BOB2),
	movable(BOB2,'P',4,4),
	print_board(BOB2),
	write('correct2\n'),
		
	set_piece(BO,'C',4,7,BOBs),
	set_piece(BOBs,'C',4,3,BOB3),
	movable(BOB3,'P',4,4),
	print_board(BOB3),
	write('correct3\n').

test_movable_horizontal(B):-

	new_board(8,8,B),
	set_piece(B,'P',4,4,BO),
	
	print_board(BO),
	movable(BO,'P',4,4),
	write('correct\n'),
	
	
	set_piece(BO,'C',5,4,BOB1),
	movable(BOB1,'P',4,4),
	print_board(BOB1),
	write('correct1\n'),
	
	
	set_piece(BO,'C',2,4,BOBx),
	set_piece(BOBx,'C',6,4,BOB2),
	movable(BOB2,'P',4,4),
	print_board(BOB2),
	write('correct2\n'),
		
	set_piece(BO,'C',7,4,BOBs),
	set_piece(BOBs,'C',3,4,BOB3),
	movable(BOB3,'P',4,4),
	print_board(BOB3),
	write('correct3\n').
	



test_sequencial(_) :-	
	sequential_cell(['P',' ',' ','B',' ',' ',' ',' '],'P'),write(['P',' ',' ','B',' ',' ',' ',' ']),write('correct\n'),
	sequential_cell([' ','P',' ','B',' ',' ',' ',' '],'P'),write([' ','P',' ','B',' ',' ',' ',' ']),write('correct\n'),
	sequential_cell([' ',' ','P','B',' ',' ',' ',' '],'P'),write([' ',' ','P','B',' ',' ',' ',' ']),write('correct\n'),
	not(sequential_cell([' ',' ','C','P','B',' ',' ',' '],'P')),write([' ',' ','C','P','B',' ',' ',' ']),write('correct\n'),
	sequential_cell([' ','C','P',' ',' ',' ',' ',' '],'P'),write([' ','C','P',' ',' ',' ',' ',' ']),write('correct\n'),
	not(sequential_cell(['P','C',' ',' ',' ',' ',' ',' '],'P')),write(['P','C',' ',' ',' ',' ',' ',' ']),write('correct\n').


test_reverse_diagonal(B) :-
	new_board(8,8,B),
	test_rev_diag_up(B),
	test_rev_diag_down(B).
test_rev_diag_up(B) :-	
	set_piece(B,'P', 4,6,OB),
	print_board(OB),
	reverse_diagonal_up(OB,4,6,BOB),
	print_board(BOB).


test_rev_diag_down(B) :-
	set_piece(B,'P', 2,6,OB),
	print_board(OB),
	reverse_diagonal_down(OB,2,6,BOB),
	print_board(BOB).
	


/*
%OLD
test_rev_down(B,IL,OL) :-
	new_board(8,8,B),
	set_piece(B,'P', 3,6,OB),
	print_board(OB),
	top_right(8,3,6,X,Y),	
	extract_reverse_diagonal(OB,X,Y,IL,OL,OBoard),	write('IL '),write(IL),write('*\n'),
	erase_horizontal_forward(IL,'P',OL),
	print_board(OBoard).
	%,erase_horizontal_forward([' ',' ',' ',' ','P',' ','+'],'P',OL).
test_rev_up(B,IL,OL) :-
	new_board(8,8,B),
	set_piece(B,'P', 6,3,OB),
	top_right(8,5,4,X,Y),
	extract_reverse_diagonal(OB,X,Y,IL,OL,OBoard),
	reverse(IL,RIL),
	erase_horizontal_forward(RIL,'P',ROL),
	reverse(ROL,OL).

test_reverse_diagonal_up(B) :-
	new_board(8,8,B),
	set_piece(B,'P', 6,3,OB),
	print_board(OB),
	reverse_diagonal_up(OB,'P',5,4,BOB),
	print_board(BOB).

*/






test_diagonal_down(B) :- 
	new_board(8,8,B),
	set_piece(B,'P', 3,3,OB),
	print_board(OB),
	diagonal_down(OB,'P',2,2,BOB),
	print_board(BOB).

	
test_diagonal_up(B) :- 
	new_board(8,8,B),
	set_piece(B,'P', 6,6,OB),
	print_board(OB),
	diagonal_up(OB,'P',7,7,BOB),
	print_board(BOB).

test_push(B):-test_push_f(B),test_push_b(B).

test_push_f(B) :- 	 
	new_board(8,8,B),
	set_piece(B,'P', 5,8,OB),
	set_piece(OB,'C', 3,8,BOB),
	print_board(BOB),
	%push_horizontal_forward(BOB,'P',1,BBOB),
	move_horizontal(BOB, 3,8,'P','push',BBOB),
	print_board(BBOB).
test_push_b(B) :-
	new_board(8,8,B), 
	set_piece(B,'P', 5,8,OB),
	set_piece(OB,'C', 6,8,BOB),
	print_board(BOB),
	%push_horizontal_backward(BOB,'P',1,BBOB),
	move_horizontal(BOB, 6,8,'P','push',BBOB),
	print_board(BBOB).


test_vertical(B) :-
	test_vertical_down(B),
	test_vertical_up(B).


test_vertical_down(B) :-
	new_board(8,8,B),
	set_piece(B,'P', 8,2,OB),
	print_board(OB),
	vertical_down(OB,'P',8,BOB),
	print_board(BOB).


test_vertical_up(B) :-
	new_board(8,8,B),
	set_piece(B,'P', 8,6,OB),
	print_board(OB),
	vertical_up(OB,'P',8,BOB),
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
	/*
test_push_f(B) :- 	 
	set_piece(B,'P', 5,1,OB),
	set_piece(OB,'C', 3,1,BOB),
	print_board(BOB),
	%push_horizontal_forward(BOB,'P',1,BBOB),
	move_horizontal(BOB, 3,1,'P','push',BBOB),
	print_board(BBOB).
	*/


test_pull_b(B) :- 
	set_piece(B,'P', 7,1,OB),
	set_piece(OB,'C', 3,1,BOB),
	print_board(BOB),
	%pull_horizontal_backward(BOB,'P',1,BBOB),
	move_horizontal(BOB, 3,1,'P','pull',BBOB),
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


