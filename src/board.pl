%UTILS

%nth(Index to get, Input List, Output) 
nth(1,[IH|_],IH):- !.
nth(X,[_|OH],NTH) :- 
	LX is X-1, 
	nth(LX,OH,NTH).
	
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
	writeln('Quantas linhas pretende no tabuleiro (m�nimo 5, standard 8): '),
	read_int(YSize),
	writeln('Quantas colunas pretende no tabuleiro  (m�nimo 5, standard 8): '),
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
[  1   2   3   4   5   6   7   8
1['-',' ','C',' ','C',' ',' ','-'],
2['C',' ',' ',' ',' ',' ',' ',' '],
3['P',' ','C',' ',' ',' ',' ',' '],
4['C',' ',' ',' ','B',' ',' ',' '],
5[' ',' ','C',' ',' ',' ',' ',' '], 
6[' ',' ',' ',' ',' ',' ','C',' '], 
7[' ',' ',' ',' ',' ',' ',' ',' '], 
8['-',' ',' ',' ',' ',' ',' ','-']
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

%HORIZONTAL

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
	
move_diagonal(IBoard, XC,YC,Piece,PushPull,OBoard) :- 
	search_diagonal(IBoard,Piece,XC,YC,XP,YP),
	write('moving diagonal\n').


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
	push_h_f(IT,Piece,OT),
	!,true.
erase_horizontal_forward( [IH|IT], Piece, [IH|OT] ) :-
	erase_horizontal_forward(IT,Piece,OT).

%push_h_f(ILst,piece,OList).
push_h_f( [],_,_) :-
	!,false.
%push_h_f( [X],_,_) :-
%	!,false.

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



move_piece(B, X, Y, NX, NY, OB):-
  nth(Y,B,ROW),nth(X,ROW,PX),
  set_piece(B,PX,NX,NY,OB1),
  erase_piece(OB1,X,Y,OB).

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





/* OLD
%up => expl. y = [7..2]
reverse_diagonal_up(IBoard,Piece,X,Y,OBoard):- 
	nth(1,IBoard,Row),length(Row,N),
	top_right(N,X,Y,TX,TY), write('\nTX= '),write(TX),write(' TY= '),write(TY),write('\n'),
	extract_reverse_diagonal(IBoard,TX,TY,IList,OList,OBoard),write(IList),write(OList),write('extract\n'),
	reverse(IList,REV_IL),write(REV_IL),write('reverse1\n'),
	erase_horizontal_forward(REV_IL,Piece,REV_OL),write(REV_OL),write('erase\n'),
	reverse(REV_OL,OList),write('reverse2\n').
*/	
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




%test_movable_reverse_diagonal(B):-
test(B):-

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

%test_horizontal()
