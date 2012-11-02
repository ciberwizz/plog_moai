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



%nth(Index to get, Input List, Output) 
nth(1,[IH|_],IH):- !.
nth(X,[_|OH],NTH) :- LX is X-1, nth(LX,OH,NTH).

%check if it's vacant...
vacant(X,Y,Board) :- nth(Y,Board,Row),
		nth(X,Row,' ').

% set piece on X Y in Board 
set_piece( IBoard, Piece, X, Y, OBoard) :- vacant(X,Y,IBoard),
					set_y_piece(IBoard, Piece, X, Y, 1, OBoard),!.

set_y_piece( [IH|IT], P, X, Y,  Y, [OH|IT]):- set_x_piece(IH,P,X,1,OH),!.
set_y_piece( [IH|IT], P, X, Y,  LY, [IH|OT]) :- LYY is LY+1, 
					set_y_piece(IT,P,X,Y,LYY,OT).

set_x_piece( [_|IT], P, X, X, [P|IT] ):- !.
set_x_piece( [IH|IT], P, X, LX, [IH|OT] ):- XLX is LX+1, 
					set_x_piece(IT,P,X,XLX,OT).



print_board( [LastLine | [] ] ):- print_line( LastLine), write('\n\n'), !.
print_board( [Line | Lines ] ):- print_line( Line), print_separator(Line), 
		print_board(Lines).

print_line( [LastPiece | [] ] ):-  write(' '), write(LastPiece), write('\n'), !.
print_line( [Piece | Pieces ] ):- write(' '), write(Piece), write(' |'), 
		print_line( Pieces ).

print_separator( [_|[]] ):- write('---\n'), !.
print_separator( [_|T] ):- write('---|'), print_separator(T).

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


%search_diagonal(Board,Piece_to_Search, XCounter,YCounter,XPiece,YPiece) =>output:xpiece,ypiece
search_diagonal(Board, P, XC,YC,XP,YP) :- search_diagonal_forward(Board, P, XC,YC,XP,YP).
search_diagonal(Board, P, XC,YC,XP,YP) :- search_diagonal_backward(Board, P, XC,YC,XP,YP).
search_diagonal(Board, P, XC,YC,XP,YP) :- search_reverse_diagonal_forward(Board, P, XC,YC,XP,YP).
search_diagonal(Board, P, XC,YC,XP,YP) :- search_reverse_diagonal_backward(Board, P, XC,YC,XP,YP).


search_diagonal_forward(Board, P, XC,YC,XC,YC) :- nth(YC,Board,Row),
						nth(XC,Row,P),
						write('found').
						
search_diagonal_forward(Board, P, XC,YC,XP,YP) :- TXC is XC + 1,
						TYC is YC + 1,
						search_diagonal_forward(Board, P, TXC,TYC,XP,YP).

search_diagonal_backward(Board, P, XC,YC,XC,YC) :- nth(YC,Board,Row),
						nth(XC,Row,P),
						write('found').
						
search_diagonal_backward(Board, P, XC,YC,XP,YP) :- TXC is XC - 1,
						TYC is YC - 1, 
						search_diagonal_backward(Board, P, TXC,TYC,XP,YP).
						
%search_reverse_diagonal(Board,Piece_to_Search, XCounter,YCounter,XPiece,YPiece) =>output:xpiece,ypiece
search_reverse_diagonal(Board, P, XC,YC,XP,YP) :- search_reverse_diagonal_forward(Board, P, XC,YC,XP,YP).
search_reverse_diagonal(Board, P, XC,YC,XP,YP) :- search_reverse_diagonal_backward(Board, P, XC,YC,XP,YP).


search_reverse_diagonal_forward(Board, P, XC,YC,XC,YC) :- nth(YC,Board,Row),
						nth(XC,Row,P),
						write('found').
						
search_reverse_diagonal_forward(Board, P, XC,YC,XP,YP) :- TXC is XC - 1,
						TYC is YC + 1,
						search_reverse_diagonal_forward(Board, P, TXC,TYC,XP,YP).

search_reverse_diagonal_backward(Board, P, XC,YC,XC,YC) :- nth(YC,Board,Row),
						nth(XC,Row,P),
						write('found').
						
search_reverse_diagonal_backward(Board, P, XC,YC,XP,YP) :- TXC is XC + 1,
						TYC is YC - 1, 
						search_reverse_diagonal_backward(Board, P, TXC,TYC,XP,YP).

%search_horizontal(Board,Piece_to_Search, XCounter,YCounter,XPiece,YPiece) =>output:xpiece,ypiece
search_horizontal(Board, P, XC,YC,XP,YP) :- search_horizontal_forward(Board, P, XC,YC,XP,YP).
search_horizontal(Board, P, XC,YC,XP,YP) :- search_horizontal_backward(Board, P, XC,YC,XP,YP).


search_horizontal_forward(Board, P, XC,YC,XC,YC) :- nth(YC,Board,Row),
						nth(XC,Row,P),
						write('found').
						
search_horizontal_forward(Board, P, XC,YC,XP,YP) :- TXC is XC + 1,
						search_horizontal_forward(Board, P, TXC,YC,XP,YP).

search_horizontal_backward(Board, P, XC,YC,XC,YC) :- nth(YC,Board,Row),
						nth(XC,Row,P),
						write('found').
						
search_horizontal_backward(Board, P, XC,YC,XP,YP) :- TXC is XC - 1,
						search_horizontal_backward(Board, P, TXC,YC,XP,YP).


%search_vertical(Board,Piece_to_Search, XCounter,YCounter,XPiece,YPiece) =>output:xpiece,ypiece
%com o OR logico
%search_vertical(Board, P, XC,YC,XP,YP) :- search_vertical_forward(Board, P, XC,YC,XP,YP) ;
%					search_vertical_backward(Board, P, XC,YC,XP,YP).
search_vertical(Board, P, XC,YC,XP,YP) :- search_vertical_forward(Board, P, XC,YC,XP,YP).

search_vertical(Board, P, XC,YC,XP,YP) :- search_vertical_backward(Board, P, XC,YC,XP,YP).


search_vertical_forward(Board, P, XC,YC,XC,YC) :- nth(YC,Board,Row),
						nth(XC,Row,P),
						write('found').
						
search_vertical_forward(Board, P, XC,YC,XP,YP) :- TYC is YC + 1,
						search_vertical_forward(Board, P, XC,TYC,XP,YP).

search_vertical_backward(Board, P, XC,YC,XC,YC) :- write('checking\n'),nth(YC,Board,Row),
						nth(XC,Row,P),
						write('found').
						
search_vertical_backward(Board, P, XC,YC,XP,YP) :- TYC is YC - 1, write('vertival'), 
						search_vertical_backward(Board, P, XC,TYC,XP,YP).



search_piece(Board,P,XC,YC,XP,YP) :- search_diagonal(Board, P, XC,YC,XP,YP).
search_piece(Board,P,XC,YC,XP,YP) :- search_horizontal(Board, P, XC,YC,XP,YP).
search_piece(Board,P,XC,YC,XP,YP) :- search_vertical(Board, P, XC,YC,XP,YP).


test(X,Y,BOB) :- new_board(8,8,B), 
		set_piece(B,'P', X,Y,OB),
		CX is X, CY is Y-3,
		set_piece(OB,'C', CX,CY,BOB),
		print_board(BOB),
		search_vertical(BOB,'P',CX,CY,_,_).
		
		
		

%mover(Input_Board, XCounter, YCounter, Piece_To_move, PushPull, Output_Board).
%mover(IBoard, XC, YC, Piece, PushPull, OBoard) :- move_diagonal(IBoard, XC,YC,Piece,PushPull,OBoard).
%mover(IBoard, XC, YC, Piece, PushPull, OBoard) :- move_horizontal(IBoard, XC,YC,Piece,PushPull,OBoard).
%mover(IBoard, XC, YC, Piece, PushPull, OBoard) :- move_vertical(IBoard, XC,YC,Piece,PushPull,OBoard).
		