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
	push_horizontal_forward(IBoard, Piece,YP,OBoard),
	write('moving push horizontal forward\n').

move_horizontal(IBoard, XC,YC,Piece,'pull',OBoard) :- 
	search_horizontal_backward(IBoard,Piece,XC,YC,_,YP),
	pull_horizontal_forward(IBoard, Piece,YP,OBoard),
	write('moving pull horizontal forward\n').
	
move_horizontal(IBoard, XC,YC,Piece,'push',OBoard) :- 
	search_horizontal_backward(IBoard,Piece,XC,YC,_,YP),
	write('moving push horizontal backward\n'),
	push_horizontal_backward(IBoard, Piece,YP,OBoard).

move_horizontal(IBoard, XC,YC,Piece,'pull',OBoard) :- 
	search_horizontal_forward(IBoard,Piece,XC,YC,_,YP),
	pull_horizontal_backward(IBoard, Piece,YP,OBoard),
	write('moving pull horizontal backward\n').




move_diagonal(IBoard, XC,YC,Piece,PushPull,OBoard) :- 
	search_diagonal(IBoard,Piece,XC,YC,XP,YP),
	write('moving diagonal\n').

move_vertical(IBoard, XC,YC,Piece,PushPull,OBoard) :- 
	search_vertical(IBoard,Piece,XC,YC,XP,YP),
	write('moving vertical\n').	
	


/*
 * horizontal move predicates..
 */

push_horizontal_forward(IBoard, Piece, YPiece,OBoard) :-
	horizontal_forward(IBoard, Piece, YPiece,OBoard).
	
pull_horizontal_forward(IBoard, Piece, YPiece,OBoard) :-
	horizontal_forward(IBoard, Piece, YPiece,OBoard).
	
pull_horizontal_backward(IBoard, Piece, YPiece,OBoard) :-
	horizontal_backward(IBoard, Piece, YPiece,OBoard).
	
push_horizontal_backward(IBoard, Piece, YPiece,OBoard) :-
	horizontal_backward(IBoard, Piece, YPiece,OBoard).


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

push_h_f( [' '|[]], Piece, Piece):- 
	!,true.
push_h_f( [' ',' '|IT],Piece,[' '|OT]) :- 
	push_h_f( [' '|IT], Piece, OT). 
push_h_f( [' ',X|IT], Piece, [Piece,X|IT]):- 
	!,true.	
push_h_f( [_|_],_,_) :- 
	!, false.


/*
 * Vertical Move predicates
 */ 











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