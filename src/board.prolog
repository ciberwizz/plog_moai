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

blank_first_corners( [_|IT], ['-'|OT] ):- blank_corner(IT,OT).

blank_corner( [_|[]], ['-'|[]] ):- !.
blank_corner( [IH|IT] ,[IH|OT] ):- blank_corner(IT,OT).

blank_last_corners(Oline, Ilines, [Oline|OT] ):- blank_last(Ilines,OT).

blank_last( [IH|[]], [OH|[]] ):- blank_first_corners(IH,OH), !.
blank_last( [IH|IT], [IH|OT] ):- blank_last(IT,OT).

%-----------------\\----------

% set piece on X Y in Board 

% TODO
set_piece( IBoard, Piece, X, Y, OBoard) :- set_y_piece(IBoard, Piece, X, Y, 1, OBoard).

set_y_piece( [IH|IT], P, X, Y,  Y, [OH|IT]):- write('====\n'),set_x_piece(IH,P,X,1,OH),!.
set_y_piece( [IH|IT], P, X, Y,  LY, [IH|OT]) :- write('\n*'),write(LY),LYY is LY+1, set_y_piece(IT,P,X,Y,LYY,OT).

set_x_piece( [_|IT], P, X, X, [OH|IT] ):- write('<=>\n'),OH = P,!.
set_x_piece( [IH|IT], P, X, LX, [IH|OT] ):- write('<>\n'), LX is LX+1, set_x_piece(IT,P,X,LX,OT).



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



%mover(InBoard, XCounter, YCounter, Cor, PushPull, OutBoard).



