
% Explanation for Program
% The program asks the user for a sudoku puzzle to solve
% The program will convert the user’s input into cell and add them a list: Board

% The cell (cell(Value, Notes, Col-Row, Square)) is made up of a few parts

% the 'final' Value of the cell
  % if the cell does not have an inputed value it will be initialized to 0
  % the Value can temporarily change to search for the solution in the run_Possibilities function

% Notes hold all the possible Values for a cell
  % if the cell has a Value (non-Zero) the cells Notes will be equal to the Value
      % cells with a Value equal to Zero- the Notes will be initialized to 0 but will be changed in findNotes
  % Notes can be changed in findNotes based on the Value of the the cells in the same Square/Row/Column

% Col-Row, Square is the location of the cell

% at any point in the next part of the process the board is solved it will end the program

% After the list is created it will create a Notes list for each cell
% then it will try to solve the puzzle by finding cells with only one member in the Notes list and
% changing its Value then creating new Notes lists for every cell and repeating the process again

% Sudoku puzzles on the easier side can be solved this way but for more complicated boards it will move to the next step
% it will find the shortest Notes list and guess a Value from the Notes list to be its new Value and
% create new Notes lists from that repeating the process from before
  % if the value seems to be right it will guess another Value repeating the process
      % if all possible Values from the are used it will return and change the Value the previous cell changed
  % if the Value seems to wrong it will change the Value of the cell to the next member of the Notes list

% Puzzles for input
% Empty if you want to make your own input
%[0,0,0,0,0,0,0,0,0].
%[0,0,0,0,0,0,0,0,0].
%[0,0,0,0,0,0,0,0,0].
%[0,0,0,0,0,0,0,0,0].
%[0,0,0,0,0,0,0,0,0].
%[0,0,0,0,0,0,0,0,0].
%[0,0,0,0,0,0,0,0,0].
%[0,0,0,0,0,0,0,0,0].
%[0,0,0,0,0,0,0,0,0].


%SOLVED
%[4,2,0,0,0,1,0,8,0].
%[9,0,0,4,0,5,0,0,1].
%[0,8,0,6,0,2,7,3,4].
%[0,0,0,1,0,4,0,0,8].
%[7,4,1,8,0,3,0,0,0].
%[0,5,3,0,6,0,4,0,0].
%[0,0,0,0,0,0,0,6,5].
%[6,0,0,0,0,9,2,4,3].
%[5,0,0,0,3,0,8,9,0].

%SOLVED
%[2,5,0,0,0,7,0,0,0].
%[0,4,0,0,5,6,0,0,2].
%[0,0,8,0,0,3,7,0,0].
%[4,0,6,0,3,9,0,0,0].
%[0,0,0,0,0,0,8,0,3].
%[1,3,0,4,0,0,0,5,0].
%[0,6,0,0,0,8,1,2,0].
%[0,0,1,6,0,0,5,3,7].
%[3,0,0,0,2,0,0,0,6].

%SOLVED
%[0,0,2,0,6,0,4,0,8].
%[0,9,8,0,0,1,0,0,0].
%[4,0,0,0,0,0,3,1,0].
%[0,0,7,2,0,3,1,0,0].
%[0,0,0,0,8,0,0,2,0].
%[0,0,0,0,0,7,6,0,4].
%[0,7,1,0,0,0,2,0,0].
%[0,0,0,0,0,0,0,6,3].
%[0,0,0,9,0,0,7,0,0].

%SOLVED
%[0,8,1,0,0,7,2,3,0].
%[0,0,0,0,0,0,9,8,0].
%[0,0,0,4,3,0,7,0,0].
%[2,0,0,0,0,0,0,0,0].
%[8,7,0,0,0,5,0,0,0].
%[0,3,0,0,0,0,5,0,0].
%[0,2,0,5,9,0,0,7,0].
%[1,9,8,0,0,2,4,0,0].
%[4,0,0,0,0,0,0,0,0].

%SOLVED
%[0,9,0,0,0,4,0,0,0].
%[0,7,0,0,8,0,0,1,3].
%[0,0,2,0,7,0,8,0,0].
%[8,0,0,0,0,0,0,0,0].
%[0,3,1,0,5,0,4,9,0].
%[0,0,0,0,0,0,0,0,6].
%[0,0,5,0,9,0,2,0,0].
%[7,1,0,0,4,0,0,6,0].
%[0,0,0,5,0,0,0,3,0].

%SOLVED
%[1,0,4,9,0,3,5,0,6].
%[0,0,0,0,0,0,0,0,0].
%[7,0,0,5,0,4,0,0,8].
%[5,0,3,0,0,0,7,0,9].
%[0,0,0,0,3,0,0,0,0].
%[4,0,6,0,0,0,2,0,1].
%[3,0,0,4,0,8,0,0,5].
%[0,0,0,0,0,0,0,0,0].
%[2,0,7,6,0,9,4,0,3].

%SOLVED
%[3,0,0,6,0,0,1,0,0].
%[0,0,1,0,0,7,0,0,0].
%[0,2,0,0,1,0,0,0,5].
%[1,0,0,3,0,0,0,4,0].
%[0,0,9,0,6,0,5,0,0].
%[0,5,0,0,0,8,0,0,2].
%[8,0,0,0,3,0,0,7,0].
%[0,0,0,4,0,0,3,0,0].
%[0,0,2,0,0,1,0,0,8].


% Start of Code--------------------------------------

%%% Starts Program %%%
start :-
    % gets user input %%%%%%%%%%
    write('Enter board (if cell is blank enter 0)'), nl,
    write('Ex. R1 = [1,0,3,5,6,0,9,0,0]'),
    nl,
    getUserInput(1, Board),
    printBoard(Board),
    no_Duplicates(Board), !,
    %%%%%%%%%%%%%%%%%%%%%%%%%%%

    findNotes(Board, NewBoard, _),                % will create a new board with accurate Notes for every cell
                                                  % if a cell only has one Note it will change the cells Value and
                                                  % loop the function until the board is solved or
                                                  % none of the cells have new Values after changing the notes

    % if board is not solved
    shortest_Notes(NewBoard, Col-Row, _-_, 10),   % Will find the cell with the shortest Notes list
    run_Possibilities(NewBoard, Col-Row).         % Will run all possibilities until it finds a solution

start :- % if the user enters input with duplicates in the same Square/Row/Col it will rerun start
    print('Invalid Input - Duplicate Found'), nl,
    start.


%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% Gets user input

getUserInput(10, []) :- !.

getUserInput(Row, NewNewBoard) :-
    write('R'),
    print(Row),
    write(' = '),
    read(Values),
    nl,
    length(Values, 9), !, % checks for correct list length
    add_to_Board(1-Row, Values, NewBoard),
    NewRow is Row + 1,
    getUserInput(NewRow, Board),
    append(Board, NewBoard, NewNewBoard).

getUserInput(Row, NewNewBoard) :- !, % handles incorrect list length
    print('Invalid Input - Invalid Row Length'), nl,
    getUserInput(Row, NewNewBoard).


% creates cells and adds them to the Board----------------

add_to_Board(10-_, [], []) :- !.

add_to_Board(Col-Row, [Value|Values], [cell(Value, Value, Col-Row, Square)|Board]) :- !,
    findSquare(Col-Row, Square),
    NewCol is Col + 1,
    add_to_Board(NewCol-Row, Values, Board).

%---------------------------------------------------------

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

printBoard(Board) :- !,
    nl,
    %prints row 1
    print_Line(9),
    write('1 |'),
    print_row(Board, 1-1),

    %prints row 2
    print_Line(9),
    write('2 |'),
    print_row(Board, 1-2),

    %prints row 3
    print_Line(9),
    write('3 |'),
    print_row(Board, 1-3),

    %prints row 4
    print_Line(9),
    write('4 |'),
    print_row(Board, 1-4),

    %prints row 5
    print_Line(9),
    write('5 |'),
    print_row(Board, 1-5),

    %prints row 6
    print_Line(9),
    write('6 |'),
    print_row(Board, 1-6),

    %prints row 7
    print_Line(9),
    write('7 |'),
    print_row(Board, 1-7),

    %prints row 8
    print_Line(9),
    write('8 |'),
    print_row(Board, 1-8),

    %prints row 9
    print_Line(9),
    write('9 |'),
    print_row(Board, 1-9),

    print_Line(9),

    write('   1   2   3   4   5   6   7   8   9 '),
    nl.

% Recursive loop to print a row ----------------
print_row(_, 10-_) :- !. % ends loop

print_row(Board, Col-Row) :- !, % recursive loop to print all the cells
    occupied_by(Board, Col-Row, Value),
    print_Cell(Value),
    NewCol is Col + 1,
    print_row(Board, NewCol-Row). % moves on to the next piece
%-----------------------------------------------

%prints cell --------
print_Cell(Value) :- !,
    write(' '),
    print(Value),
    write(' |').
%--------------------

% Recursive loop to print a horizontal line----------
print_Line(0) :- !, nl. % ends loop

print_Line(9) :- !, % prints first segment '  +---+'
    nl,
    write('  +---+'),

    NumOfCols_Left is 9 - 1,
    print_Line(NumOfCols_Left).

    print_Line(NumOfCols) :- % prints rest of line
    write('---+'),

    NumOfCols_Left is NumOfCols - 1,
    print_Line(NumOfCols_Left).

%------------------------------------------------------






%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

occupied_by(Board, Col-Row, Value) :- !, % can get value or location
    mymember(cell(Value, _, Col-Row, _), Board).

occupied_by(Board, Square, Value) :- !, % can get value or location
    mymember(cell(Value, _, _-_, Square), Board).

occupied_by_Notes(Board, Col-Row, Notes) :- !, % can get Notes or location
    mymember(cell(_, Notes, Col-Row, _), Board).

mymember(X, [X|_]) :- !.

mymember(X, [_|L]) :- !,
    mymember(X, L).

% Finds which square the cell is in
findSquare(Col-Row, 1) :-
    member(Col, [1,2,3]),
    member(Row, [1,2,3]), !.

findSquare(Col-Row, 2) :-
    member(Col, [4,5,6]),
    member(Row, [1,2,3]), !.

findSquare(Col-Row, 3) :-
    member(Col, [7,8,9]),
    member(Row, [1,2,3]), !.

findSquare(Col-Row, 4) :-
    member(Col, [1,2,3]),
    member(Row, [4,5,6]), !.

findSquare(Col-Row, 5) :-
    member(Col, [4,5,6]),
    member(Row, [4,5,6]), !.

findSquare(Col-Row, 6) :-
    member(Col, [7,8,9]),
    member(Row, [4,5,6]), !.

findSquare(Col-Row, 7) :-
    member(Col, [1,2,3]),
    member(Row, [7,8,9]), !.

findSquare(Col-Row, 8) :-
    member(Col, [4,5,6]),
    member(Row, [7,8,9]), !.

findSquare(Col-Row, 9) :-
    member(Col, [7,8,9]),
    member(Row, [7,8,9]), !.
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

% Checks if the puzzle is solved (aka if in every cell Value is not equal to zero)

is_solved(Board) :- !,
    \+ mymember(cell(0, _, _-_, _), Board).

% if the puzzle is solved it will end the program

if_solved_end_program(Board) :-
 \+ is_solved(Board), !. % if the Board is not solved then do nothing

 if_solved_end_program(Board) :-
     no_Duplicates(Board), !,
     nl, print('Solved Board!'),
     printBoard(Board),
     abort. % ends program

if_solved_end_program(_) :- !. % if cells have duplicates it does nothing (will be handled in run_Possibilities)



%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

no_Duplicates([]) :- !.

no_Duplicates([cell(X, _, _-_, _)|Rest]) :-
    X is 0, !, % if the cell Value is 0 it has no real Value so it does not need to be checked
    no_Duplicates(Rest).

no_Duplicates([cell(X, _, Col-Row, Square)|Rest]) :- !,
\+ (mymember(cell(X, _, _-Row, _),Rest);
    mymember(cell(X, _, Col-_, _),Rest);
    mymember(cell(X, _, _-_, Square),Rest)),
    no_Duplicates(Rest).


%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% changes notes and changes cells

% if the Board has duplicate values in the same Square/Row/Col it will not reiderate though findNotes
findNotes(Board, NewBoard, 0) :- % the 0 Value is to tell run_Possibilities that the board is false
    intersection(Board, 1-1, NewBoard, _),
    \+ no_Duplicates(NewBoard), !.

findNotes(Board, NewNewBoard, 1) :- !,
    intersection(Board, 1-1, NewBoard, Rerun), % rerun checks if any values are changed
    should_Rerun(Rerun, Bool),
    (Bool = 1 -> findNotes(NewBoard, NewNewBoard, _); % if a Value is changed it will run findNotes again
    Bool = 0 -> append([], NewBoard, NewNewBoard), if_solved_end_program(NewBoard)). % if a value is not changed it check if the puzzle is sovled and end the function



%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

intersection(_, 10-_, [], []) :- !.  % this ends the recursive function


intersection(Board, Col-10, NewBoard, Rerun) :- !,
    NewCol is Col + 1,
    intersection(Board, NewCol-1, NewBoard, Rerun).


% if the Col-Row already has a Value (non-zero) it doesn’t find the intersection for the cell and just adds the cell to NewBoard
intersection(Board, Col-Row, [cell(Value, Notes, Col-Row, Square)|NewBoard], Rerun) :-
      \+ occupied_by(Board, Col-Row, 0), !,

      occupied_by(Board, Col-Row, Value),
      occupied_by_Notes(Board, Col-Row, Notes),
      findSquare(Col-Row, Square),

      NewRow is Row + 1,
      intersection(Board, Col-NewRow, NewBoard, Rerun).

% if the cell doesn’t have a Value (so the Value = 0) it find the Notes list for that cell based on the intersection of the Values in the same Col/Row/Square
intersection(Board, Col-Row, [cell(Value, NewNotes, Col-Row, Square)|NewBoard], [Check|Rerun]) :- !,

    findSquare(Col-Row, Square),
    squareNotes(Board, Square, 1, SquareNotes),
    colNotes(Board, Col, 1, ColNotes),
    rowNotes(Board, Row, 1, RowNotes),
    checkIntersect(ColNotes, RowNotes, SquareNotes, Notes),

    new_Cell_Value(Board, Col-Row, [0|Notes], Value, Check),

    change_Notes(Check, Notes, NewNotes),

    NewRow is Row + 1,
    intersection(Board, Col-NewRow, NewBoard, Rerun).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

change_Notes(0, Notes, [0|Notes]):- !.

change_Notes(1, [NewNotes|_], NewNotes):- !.

should_Rerun(Rerun, 1) :-
mymember(1, Rerun), !.

should_Rerun(_, 0):- !.

%%%%%%%%%%%%%%%%%%%%%%%%

% if the Notes has only 1 non-zero member then it changes the Value of the cell
new_Cell_Value(_, _-_,[0|Item], Value, 1) :-
length(Item, Length),
Length is 1, !,
nth0(0, Item, Value).

% if the cell has more than 1 non-zero Value it leaves the cells value as 0
new_Cell_Value(_, _-_, _, 0, 0) :- !.


%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

% find the intersection of three list (IntersectionNotes is the intersection)
checkIntersect([], _, _, []) :- !.

% does not add Value to IntersectionNotes if HeadCol is not a member of RowNotes or SquareNotes
checkIntersect([HeadCol|ColNotes], RowNotes, SquareNotes, IntersectionNotes) :-
    (\+mymember(HeadCol, RowNotes);\+mymember(HeadCol, SquareNotes)), !,
    checkIntersect(ColNotes, RowNotes, SquareNotes, IntersectionNotes).

% if HeadCol is a member of both SquareNotes and RowNotes (by process of elimination) if will add the value to IntersectionNotes
checkIntersect([HeadCol|ColNotes], RowNotes, SquareNotes, [HeadCol|IntersectionNotes]) :- !,
    checkIntersect(ColNotes, RowNotes, SquareNotes, IntersectionNotes).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%


%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

% find the all possible values (Notes)
rowNotes(_,_,10,[]) :- !.

rowNotes(Board, Row, Value, RowNotes) :-
    occupied_by(Board, _-Row, Value), !,
    NewValue is Value + 1,
    rowNotes(Board, Row, NewValue, RowNotes).

rowNotes(Board, Row, Value, [Value|RowNotes]) :-
    NewValue is Value + 1,
    rowNotes(Board, Row, NewValue, RowNotes).


%%%%%%%%%%%%%%%%%%%%%%%%

colNotes(_,_,10,[]) :- !.

colNotes(Board, Col, Value, ColNotes) :-
    occupied_by(Board, Col-_, Value), !,
    NewValue is Value + 1,
    colNotes(Board, Col, NewValue, ColNotes).

colNotes(Board, Col, Value, [Value|ColNotes]) :-
    NewValue is Value + 1,
    colNotes(Board, Col, NewValue, ColNotes).


%%%%%%%%%%%%%%%%%%%%%%%%

squareNotes(_,_,10,[]) :- !.

squareNotes(Board, Square, Value, SquareNotes) :-
    occupied_by(Board, Square, Value), !,
    NewValue is Value + 1,
    squareNotes(Board, Square, NewValue, SquareNotes).

squareNotes(Board, Square, Value, [Value|SquareNotes]) :-
    NewValue is Value + 1,
    squareNotes(Board, Square, NewValue, SquareNotes).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%





%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%


run_Possibilities(Board, Col-Row):- !,

    change_Value(Board, Col-Row, NewBoard), !, % changes the Value to the first (non- zero) member of Notes of the cell at Col-Row
    occupied_by(NewBoard, Col-Row, Value), !,
    findNotes(NewBoard, NewNewBoard, _),
    check(NewNewBoard, NewNewBoard), % will loop run_Possibilities until it is solved unless the board is incorrect

    % if this path does not find the solution it will continue on with the function

    remove_Note(Board, Col-Row, Value, NewNewNewBoard), !, % will remove the changed Value from the Notes list
    does_Cell_Change_Value(NewNewNewBoard, Col-Row). % will call run_Possibilities with new value
                                                      % unless there is not more Values in the list it will return true and change the Value of the cell changed before it


%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

% Creates a board with all the members of the last board + a new cell

      add_Cell(Board, Value, Notes, Col-Row, Square, [cell(Value, Notes, Col-Row, Square)|Board]).

%---------------

% Creates a board without the cell at Col-Row

    % if the cell at Col-Row is found it will add the rest of Board to the new board
      remove_Cell([cell(Value, Notes, Col-Row, Square)|Board], Col-Row, Board) :- !.


    % else it will just add the individual cell to the new board and call the funtion again
      remove_Cell([cell(Value, Notes, Col-Row, Square)|Board], Dif_Col-Dif_Row, [cell(Value, Notes, Col-Row, Square)|NewBoard]) :- !,
            remove_Cell(Board, Dif_Col-Dif_Row, NewBoard).

%---------------


% Will remove a member (Value_to_Remove) of Notes list from the cell at Col-Row
% NewBoard is the changed board
remove_Note(Board, Col-Row, Value_to_Remove, NewBoard) :- !,
    occupied_by_Notes(Board, Col-Row, Notes), % gets Notes from cell at Col-Row
    findSquare(Col-Row, Square),              % gets Square from cell at Col-Row
    Value is 0,                               % Value is zero all cells with non-zero Values will not need to remove a note

    remove_member_of_list(Notes, Value_to_Remove, NewNotes), % Creates a new list without Value_to_Remove
    remove_Cell(Board, Col-Row, Board_With_Removed_Cell),    % Creates a new Board without the cell at Col-Row

    add_Cell(Board_With_Removed_Cell, Value, NewNotes, Col-Row, Square, NewBoard). % Creates a new Board re-adding the cell at Col-Row with the New Notes



%%%%%%%%%%%%%%%%%

remove_member_of_list([], _, []) :- !.

remove_member_of_list([Value_to_Remove|Rest], Value_to_Remove, NewNotes):- !,
    remove_member_of_list(Rest, Value_to_Remove, NewNotes).

remove_member_of_list([Value|Rest], Value_to_Remove, [Value|NewNotes]) :- !,
    remove_member_of_list(Rest, Value_to_Remove, NewNotes).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

change_Value(Board, Col-Row, NewNewBoard) :- !,
    occupied_by_Notes(Board, Col-Row, [0|[Value|_]]),
    findSquare(Col-Row, Square),
    remove_Cell(Board, Col-Row, NewBoard),
    add_Cell(NewBoard, Value, Value, Col-Row, Square, NewNewBoard).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

shortest_Notes([], TempCol-TempRow, TempCol-TempRow, _) :- !.

shortest_Notes([cell(_, [0,_,_], Col-Row, _)|_], Col-Row, _-_, _) :- !.

shortest_Notes([cell(_, Notes, Col-Row, _)|Board], Shortest_Col-Shortest_Row, _-_, Shortest) :-
    length(Notes, NewShortest),
    NewShortest < Shortest, !,
    shortest_Notes(Board, Shortest_Col-Shortest_Row, Col-Row, NewShortest).

shortest_Notes([cell(_, _, _-_, _)|Board], Shortest_Col-Shortest_Row, TempCol-TempRow, Shortest):- !,
    shortest_Notes(Board, Shortest_Col-Shortest_Row, TempCol-TempRow, Shortest).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

check(Board, Board) :-
\+ no_Duplicates(Board), !.

check([], Board) :- !,
    shortest_Notes(Board, Col-Row, _-_, 10), !,
    run_Possibilities(Board, Col-Row).

check([cell(_, [0], _-_, _)|_], _) :- !.

check([cell(_, _, _-_, _)|Rest], Board) :- !,
    check(Rest, Board).



%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
does_Cell_Change_Value(Board, Col-Row) :-
    occupied_by_Notes(Board, Col-Row, [0]), !.

does_Cell_Change_Value(Board, Col-Row) :-

      occupied_by_Notes(Board, Col-Row, [0|Value]),
      length(Value, Value_Length),
      Value_Length is 1,
      change_Value(Board, Col-Row, NewBoard),
      findNotes(NewBoard, _, 0).

does_Cell_Change_Value(Board, Col-Row) :-

    occupied_by_Notes(Board, Col-Row, [0|Value]),
    length(Value, Value_Length),
    Value_Length is 1, !,
    change_Value(Board, Col-Row, NewBoard),
    findNotes(NewBoard, NewNewBoard, 1),
    shortest_Notes(NewNewBoard, Shortest_Col-Shortest_Row, _-_, 10),
    run_Possibilities(NewNewBoard, Shortest_Col-Shortest_Row).

does_Cell_Change_Value(Board, Col-Row) :- !,
    run_Possibilities(Board, Col-Row).


%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

