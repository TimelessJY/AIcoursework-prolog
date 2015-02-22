initialState([[[1,1],[2,6],[3,4],[3,5],[3,8],[4,1],[4,2],[5,7],[6,3],[7,1],[7,3],[7,5]],
	[[1,8],[2,2],[2,8],[3,7],[4,6],[5,3],[6,6],[7,6],[7,7],[7,8],[8,3],[8,7]]]).


%-------Part 1 , question 3------

test_strategy(N,S1,S2):-
	testFor(N, S1, S2, 0, 0, 0, 0, 0, 0, 0, N).

%%		helper function: testFor/11, which loops N times,
%%		to get answers
testFor(0, _, _, NumDraw, NumWin1, NumWin2, Longest, Shortest, GameLength, GameTime, NumPlays):-
	GL = GameLength / NumPlays,
	GT = GameTime / NumPlays,
	format(' ~d draws, ~n ~d wins for player 1, ~n ~d wins for player 2, ~n longest game has ~d moves, ~n shortest game has ~d moves, ~n average game length is ~2f, ~n average game time is ~d', [NumDraw, NumWin1, NumWin2, Longest, Shortest,GL, GT]),!.

testFor(N,S1,S2,NumDraw, NumWinB,NumWinR,Longest, Shortest, GameLength, GameTime, NumPlays):-
	play(quiet, S1, S2, NumMove, WinP),
	(   WinP == 'draw'
	->  NewDraw is NumDraw + 1,
	    WinB is NumWinB,
	    WinR is NumWinR
	;   (WinP == 'r'
	->  NewDraw is NumDraw,
	    WinB is NumWinB,
	    WinR is NumWinR + 1
	;   NewDraw is NumDraw,
	    WinB is NumWinB + 1,
	    WinR is NumWinR
	    )
	),
	(   NumMove > Longest
	->  NewL is NumMove
	;   NewL is Longest
	),
	(   NumMove < Shortest
	->  NewS is NumMove
	;   (Shortest == 0
	->  NewS is NumMove
	;   NewS is Shortest)
	),
	GL is GameLength + NumMove,
	GT is GameTime + 1,
	NewN is N - 1,
	testFor(NewN, S1, S2, NewDraw, WinB, WinR, NewL, NewS, GL, GT, NumPlays).


%------------Part 2 --------------------------
%%	helper function, opponent/2
%%	given the colour of the player(1st argument),
%%	return the colour of the opponent(2nd argument)
opponent('b', 'r').
opponent('r', 'b').


%%	helper function, numPieces/3
%%	getting the number of pieces of the given colour
numPieces('b', [Blues, _], L) :-
	length(Blues, L).

numPieces('r', [_, Reds], L) :-
	length(Reds, L).


%%	helper function, findAllMoves/3
%%	getting a list of all possible moves of the player
findAllMoves('b', [Blues, Reds], PossMoves):-
	findall([A,B,MA,MB],
		(member([A,B], Blues),
		 neighbour_position(A,B,[MA,MB]),
		 \+member([MA,MB],Blues),
		 \+member([MA,MB],Reds)),
		 PossMoves).

findAllMoves('r', [Blues, Reds], PossMoves):-
	findall([A,B,MA,MB],
		(member([A,B], Reds),
		 neighbour_position(A,B,[MA,MB]),
		 \+member([MA,MB],Reds),
		 \+member([MA,MB],Blues)),
		 PossMoves).

%%	helper function, make_move/4
%%	make the given move, calling alter_board
make_move(Move, [Blues, Reds], [NewBlues, Reds], 'b'):-
	alter_board(Move, Blues, NewBlues).
make_move(Move, [Blues, Reds], [Blues, NewReds], 'r'):-
	alter_board(Move, Reds, NewReds).



%%	helper function, generate_board/5
%%	generate the board based on the best move index
generate_board(MoveIndex, MoveList,
		   Board, NewBoard, Colour):-
	nth1(MoveIndex, MoveList, Move),
	make_move(Move, Board, AfterMove, Colour),
	next_generation(AfterMove, NewBoard).


%-------Part 2 , 1st proposition------
% in order to get the minimum pieces of opponent,
% set the initial compareTo value as 65,
% because any valid number of pieces should be less than 65
bloodlust(Colour, Board, NewBoardState, Move):-
	findAllMoves(Colour, Board, PossMoves),
	length(PossMoves, L),
	loopThrough(L, PossMoves, Board, 0, 65, MoveIndex, Colour),
	generate_board(MoveIndex, PossMoves,
		       Board, NewBoardState, Move).


%%	helper function loopThough/7
%%	loop through each possible move
%%	to get minimum number of pieces of opponent
loopThrough(0, _, _, Best, _, Best, _).
loopThrough(N, MoveList, Board, Counter, CompareTo, Best, Colour):-
	generate_board(N, MoveList, Board, NewBoard, Colour),
	opponent(Colour, OppoC),
	numPieces(OppoC, NewBoard, L),
	NewN is N-1,
	(   L < CompareTo
	->  loopThrough(NewN, MoveList, Board, N, L, Best, Colour)
	;   loopThrough(NewN, MoveList,	Board, Counter, CompareTo, Best, Colour)
	).




%-------Part 2 , 2nd proposition------
% in order to get the maximum pieces of player,
% set the initial compareTo value as -1,
% because any valid number of pieces should be larger than -1

self_preservation(Colour, Board, NewBoardState, Move):-
	findAllMoves(Colour, Board, PossMoves),
	length(PossMoves, L),
	loopThrough2(L, PossMoves, Board, 0, -1, MoveIndex, Colour),
	generate_board(MoveIndex, PossMoves,
		       Board, NewBoardState, Move).


%%	helper function loopThough2/7
%%	loop through each possible move
%%	to get maximum number of pieces of player
loopThrough2(0, _, _, Best, _, Best, _).
loopThrough2(N, MoveList, Board, Counter, CompareTo, Best, Colour):-
	generate_board(N, MoveList, Board, NewBoard, Colour),
	numPieces(Colour, NewBoard, L),
	NewN is N-1,
	(   L > CompareTo
	->  loopThrough2(NewN, MoveList, Board, N, L, Best, Colour)
	;   loopThrough2(NewN, MoveList, Board, Counter, CompareTo, Best, Colour)
	).




%-------Part 2 , 3rd proposition------
% as looking for the maximum difference between the two players,
% set the initial difference as -65,
% so that any valid difference must be larger than -65
land_grab(Colour, Board, NewBoardState, Move):-
	findAllMoves(Colour, Board, PossMoves),
	length(PossMoves, L),
	loopThrough3(L, PossMoves, Board, 0, -65, MoveIndex, Colour),
	generate_board(MoveIndex, PossMoves,
		       Board, NewBoardState, Move).

%%	helper function loopThough3/7
%%	loop through each possible move
%%	to get maximum difference between pieces of player and oponent
loopThrough3(0, _, _, Best, _, Best, _).
loopThrough3(N, MoveList, Board, Counter, CompareTo, Best, Colour):-
	generate_board(N, MoveList, Board, NewBoard, Colour),
	numPieces(Colour, NewBoard, L1),
	opponent(Colour, OppoC),
	numPieces(OppoC, NewBoard, L2),
	Diff is L1 - L2,
	NewN is N-1,
	(   Diff > CompareTo
	->  loopThrough3(NewN, MoveList, Board, N, Diff, Best, Colour)
	;   loopThrough3(NewN, MoveList, Board, Counter, CompareTo, Best, Colour)
	).




%-------Part 2 , 4th proposition------
minimax(Colour, Board, NewBoardState, Move):-
	findAllMoves(Colour, Board, PossMoves),
	length(PossMoves, L),
	loopThrough4(L, PossMoves, Board, 0, -65, MoveIndex, Colour),
	generate_board(MoveIndex, PossMoves,
		       Board, NewBoardState, Move).

%%	helper function loopThough4/7
%%	loop through each possible move
%%	to get max pieces of player
%%	after the player makes a land_grab move
loopThrough4(0, _, _, Best, _, Best, _).
loopThrough4(N, MoveList, Board, Counter, CompareTo, Best, Colour):-
	generate_board(N, MoveList, Board, NewBoard, Colour),
	opponent(Colour, OppoC),
	land_grab(OppoC, NewBoard, PredictBoard, _),
	numPieces(Colour, PredictBoard, L1),
	numPieces(OppoC, PredictBoard, L2),
	NewN is N-1,
	(   L1 == 0
	%%  this is the worst case, so skip
	->  (CompareTo < 0
	    ->
	    loopThrough4(NewN, MoveList, Board, N, L1, Best, Colour)
	    ;
	    loopThrough4(NewN, MoveList, Board, Counter, CompareTo, Best, Colour)
	    )
	;   (L2 == 0
	    %% best case, so finish
	    ->
	    loopThrough4(0, MoveList, Board, Counter, CompareTo, Best, Colour)
	    ;	(L1 > CompareTo
		%% best case up till now, so keep checking
		->
		loopThrough4(NewN, MoveList, Board, N, L1, Best, Colour)
		;
		loopThrough4(NewN, MoveList, Board,
			     Counter, CompareTo, Best,Colour)
		)
	    )
	).
