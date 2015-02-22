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
%%		helper function, findAllMoves/3
%%		getting a list of all possible moves of the player
findAllMoves(Player, Opponent, PossMoves):-
    findall([A,B,MA,MB],
		(member([A,B], Player),
		 neighbour_position(A,B,[MA,MB]),
		 \+member([MA,MB],Player),
		 \+member([MA,MB],Opponent)),
		 PossMoves).

%%		helper function, generate_board/6
%%		generate the board based on the best move index
generate_board(MoveIndex, PossMoves, Player, Opponent, NewBoardState, Move):-
	nth1(MoveIndex, PossMoves, Move),
	alter_board(Move, Player, NewPlayer),
	next_generation([NewPlayer, Opponent], NewBoardState).

%%		helper function, try_generate_board/5
%%		generate the board for the current move
%%		used for selection of best move
try_generate_board(N, MoveList,
		   Player, Opponent, NewBoard):-
	nth1(N, MoveList, Move),
	alter_board(Move, Player, NewPlayer),
	next_generation([NewPlayer, Opponent], NewBoard).

%% 		helper function, pieces/3


%-------Part 2 , 1st proposition------
bloodlust(PlayerColour, [AliveB, AliveR], NewBoardState, Move):-
	(   PlayerColour == 'b'
	->  findBestMove(AliveB, AliveR, NewBoardState, Move)
	;   findBestMove(AliveR, AliveB, NewBoardState, Move)
	).

%%		helper function findBestMove/4
%%		used the feedback from loopThrough/7 to get the NewBoardState
findBestMove(Player, Opponent, NewBoardState, Move):-
	findAllMoves(Player, Opponent, PossMoves),
	length(PossMoves, L),
	loopThrough(L, PossMoves, Player, Opponent, 0, 64, MoveIndex),
	generate_board(MoveIndex, PossMoves,
		       Player, Opponent, NewBoardState, Move).

%%		helper function loopThough/7
%%		loop through each possible move
%%		to get minimum number of pieces of opponent
loopThrough(0, _, _, _, Best, _, Best).
loopThrough(N, MoveList, Player, Opponent, Counter, CompareTo, Best):-
	try_generate_board(N, MoveList, Player, Opponent, [_, NewOpponent]),
	length(NewOpponent, L),
	NewN is N-1,
	(   L < CompareTo
	->  loopThrough(NewN, MoveList, Player, Opponent, N, L, Best)
	;   loopThrough(NewN, MoveList, Player, Opponent, Counter, CompareTo, Best)
	).




%-------Part 2, 2nd proposition------
self_preservation(PlayerColour, [AliveB, AliveR], NewBoardState, Move):-
	(   PlayerColour == 'b'
	->  findBestMove2(AliveB, AliveR, NewBoardState, Move)
	;   findBestMove2(AliveR, AliveB, NewBoardState, Move)
	).

%%      helper function findBestMove2/4
%%		used the feedback from loopThrough2/7 to get the NewBoardState
findBestMove2(Player, Opponent, NewBoardState, Move):-
	findAllMoves(Player, Opponent, PossMoves),
	length(PossMoves, L),
	loopThrough2(L, PossMoves, Player, Opponent, 0, -1, MoveIndex),
	generate_board(MoveIndex, PossMoves,
		       Player, Opponent, NewBoardState, Move).

%%		helper function loopThough2/7
%%		loop through each possible move
%%		to get maximum number of pieces of player
loopThrough2(0, _, _, _, Best, _, Best).
loopThrough2(N, MoveList, Player, Opponent, Counter, CompareTo, Best):-
	try_generate_board(N, MoveList, Player, Opponent, [NextPlayer,_]),
	length(NextPlayer, L),
	NewN is N-1,
	(   L > CompareTo
	->  loopThrough2(NewN, MoveList, Player, Opponent, N, L, Best)
	;   loopThrough2(NewN, MoveList, Player,
			 Opponent, Counter, CompareTo, Best)
	).



%-------Part 2, 3rd proposition------
land_grab(PlayerColour, [AliveB, AliveR], NewBoardState, Move):-
	(   PlayerColour == 'b'
	->  findBestMove3(AliveB, AliveR, NewBoardState, Move)
	;   findBestMove3(AliveR, AliveB, NewBoardState, Move)
	).

%%      helper function findBestMove3/4
%%		used the feedback from loopThrough3/7 to get the NewBoardState
findBestMove3(Player, Opponent, NewBoardState, Move):-
	findAllMoves(Player, Opponent, PossMoves),
	length(PossMoves, L),
	loopThrough3(L, PossMoves, Player, Opponent, 0, -1, MoveIndex),
	generate_board(MoveIndex, PossMoves,
		       Player, Opponent, NewBoardState, Move).

%%		helper function loopThough3/7
%%		loop through each possible move
%%		to get maximum differnece between pieces of player and opponent
loopThrough3(0, _, _, _, Best, _, Best).
loopThrough3(N, MoveList, Player, Opponent, Counter, CompareTo, Best):-
	try_generate_board(N, MoveList, Player,
			   Opponent, [NextPlayer,NewOpponent]),
	length(NextPlayer, L1),
	length(NewOpponent, L2),
	Diff is L1 - L2,
	NewN is N-1,
	(   Diff > CompareTo
	->  loopThrough3(NewN, MoveList, Player, Opponent, N, Diff, Best)
	;   loopThrough3(NewN, MoveList, Player,
			 Opponent, Counter, CompareTo, Best)
	).


%-------Part 2, 4th proposition------
minimax(PlayerColour, [AliveB, AliveR], NewBoardState, Move):-
	(   PlayerColour == 'b'
	->  findBestMove(AliveB, AliveR, NewBoardState, Move, PlayerColour)
	;   findBestMove(AliveR, AliveB, NewBoardState, Move, PlayerColour)
	).

%%  	helper function findBestMove/5
%%		used the feedback from loopThrough4/7 to get the NewBoardState
findBestMove(Player, Opponent, NewBoardState, Move, PlayerColour):-
	findAllMoves(Player, Opponent, PossMoves),
	length(PossMoves, L),
	loopThrough(L, PossMoves, Player, Opponent, 0, -1, MoveIndex, PlayerColour),
	generate_board(MoveIndex, PossMoves,
		       Player, Opponent, NewBoardState, Move).
		       
%%		helper function loopThough/8
%%		loop through each possible move
%%		to get maximum pieces of player 
%%  	after the opponent makes a land_grab move
loopThrough(0, _, _, _, Best, _, Best, _).
loopThrough(N, MoveList, Player, Opponent, 
		Counter, CompareTo, Best, PlayerColour):-
	try_generate_board(N, MoveList, Player,
			   Opponent, NewBoardState),
	land_grab(PlayerColour, NewBoardState, [PredictPlayer, PredictOppo], Move),
	length(PredictPlayer, L1),
	length(PredictOppo, L2),
	NewN is N-1, 
	(L1 == 0 
	->
	 loopThrough(NewN, MoveList, Player,
			 Opponent, Counter, CompareTo, Best, PlayerColour) 
	; (L2 == 0
	  -> 
	  loopThrough(0, MoveList, Player,
			 Opponent, Counter, CompareTo, Counter, PlayerColour)
	  ; (   L1 > CompareTo
	->  loopThrough(NewN, MoveList, Player, Opponent, N, L1, Best, PlayerColour)
	;   loopThrough(NewN, MoveList, Player,
			 Opponent, Counter, CompareTo, Best, PlayerColour)
			 ) )
			 ).
			 
			 
			 
			 
			 
