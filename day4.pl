% -*- mode: prolog -*-

:- use_module(library(clpfd), [transpose/2]).
:- use_module(library(dcg/basics)).
:- use_module(library(dcg/high_order)).
:- use_module(library(yall)).

:- use_module(utils, [times/6]).

%%% Parsing

spaces --> " ", spaces.
spaces --> " ".

drawn_numbers(Nums) --> sequence(integer, ",", Nums).

board_row(Row) -->
    optional(" ", []),
    times(5, integer, spaces, Nums),
    { maplist(([N, unmarked - N] >> true), Nums, Row) }.

board_row_sep --> "\n".

board(Board) -->
    times(5, board_row, board_row_sep, Board).

game(game(Numbers, Boards)) -->
    drawn_numbers(Numbers),
    "\n\n",
    sequence(board, "\n\n", Boards),
    optional("\n", []).

read_game(Stream, Game) :-
    read_string(Stream, _, String),
    string_codes(String, Codes),
    phrase(game(Game), Codes).

read_game_from_file(File, Game) :-
    open(File, read, Stream),
    read_game(Stream, Game).

%%% Game logic

all_marked(Row) :-
    forall(member(Marked - _, Row), Marked = marked).

rows(Board, Board).
cols(Board, Cols) :- transpose(Board, Cols).

runs(Board, Runs) :-
    rows(Board, Rows),
    cols(Board, Cols),
    append(Rows, Cols, Runs).

won(Board) :-
    runs(Board, Runs),
    member(Run, Runs),
    all_marked(Run).

not_won(Board) :-
    runs(Board, Runs),
    forall(member(Run, Runs), member(unmarked - _, Run)).

mark_number(Number, _ - Number, marked - Number) :- !.
mark_number(Number, Marked - N, Marked - N) :- Number =\= N.

mark(Number, Board, NewBoard) :-
    maplist(maplist(mark_number(Number)), Board, NewBoard).

step_game(game([Num | Numbers], Bs), game(Numbers, NextBs), NextBs - Num) :-
    maplist(mark(Num), Bs, NextBs).

game_states(Game, GameStates) :-
    lazy_list(
        step_game,
        Game,
        GameStates
    ).

winning_board(Game, Board, LastNum) :-
    game_states(Game, GameStates),
    member(Boards - LastNum, GameStates),
    member(Board, Boards),
    won(Board).

board_score(Board, LastNum, Score) :-
    findall(N, (member(R, Board), member(unmarked - N, R)), UnmarkedNums),
    sum_list(UnmarkedNums, Sum),
    Score is LastNum * Sum.

%%% Part 2

pairs([], []).
pairs([_], []).
pairs([X, Y | L], [X - Y | Pairs]) :- pairs([Y | L], Pairs).

last_to_win(Game, Board, Num) :-
    game_states(Game, GameStates),
    pairs(GameStates, StatePairs),

    % Find the index at which all boards have won
    member((LastBoards - _) - (Boards - Num), StatePairs),
    forall(member(B, Boards), won(B)),

    % Then find the board that wasn't winning in LastBoards
    nth0(BoardIdx, LastBoards, LostBoard),
    not_won(LostBoard),

    % Use that as the index for the board that just won
    nth0(BoardIdx, Boards, Board).

%%% Tests

:- begin_tests(day4).

test(all_marked) :-
    all_marked(
        [marked - 14, marked - 21, marked - 17, marked - 24, marked - 4]
    ),
    \+ all_marked(
        [marked - 14, unmarked - 21, marked - 17, marked - 24, marked - 4]
       ).

test(mark_number) :-
    mark_number(1, unmarked - 1, marked - 1),
    mark_number(1, marked - 1, marked - 1),
    mark_number(2, marked - 1, marked - 1),
    mark_number(2, unmarked - 1, unmarked - 1).

:- end_tests(day4).
