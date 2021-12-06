% -*- mode: prolog -*-

:- use_module(library(aggregate), [aggregate/3]).
:- use_module(library(clpfd)).
:- use_module(library(dcg/high_order)).
:- use_module(library(dcg/basics)).

step_fish(0, 6).
step_fish(N, N1) :-
    N #\= 0,
    N1 #= N - 1.

next_day([], []).
next_day(Fish, NextDay) :-
    maplist(step_fish, Fish, NextFish),
    ( aggregate(count, member(0, Fish), NumNewFish)
    ; NumNewFish = 0
    ),
    length(NewFish, NumNewFish),
    maplist(=(8), NewFish),
    append(NextFish, NewFish, NextDay).

next_day(Fish, NextFish, NumFish) :-
    next_day(Fish, NextFish),
    length(NextFish, NumFish).

num_fish_at_day(InitFish, Day, NumFish) :-
    lazy_list(next_day, InitFish, States),
    nth1(Day, States, NumFish).

%%%

fish(Fish) -->
    sequence(integer, ",", Fish),
    eol.
