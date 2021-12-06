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

%%%

num_occurrences(L, X, N) :- aggregate(count, member(X, L), N), !.
num_occurrences(_, _, 0).

fish_count(Fish, Counts) :-
    findall(N, between(0, 8, N), Ns),
    maplist(num_occurrences(Fish), Ns, Counts),
    length(Counts, 9).

next_day_count([Zeros | Counts], NextDay) :-
    length(Counts, 8),
    nth0(7, Counts, Sevens),
    nth0(6, Counts, Sixes1),
    Sixes #= Sixes1 + Zeros,
    prefix(To6, Counts),
    prefix(To6, NextDay),
    append(To6, [Sixes, Sevens, Zeros], NextDay),
    length(NextDay, 9).

next_day_count(FishCounts, NextFishCounts, NumFish) :-
    next_day_count(FishCounts, NextFishCounts),
    sum_list(NextFishCounts, NumFish).

num_fish_at_day_count(InitFish, Day, NumFish) :-
    fish_count(InitFish, InitFishCounts),
    lazy_list(next_day_count, InitFishCounts, States),
    nth1(Day, States, NumFish).
