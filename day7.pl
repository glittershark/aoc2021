%-*- mode: prolog -*-

:- use_module(library(clpfd)).
:- use_module(library(aggregate)).
:- use_module(library(dcg/high_order)).
:- use_module(library(dcg/basics)).

test_crabs([16,1,2,0,4,2,7,1,2,14]).

crabs(Crabs) --> sequence(integer, ",", Crabs), eol.

crab_range(Crabs, Min, Max) :-
    aggregate(max(Crab), member(Crab, Crabs), Max),
    aggregate(min(Crab), member(Crab, Crabs), Min).

diff(X, Y, Diff) :- Diff #= abs(X - Y).

total_fuel_part1(Crabs, FinalPosition, TotalFuel) :-
    crab_range(Crabs, Min, Max),
    FinalPosition in Min..Max,
    maplist(diff(FinalPosition), Crabs, Fuel),
    sum(Fuel, #=, TotalFuel).

min_fuel_part1(Crabs, FinalPosition, Fuel) :-
    total_fuel_part1(Crabs, FinalPosition, Fuel),
    labeling([bisect, min(Fuel)], [FinalPosition, Fuel]).

triangle(N, Triangle) :-
    Triangle #= N + ((N * (N - 1)) // 2).

fuel_part2(X, Y, Fuel) :-
    diff(X, Y, Diff),
    triangle(Diff, Fuel).

total_fuel_part2(Crabs, FinalPosition, TotalFuel):-
    crab_range(Crabs, Min, Max),
    FinalPosition in Min..Max,
    maplist(fuel_part2(FinalPosition), Crabs, Fuel),
    sum(Fuel, #=, TotalFuel).

min_fuel_part2(Crabs, FinalPosition, Fuel) :-
    total_fuel_part2(Crabs, FinalPosition, Fuel),
    labeling([bisect, min(Fuel)], [FinalPosition, Fuel]).
