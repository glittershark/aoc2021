%-*- mode: prolog -*-

:- use_module(library(clpfd)).
:- use_module(library(aggregate)).
:- use_module(library(dcg/high_order)).
:- use_module(library(dcg/basics)).

crab_range(Crabs, Min, Max) :-
    aggregate(max(Crab), member(Crab, Crabs), Max),
    aggregate(min(Crab), member(Crab, Crabs), Min).

diff(X, Y, Diff) :- Diff #= abs(X - Y).

total_fuel(Crabs, FinalPosition, TotalFuel) :-
    crab_range(Crabs, Min, Max),
    FinalPosition in Min..Max,
    maplist(diff(FinalPosition), Crabs, Fuel),
    sum(Fuel, #=, TotalFuel).

min_fuel(Crabs, FinalPosition, Fuel) :-
    total_fuel(Crabs, FinalPosition, Fuel),
    labeling([min(Fuel)], [FinalPosition, Fuel]).

crabs(Crabs) --> sequence(integer, ",", Crabs), eol.
