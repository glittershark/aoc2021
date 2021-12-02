% -*- mode: prolog -*-
:- use_module(library(readutil)).
:- use_module(library(lists)).
:- use_module(utils).

%%% Utils

string_number(S, N) :- number_string(N, S).

read_input(File, Numbers) :-
    open(File, read, Stream),
    read_lines(Stream, Lines),
    close(Stream),
    maplist(string_number, Lines, Numbers).

%%% Part 1

times_depth_increased([], 0).
times_depth_increased([_], 0).
times_depth_increased([X, Y | Rest], Out) :-
    (Y > X -> Incr = 1 ; Incr = 0),
    times_depth_increased([Y | Rest], Out1),
    Out is Out1 + Incr.

process_file_part1(File, Result) :-
    read_input(File, Nums),
    times_depth_increased(Nums, Result).

%%% Part 2

windows([], []).
windows([_], []).
windows([_, _], []).
windows([A, B, C | Rest], Out) :-
    windows([B, C | Rest], OutRest),
    Out = [[A, B, C] | OutRest].

window_sums(L, Out) :-
    windows(L, Windows),
    maplist(sum_list, Windows, Out).

process_file_part2(File, Result) :-
    read_input(File, Nums),
    window_sums(Nums, Sums),
    times_depth_increased(Sums, Result).
