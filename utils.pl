% -*- mode: prolog -*-
:- module(utils, [read_lines/2, binary_number/2]).
:- use_module(library(clpfd)).

read_lines(Stream, []) :- at_end_of_stream(Stream).
read_lines(Stream, [X | L]) :-
    \+ at_end_of_stream(Stream),
    read_line_to_string(Stream, X),
    read_lines(Stream, L).

binary_number(Bs, N) :-
   binary_number_min(Bs, 0,N, N).

binary_number_min([], N,N, _M).
binary_number_min([B|Bs], N0,N, M) :-
   B in 0..1,
   N1 #= B+2*N0,
   M #>= N1,
   binary_number_min(Bs, N1,N, M).
