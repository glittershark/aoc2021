% -*- mode: prolog -*-

:- use_module(library(dcg/basics)).
:- use_module(library(dcg/high_order)).
:- use_module(library(aggregate)).
:- use_module(library(assoc)).
:- use_module(library(clpfd)).
:- use_module(utils, [lazy_sequence/5]).

canonical_number_(0, [a, b, c, e, f, g]).
canonical_number_(1, [c, f]).
canonical_number_(2, [a, c, d, e, g]).
canonical_number_(3, [a, c, d, f, g]).
canonical_number_(4, [b, c, d, f]).
canonical_number_(5, [a, b, d, f, g]).
canonical_number_(6, [a, b, d, e, f, g]).
canonical_number_(7, [a, c, f]).
canonical_number_(8, [a, b, c, d, e, f, g]).
canonical_number_(9, [a, b, c, d, f, g]).

canonical_number(N, L) :-
    ground(L), !,
    sort(L, Sorted),
    canonical_number_(N, Sorted).
canonical_number(N, L) :- canonical_number_(N, L).
canonical_number(N, L) :-
    ground(N), !,
    canonical_number_(N, P),
    permutation(L, P).

number(N, L) :-
    length(L, Len),
    canonical_number(N, CanonicalL),
    length(CanonicalL, Len).

unique_number(N, L) :-
    findall(N, number(N, L), Ns),
    length(Ns, 1).

wire(a). wire(b). wire(c). wire(d). wire(e). wire(f). wire(g).
wire(Wire) -->
    [C],
    { atom_codes(Wire, [C]),
      wire(Wire)
    }.

signal(Pattern) --> sequence(wire, Pattern).

signals(Signals) --> lazy_sequence(signal, " ", Signals).

entry(entry(SignalPatterns, OutputValues)) -->
    signals(SignalPatterns),
    " | ",
    signals(OutputValues).

entries(Entries) -->
    lazy_sequence(entry, "\n", Entries),
    eol.

%%%

entry_signal_patterns(entry(SignalPatterns, _), SignalPatterns).
entry_output_values(entry(_, OutputValues), OutputValues).

%%%

num_unique_numbers(Entries, Num) :-
    findall(
        Num,
        (
            member(Entry, Entries),
            entry_output_values(Entry, OutputValues),
            member(Signal, OutputValues),
            unique_number(Num, Signal)
        ),
        Nums
    ),
    length(Nums, Num).

%%%

number_of_digits(N, 1) :- N in 0..9.
number_of_digits(N, Digits) :-
    N1 #= N div 10,
    number_of_digits(N1, Digits1),
    Digits is Digits1 + 1.

num_digits_(Num, [Num]) :- Num in 0..9.
num_digits_(Num, [D | Ds]) :-
    number_of_digits(Num, Digits),
    Position #= Digits - 1,
    length(Ds, Position),
    D #= Num div (10 ^ Position),
    Num1 #= Num mod (10 ^ Position),
    num_digits(Num1, Ds).

num_digits_(Num, [D | Ds]) :-
    num_digits(Num1, Ds),
    length(Ds, Position),
    Num #= (D * 10 ^ (Position)) + Num1.

num_digits(Num, Digits) :- num_digits_(Num, Digits), !.


%%%

maps_to(Mapping, A, B) :-
    member(A - B, Mapping).

signal_num(Mapping, Signal, Num) :-
    number(Num, Signal),
    maplist(maps_to(Mapping), Signal, RemappedSignal),
    canonical_number(Num, RemappedSignal).

entry_num(entry(SignalPatterns, OutputValues), Num) :-
    Mapping = [a-_, b-_, c-_, d-_, e-_, f-_, g-_],
    maplist(signal_num(Mapping), SignalPatterns, _),
    maplist(signal_num(Mapping), OutputValues, Digits),
    num_digits(Num, Digits).

answer_part2(Entries, Answer) :-
    maplist(entry_num, Entries, Nums),
    sum(Nums, #=, Answer).
