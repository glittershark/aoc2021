% -*- mode: prolog -*-

:- use_module(library(dcg/basics), [integer/3, eol/2]).
:- use_module(library(dcg/high_order)).
:- use_module(library(clpfd)).

:- use_module(utils, [phrase_file/2]).

%%% Parsing

point(X - Y) -->
    integer(X),
    ",",
    integer(Y).

line(Start - End) -->
    point(Start),
    " -> ",
    point(End),
    eol.

lines(Lines) --> sequence(line, Lines).

lines_from_file(File, Lines) :-
    phrase_file(lines(Lines), File).

%%%

line_points_horizontal((X-Y)-(X-Y), [X-Y]) :- !.
line_points_horizontal((X1-Y)-(X2-Y), [X1-Y | Points]) :-
    X2 #> X1, !,
    Xnext #= X1 + 1,
    line_points_horizontal((Xnext-Y)-(X2-Y), Points).
line_points_horizontal((X-Y1)-(X-Y2), [X-Y1 | Points]) :-
    Y2 #> Y1, !,
    Ynext #= Y1 + 1,
    line_points_horizontal((X-Ynext)-(X-Y2), Points).
% Consider reversed horizontal lines
line_points_horizontal(Start-End, Points) :-
    (X1-Y1)-(X2-Y2) = Start-End,
    (X1 #= X2 ; Y1 #= Y2),
    (X1 #> X2 ; Y1 #> Y2), !,

    line_points_horizontal(End-Start, ReversePoints),
    reverse(ReversePoints, Points).
line_points_horizontal(_, []). % Ignore diagonal lines

line_points_diagonal(Pt-Pt, [Pt]) :- !.
line_points_diagonal((X1-Y1)-(X2-Y2), []) :- (X1 #= X2 ; Y1 #= Y2), !.

line_points_diagonal((X1-Y1)-(X2-Y2), [X1-Y1 | Points]) :-
    ( (X1 #< X2, Xnext is X1 + 1)
    ; (X1 #> X2, Xnext is X1 - 1)
    ),
    ( (Y1 #< Y2, Ynext is Y1 + 1)
    ; (Y1 #> Y2, Ynext is Y1 - 1)
    ), !,
    line_points_diagonal((Xnext-Ynext)-(X2-Y2), Points).

:- table line_points/3.
line_points(horizontal, Line, Points) :- line_points_horizontal(Line, Points).
line_points(diagonal, Line, Points) :- line_points_diagonal(Line, Points).
line_points(any, Line, Points) :-
    ( (line_points(horizontal, Line, Points), [_|_] = Points)
    ; (line_points(diagonal, Line, Points), [_|_] = Points)
    ).

%%%

%%%

:- table two_lines_overlap_at/2.
two_lines_overlap_at(LinePoints, OverlappingPoint) :-
    % this is exponential, unsurprisingly
    select(Pts1, LinePoints, RestPoints),
    member(Pts2, RestPoints),
    member(OverlappingPoint, Pts1),
    member(OverlappingPoint, Pts2).

num_overlapping_points(Dir, Lines, Num) :-
    maplist(line_points(Dir), Lines, LinePoints),
    setof(Point, two_lines_overlap_at(LinePoints, Point), Points),
    length(Points, Num).

%%%
