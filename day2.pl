% -*- mode: prolog -*-
:- set_prolog_flag(double_quotes, chars).
:- use_module(library(dcg/basics), [integer/3]).
:- use_module(utils).

command(forward, Amount) -->
    "forward ",
    integer(Amount).

command(down, Amount) -->
    "down ",
    integer(Amount).

command(up, Amount) -->
    "up ",
    integer(Amount).

string_command(String, Direction - Amount) :-
    string(String), !,
    string_chars(String, Chars),
    phrase(command(Direction, Amount), Chars).

string_command(Chars, Direction - Amount) :-
    list(Chars), !,
    phrase(command(Direction, Amount), Chars).

read_commands(File, Commands) :-
    read_lines(File, Lines),
    maplist(string_command, Lines, Commands).

%%%

final_coordinates_part1([], 0, 0).
final_coordinates_part1(Commands, HPos, Depth) :-
    select(forward - Amount, Commands, RestCommands),
    final_coordinates_part1(RestCommands, HPos1, Depth),
    HPos is HPos1 + Amount.
final_coordinates_part1(Commands, HPos, Depth) :-
    select(down - Amount, Commands, RestCommands),
    final_coordinates_part1(RestCommands, HPos, Depth1),
    Depth is Depth1 + Amount.
final_coordinates_part1([up-Amount | Cmds], HPos, Depth) :-
    NegAmount is Amount * -1,
    final_coordinates_part1([down-NegAmount | Cmds], HPos, Depth).

answer_part1(File, Answer) :-
    open(File, read, Stream),
    read_commands(Stream, Commands),
    final_coordinates_part1(Commands, HPos, Depth),
    Answer is HPos * Depth.

%%%

final_coordinates_part2([], 0, 0, _).

final_coordinates_part2([down - Amount | Commands], HPos, Depth, Aim) :-
    Aim1 is Aim + Amount,
    final_coordinates_part2(Commands, HPos, Depth, Aim1).

final_coordinates_part2([up - Amount | Commands], HPos, Depth, Aim) :-
    Aim1 is Aim - Amount,
    final_coordinates_part2(Commands, HPos, Depth, Aim1).

final_coordinates_part2([forward - Amount | Commands], HPos, Depth, Aim) :-
    final_coordinates_part2(Commands, HPos1, Depth1, Aim),
    HPos is HPos1 + Amount,
    Depth is Depth1 + (Aim * Amount).

test_part2(Answer) :-
    Commands = [
        forward-5,
        down-5,
        forward-8,
        up-3,
        down-8,
        forward-2
    ],
    final_coordinates_part2(Commands, HPos, Depth, 0),
    Answer is HPos * Depth.

answer_part2(File, Answer) :-
    open(File, read, Stream),
    read_commands(Stream, Commands),
    final_coordinates_part2(Commands, HPos, Depth, 0),
    Answer is HPos * Depth.
