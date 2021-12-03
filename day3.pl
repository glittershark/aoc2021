% -*- mode: prolog -*-

:- use_module(library(clpfd), [transpose/2]).
:- use_module(library(aggregate)).

:- use_module(utils, [read_lines/2, binary_number/2]).


read_binary_string([], []).
read_binary_string(['0' | S], [0 | B]) :- read_binary_string(S, B).
read_binary_string(['1' | S], [1 | B]) :- read_binary_string(S, B).

%%%

% nondet, since there might be a tie
most_and_least_common(L, MostCommon, LeastCommon) :-
    findall(E - C0, aggregate(count, member(E, L), C0), Counts),

    aggregate(max(CMx, V), member(V - CMx, Counts), max(MaxCount, _)),
    aggregate(min(CMn, V), member(V - CMn, Counts), min(MinCount, _)),

    member(MostCommon - MostCommonCount, Counts),
    member(LeastCommon - LeastCommonCount, Counts),

    MostCommonCount = MaxCount,
    LeastCommonCount = MinCount.

gamma_epsilon(Report, Gamma, Epsilon) :-
    transpose(Report, Transposed),
    maplist(most_and_least_common, Transposed, GammaBinary, EpsilonBinary),
    binary_number(GammaBinary, Gamma),
    binary_number(EpsilonBinary, Epsilon).

power_consumption(Report, Power) :-
    gamma_epsilon(Report, Gamma, Epsilon),
    Power is Gamma * Epsilon.

power_consumption_from_file(File, Answer) :-
    open(File, read, Stream),
    read_lines(Stream, Lines),
    maplist(string_chars, Lines, LineChars),
    maplist(read_binary_string, LineChars, Report),
    power_consumption(Report, Answer).

%%%

oxygen_generator_rating_pos(Report, Rating, Pos) :-
    % Find all most common bits
    maplist(nth0(Pos), Report, Bits),
    findall(MostCommon, most_and_least_common(Bits, MostCommon, _), MostCommonBits),
    ([MostCommon] = MostCommonBits ; MostCommon = 1),

    !,

    % Find values matching that bit
    findall(
        Value,
        (member(Value, Report), nth0(Pos, Value, MostCommon)),
        NewReport
    ),

    % Either we only have one value, or keep going
    (  [RatingBinary] = NewReport
    -> binary_number(RatingBinary, Rating)
    ;  Pos1 is Pos + 1,
       oxygen_generator_rating_pos(NewReport, Rating, Pos1)
    ).

oxygen_generator_rating(Report, Rating) :-
    oxygen_generator_rating_pos(Report, Rating, 0).


co2_scrubber_rating_pos(Report, Rating, Pos) :-
    % Find all least common bits
    maplist(nth0(Pos), Report, Bits),
    findall(LeastCommon, most_and_least_common(Bits, _, LeastCommon), LeastCommonBits),
    ([LeastCommon] = LeastCommonBits ; LeastCommon = 0),

    !,

    % Find values matching that bit
    findall(
        Value,
        (member(Value, Report), nth0(Pos, Value, LeastCommon)),
        NewReport
    ),

    % Either we only have one value, or keep going
    (  [RatingBinary] = NewReport
    -> binary_number(RatingBinary, Rating)
    ;  Pos1 is Pos + 1,
       co2_scrubber_rating_pos(NewReport, Rating, Pos1)
    ).

co2_scrubber_rating(Report, Rating) :-
    co2_scrubber_rating_pos(Report, Rating, 0).


life_support_rating(Report, Rating) :-
    oxygen_generator_rating(Report, OxygenGeneratorRating),
    co2_scrubber_rating(Report, CO2ScrubberRating),
    Rating is OxygenGeneratorRating * CO2ScrubberRating.


life_support_rating_from_file(File, Answer) :-
    open(File, read, Stream),
    read_lines(Stream, Lines),
    maplist(string_chars, Lines, LineChars),
    maplist(read_binary_string, LineChars, Report),
    life_support_rating(Report, Answer).
