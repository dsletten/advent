#!/usr/bin/swipl -q -t main -f
%%
%   -*- Mode: Prolog -*-
%   Name:               2015.pl
%
%   Started:            Fri Sep 10 20:07:50 2021
%
%   Purpose: Advent of Code 2015
%   https://adventofcode.com/2015
%
%   Notes:
%
%%

:- module(advent_2015, [deliver_presents/2, enter_basement/2, compute_wrap/2, compute_ribbon/2]).

:- use_module('/home/slytobias/prolog/books/Adventure/io.pl').

%%%
%%%    Day 1
%%%    
deliver_presents(S, Floor) :-
    atom_chars(S, L),
    deliver_presents(L, Floor, 0).
deliver_presents([], Floor, Floor).
deliver_presents(['('|T], Floor, Acc) :-
    Acc1 is Acc + 1,
    deliver_presents(T, Floor, Acc1).
deliver_presents([')'|T], Floor, Acc) :-
    Acc1 is Acc - 1,
    deliver_presents(T, Floor, Acc1).

%% deliver_presents('(())', 0).
%% deliver_presents('()()', 0).
%% deliver_presents('(((', 3).
%% deliver_presents('(()(()(', 3).
%% deliver_presents('))(((((', 3).
%% deliver_presents('())', -1).
%% deliver_presents('))(', -1).
%% deliver_presents(')))', -3).
%% deliver_presents(')())())', -3).

start_floor('(', 1).
start_floor(')', -1).

enter_basement(S, Index) :-
    atom_chars(S, [H|T]),
    start_floor(H, F),
    enter_basement(T, F, 1, Index).
enter_basement(_, -1, I, I).
enter_basement(['('|T], F, I, Index) :-
    F1 is F + 1,
    I1 is I + 1,
    enter_basement(T, F1, I1, Index).
enter_basement([')'|T], F, I, Index) :-
    F1 is F - 1,
    I1 is I + 1,
    enter_basement(T, F1, I1, Index).

%% enter_basement(')', 1).
%% enter_basement('()())', 5).
%% enter_basement('()', F).

%%%
%%%    Day 2
%%%
compute_wrap([Length, Width, Height], Wrap) :-
    msort([Length, Width, Height], [A, B, C]), % Don't remove duplicates!!
    AB is A * B,
    AC is A * C,
    BC is B * C,
    Slack is AB,
    Wrap is 2 * AB + 2 * AC + 2 * BC + Slack.

%% compute_wrap(2, 3, 4, 58).
%% compute_wrap(1, 1, 10, 43).

parse_dimensions(D, Ns) :-
    atom_string(D, S),
    split_string(S, "x", "", Ds),
    convert_dimensions(Ds, Ns).

convert_dimensions([], []).
convert_dimensions([D|Ds], [N|Ns]) :-
    number_string(N, D),
    convert_dimensions(Ds, Ns).

calculate_total_wrap(File, Sum) :-
    io:read_file(File, Lines),
    parse_file(Lines, Dimensions),
    compute_all_wraps(Dimensions, Sum).

compute_all_wraps([], 0).
compute_all_wraps([D|Ds], S) :-
    compute_wrap(D, W),
    compute_all_wraps(Ds, S1),
    S is S1 + W.

parse_file([], []).
parse_file([L|Ls], [D|Ds]) :-
    parse_dimensions(L, D),
    parse_file(Ls, Ds).

compute_ribbon([Length, Width, Height], R) :-
    msort([Length, Width, Height], [A, B, C]),
    Ribbon is 2 * A + 2 * B,
    Bow is A * B * C,
    R is Ribbon + Bow.

%% compute_ribbon(2, 3, 4, 34).
%% compute_ribbon(1, 1, 10, 14).

%% (defn calculate-total-ribbon [file]
%%   (reduce + (map #(apply compute-ribbon %) (map parse-dimensions (string/split (slurp file) #"\n")))) )
calculate_total_ribbon(File, Sum) :-
    io:read_file(File, Lines),
    parse_file(Lines, Dimensions),
    compute_all_ribbons(Dimensions, Sum).

compute_all_ribbons([], 0).
compute_all_ribbons([D|Ds], S) :-
    compute_ribbon(D, R),
    compute_all_ribbons(Ds, S1),
    S is S1 + R.

%%%
%%%    Day 3
%%%    Probably not the right way to do this...
%%%    Dynamically uses KB to record visited locations.
%%%    Should use SWIPL dictionary instead??
%%%
:- dynamic visited/3.

update_location('^', X, Y, X, Y1) :-
    Y1 is Y + 1.
update_location('v', X, Y, X, Y1) :-
    Y1 is Y - 1.
update_location('>', X, Y, X1, Y) :-
    X1 is X + 1.
update_location('<', X, Y, X1, Y) :-
    X1 is X - 1.

leave_present(X, Y) :-
    visited(X, Y, C), !,
    retract(visited(X, Y, C)),
    C1 is C + 1,
    assertz(visited(X, Y, C1)).
leave_present(X, Y) :-
    \+ visited(X, Y, _),
    assertz(visited(X, Y, 1)).

%%%
%%%    Nonsense!!!
%%%    
%% visit_houses(Directions, Count) :-
%% %    retract(visited(_, _, _)),
%%     retractall(visited(_, _, _)),
%%     leave_present(0, 0),
%%     visit_houses(Directions, Count, 1, 0, 0).
%% visit_houses([], Count, Count, _, _) :- !.
%% visit_houses([D|Ds], Count, C, X, Y) :-
%%     update_location(D, X, Y, X1, Y1),
%%     leave_present(X1, Y1),
%%     C1 is C + 1,
%%     visit_houses(Ds, Count, C1, X1, Y1).

%% visit_houses(Directions, Count) :-
%% %    retract(visited(_, _, _)),
%%     retractall(visited(_, _, _)),
%%     leave_present(0, 0),
%%     visit_houses(Directions, Count, 0, 0).
%% visit_houses([], Count, _, _) :-
%%     findall(C, visited(_, _, C), L),
%%     length(L, Count).
%% visit_houses([D|Ds], Count, X, Y) :-
%%     update_location(D, X, Y, X1, Y1),
%%     leave_present(X1, Y1),
%%     visit_houses(Ds, Count, X1, Y1).

visit_houses(Directions, Count) :-
%    retract(visited(_, _, _)),
    retractall(visited(_, _, _)),
    visit_houses(Directions, Count, 0, 0).
visit_houses([], Count, X, Y) :-
    leave_present(X, Y),
    findall(C, visited(_, _, C), L),
    length(L, Count).
visit_houses([D|Ds], Count, X, Y) :-
    leave_present(X, Y),
    update_location(D, X, Y, X1, Y1),
    visit_houses(Ds, Count, X1, Y1).

%% io:read_chars('day3.data', L), visit_houses(L, C).
%% L = [^, >, <, ^, >, >, >, ^, <|...],
%% C = 2081 

leave_present(agent(_, X, Y)) :-
    leave_present(X, Y).

update_location(agent(Name, X, Y), Dir, agent(Name, X1, Y1)) :-
    update_location(Dir, X, Y, X1, Y1).

robo_visit(Directions, Count) :-
    retractall(visited(_, _, _)),
    Santa = agent(santa, 0, 0),
    Robot = agent(robot, 0, 0),
    leave_present(Santa),
    leave_present(Robot),
    Team = [Santa, Robot],
    robo_visit(Directions, Team, Count).
robo_visit([], _, Count) :-
    findall(C, visited(_, _, C), L),
    length(L, Count).
robo_visit([D|Ds], [A|As], Count) :-
    update_location(A, D, A1),
    leave_present(A1),
    append(As, [A1], Team),
    robo_visit(Ds, Team, Count).
    
%% io:read_chars('day3.data', L), robo_visit(L, C).
%% L = [^, >, <, ^, >, >, >, ^, <|...],
%% C = 2341 

%%%
%%%    Day 4
%%%    
:- use_module(library(md5)).

mine_advent_coin(Prefix, N, Coin) :-
    mine_advent_coin(Prefix, N, 1, Coin).
mine_advent_coin(Prefix, N, I, I) :-
    atom_concat(Prefix, I, Seed),
    coin_found(Seed, N).
mine_advent_coin(Prefix, N, I, Coin) :-
    I1 is I + 1,
    mine_advent_coin(Prefix, N, I1, Coin).

coin_found(Seed, N) :-
    md5_hash(Seed, M, []),
    atom_chars(M, Nybbles),
    first_n_zeroes(Nybbles, N).

first_n_zeroes(_, 0).
first_n_zeroes(['0'|As], N) :-
    N1 is N - 1,
    first_n_zeroes(As, N1).

%% advent_2015: 106 ?- mine_advent_coin("ckczppom", 5, I).
%% I = 117946

%% advent_2015: 107 ?- mine_advent_coin("ckczppom", 6, I).
%% I = 3938038 

%%%
%%%    Day 5
%%%    So many cuts!!
%%%    
check_nice1(File, Count) :-
    io:read_file(File, Lines),
    count_nice1(Lines, Count, 0).

count_nice1([], Count, Count).
count_nice1([L|Ls], Count, C) :-
    nice1(L),
    C1 is C + 1,
    count_nice1(Ls, Count, C1).
count_nice1([_|Ls], Count, C) :-
    count_nice1(Ls, Count, C).

nice1(S) :-
    atom_chars(S, L),
    check_nice(L, 0, false).

nice(V, true) :-
    V >= 3.

vowel(a).
vowel(e).
vowel(i).
vowel(o).
vowel(u).

check_nice([], V, D) :-
    nice(V, D).
check_nice([a|Cs], V, D) :-
    !,
    V1 is V + 1,
    check_a(Cs, V1, D).
check_nice([c|Cs], V, D) :-
    !, check_c(Cs, V, D).
check_nice([p|Cs], V, D) :-
    !, check_p(Cs, V, D).
check_nice([x|Cs], V, D) :-
    !, check_x(Cs, V, D).
check_nice([C|Cs], V, D) :-
    vowel(C), !,
    V1 is V + 1,
    check_double_nice(C, Cs, V1, D).
check_nice([C|Cs], V, D) :-
    check_double_nice(C, Cs, V, D).

check_double_nice(_, [], V, D) :-
    nice(V, D).
check_double_nice(A, [A|Cs], V, _) :-
    !,
    check_nice([A|Cs], V, true).
check_double_nice(_, [A|Cs], V, D) :-
    check_nice([A|Cs], V, D).
    
check_a([], V, D) :-
    nice(V, D).
check_a([b|_], _, _) :- !, fail.
check_a([a|Cs], V, _) :-
    !,
    V1 is V + 1,
    check_a(Cs, V1, true).
check_a([c|Cs], V, D) :-
    !, check_c(Cs, V, D).
check_a([p|Cs], V, D) :-
    !, check_p(Cs, V, D).
check_a([x|Cs], V, D) :-
    !, check_x(Cs, V, D).
check_a([C|Cs], V, D) :-
    vowel(C), !,
    V1 is V + 1,
    check_double_nice(C, Cs, V1, D).
check_a([C|Cs], V, D) :-
    check_double_nice(C, Cs, V, D).

check_c([], V, D) :-
    nice(V, D).
check_c([d|_], _, _) :- !, fail.
check_c([c|Cs], V, _) :-
    !, check_c(Cs, V, true).
check_c([a|Cs], V, D) :-
    !,
    V1 is V + 1,
    check_a(Cs, V1, D).
check_c([p|Cs], V, D) :-
    !, check_p(Cs, V, D).
check_c([x|Cs], V, D) :-
    !, check_x(Cs, V, D).
check_c([C|Cs], V, D) :-
    vowel(C), !,
    V1 is V + 1,
    check_double_nice(C, Cs, V1, D).
check_c([C|Cs], V, D) :-
    check_double_nice(C, Cs, V, D).

check_p([], V, D) :-
    nice(V, D).
check_p([q|_], _, _) :- !, fail.
check_p([p|Cs], V, _) :-
    !, check_p(Cs, V, true).
check_p([a|Cs], V, D) :-
    !,
    V1 is V + 1,
    check_a(Cs, V1, D).
check_p([c|Cs], V, D) :-
    !, check_c(Cs, V, D).
check_p([x|Cs], V, D) :-
    !, check_x(Cs, V, D).
check_p([C|Cs], V, D) :-
    vowel(C), !,
    V1 is V + 1,
    check_double_nice(C, Cs, V1, D).
check_p([C|Cs], V, D) :-
    check_double_nice(C, Cs, V, D).

check_x([], V, D) :-
    nice(V, D).
check_x([y|_], _, _) :- !, fail.
check_x([x|Cs], V, _) :-
    !, check_x(Cs, V, true).
check_x([a|Cs], V, D) :-
    !,
    V1 is V + 1,
    check_a(Cs, V1, D).
check_x([c|Cs], V, D) :-
    !, check_c(Cs, V, D).
check_x([p|Cs], V, D) :-
    !, check_p(Cs, V, D).
check_x([C|Cs], V, D) :-
    vowel(C), !,
    V1 is V + 1,
    check_double_nice(C, Cs, V1, D).
check_x([C|Cs], V, D) :-
    check_double_nice(C, Cs, V, D).

%% (deftest test-nice-string-p ()
%%   (check
%%    (nice-string-p "ugknbfddgicrmopn")
%%    (nice-string-p "aaa")
%%    (not (nice-string-p "jchzalrnumimnmhp"))
%%    (not (nice-string-p "haegwjzuvuyypxyu"))
%%    (not (nice-string-p "dvszwmarrgswjxmb"))))







check_nice2(File, Count) :-
    io:read_file(File, Lines),
    count_nice2(Lines, Count, 0).

count_nice2([], Count, Count).
count_nice2([L|Ls], Count, C) :-
    nice2(L),
    C1 is C + 1,
    count_nice2(Ls, Count, C1).
count_nice2([_|Ls], Count, C) :-
    count_nice2(Ls, Count, C).

nice2(S) :-
    atom_chars(S, [A, B|Cs]),
    rule1(A, B, Cs),
    rule2(A, B, Cs).

rule1(A, B, Cs) :-
    search(A, B, Cs).
rule1(_, A, [B|Cs]) :-
    rule1(A, B, Cs).
search(A, B, [A, B|_]).
search(A, B, [_|Cs]) :-
    search(A, B, Cs).

rule2(A, _, [A|_]).
rule2(_, A, [B|Cs]) :-
    rule2(A, B, Cs).

%% advent_2015: 20 ?- nice2(qjhvhtzxzqqjkmpb).
%% true ;
%% true ;
%% false.

%% advent_2015: 21 ?- nice2(xxyxx).
%% true ;
%% false.

%% advent_2015: 22 ?- nice2(qryjbohkprfazczc).
%% true ;
%% true ;
%% false.

%% advent_2015: 23 ?- nice2(uurcxstgmygtbstg).
%% false.

%% advent_2015: 24 ?- nice2(ieodomkazucvgmuy).
%% false.

%% advent_2015: 25 ?- nice2(suerykeptdsutidb).
%% false.

%% advent_2015: 30 ?- check_nice2('day5.data', C).
%% C = 51
