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

