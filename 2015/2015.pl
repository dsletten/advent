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

:- module(advent_2015, [deliver_presents/2, enter_basement/2]).

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
