:- module(lists).

:- native iota/3.
:- native iota/4.

:- native length/2.
:- native member/2.
:- native reverse/2.

:- native set/2.
:- native set/3.
:- native sort/2.

:- module_transparent(set/2).
:- module_transparent(set/3).

append([], L, L).
append([H|T], L2, [H|R]) :- append(T, L2, R).
