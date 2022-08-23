:- module(solution_sequences).

:- native limit/2.
:- native offset/2.
:- native distinct/2.
:- native call_nth/2.

:- module_transparent(limit/2).
:- module_transparent(offset/2).
:- module_transparent(distinct/1).
:- module_transparent(distinct/2).
:- module_transparent(call_nth/2).

distinct(Goal) :- solution_sequences:distinct(Goal, Goal).