:- module($clauses).

:- native abolish/1.
:- native assert/1.
:- native retract/1.
:- native retractAll/1.

:- module_transparent(assert/1).
:- module_transparent(retract/1).

true() :- 1 = 1.
fail() :- 1 = 2.
