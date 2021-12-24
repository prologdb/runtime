:- module($clauses).

:- native assert/1.
:- native retract/1.

:- module_transparent(assert/1).
:- module_transparent(retract/1).
:- module_transparent(retractAll/1).

true() :- 1 = 1.
fail() :- 1 = 2.

retractAll(Template) :-
    findall(_, $clauses:retract(Template), _)
    .
