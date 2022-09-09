:- module($clauses).
:- use_module(essential($dynamic), [findall/3]).

:- native assert/1.
:- native retract/1.

:- module_transparent(assert/1).
:- module_transparent(retract/1).
:- module_transparent(retractAll/1).

:- det(assert/1).
:- det(retractAll/1).

true() :- 1 = 1.
fail() :- 1 = 2.
false() :- 1 = 2.

retractAll(Template) :-
    $dynamic:findall(_, $clauses:retract(Template), _)
    .
