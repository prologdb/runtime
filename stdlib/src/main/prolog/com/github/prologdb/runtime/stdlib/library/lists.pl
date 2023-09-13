:- module(lists, [
    between/3,
    length/2,
    member/2,
    reverse/2,
    set/2,
    set/3,
    sort/2,
    msort/2,
    append/3,
    index_unifying/3
]).

:- native length/2.
:- native member/2.
:- native reverse/2.

:- native set/2.
:- native set/3.
:- native sort/2.
:- native msort/2.

:- module_transparent(set/2).
:- module_transparent(set/3).

append([], L, L).
append([H|T], L2, [H|R]) :- append(T, L2, R).

index_unifying(List, Index, Element) :-
    once(
        var(Index) ;
        (length(List, NElements), Index < NElements, Index >= 0)
    ),
    index_unifying(List, Index, Element, 0).

index_unifying([], _, _, _) :- fail().
index_unifying([Sample|Rest], Index, Sample, Index).
index_unifying([_|Rest], Index, Element, IndexCarry) :-
    NextIndex is IndexCarry + 1,
    index_unifying(Rest, Index, Element, NextIndex).

between(_, _, Value) :- nonvar(Value), \+ integer(Value), error("The value must be unbound or an integer").
between(Low, High, _) :- (\+ integer(Low) ; \+ integer(High)), error("Both bounds must be integers").
between(Low, High, Value) :-
    number(Value),
    once(
        (High >= Value, Low =< Value)
        ;
        (High =< Value, Low >= Value)
    ).
between(Low, High, Value) :-
    var(Value),
    (
        (Low =< High, between_iterate_up(Low, 1, High, Value))
        ;
        (Low > High, between_iterate_down(Low, 1, High, Value))
    )
    .

between_iterate_up(Current, _, _, Current).
between_iterate_up(Current, Step, Until, Value) :-
     Next is Current + Step,
     Next =< Until,
     between_iterate_up(Next, Step, Until, Value).

between_iterate_down(Current, _, _, Current).
between_iterate_down(Current, Step, Until, Value) :-
     Next is Current - Step,
     Next >= Until,
     between_iterate_down(Next, Step, Until, Value).