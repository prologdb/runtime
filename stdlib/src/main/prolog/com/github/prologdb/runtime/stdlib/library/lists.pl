:- module(lists, [
    iota/3,
    iota/4,
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

:- native iota/3.
:- native iota/4.

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
