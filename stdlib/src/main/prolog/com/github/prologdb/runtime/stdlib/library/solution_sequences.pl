:- module(solution_sequences, [
    limit/2,
    offset/2,
    distinct/1,
    distinct/2,
    call_nth/2,
    order_by/2
]).

:- native limit/2.
:- native offset/2.
:- native distinct/2.
:- native call_nth/2.

:- module_transparent(limit/2).
:- module_transparent(offset/2).
:- module_transparent(distinct/1).
:- module_transparent(distinct/2).
:- module_transparent(call_nth/2).
:- module_transparent(order_by/2).

distinct(Goal) :- solution_sequences:distinct(Goal, Goal).

order_by(asc(OrderBy), Goal) :-
    $dynamic:findall([Goal, OrderBy], Goal, Solutions),
    sort:predsort(solution_sequences:order_by_compare_second_list_element, Solutions, SortedSolutions),
    lists:member([Goal, _], SortedSolutions).

order_by(desc(OrderBy), Goal) :-
    $dynamic:findall([Goal, OrderBy], Goal, Solutions),
    sort:predsort(solution_sequences:order_by_compare_second_list_element, Solutions, SortedSolutions),
    lists:reverse(SortedSolutions, ReverseSortedSolutions),
    lists:member([Goal, _], ReverseSortedSolutions).

order_by_compare_second_list_element(Delta, [_,E1|_], [_,E2|_]) :- compare(Delta, E1, E2).