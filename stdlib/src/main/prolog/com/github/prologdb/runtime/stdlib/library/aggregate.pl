:- module(aggregate, [
    op(1100, xfx, as),
    reduce/2,
    count/4,
    count/5,
    min/4,
    min/5,
    max/4,
    max/5,
    avg/4,
    avg/5,
    sum/4,
    sum/5,
    percentile_discrete/4,
    percentile_discrete/5
]).

:- native reduce/2.
:- module_transparent(reduce/2).

count(reductor, initialize, count, 0).
count(reductor, accumulate, count, Acc, NAcc) :- NAcc is Acc + 1.
count(reductor, finalize, count, Acc, Acc).

min(reductor, initialize, min(_), {initial: true}).
min(reductor, accumulate, min(Of), {initial: true}, {acc: Of}).
min(reductor, accumulate, min(Of), {acc: Acc}, {acc: Acc}) :- Of >= Acc.
min(reductor, accumulate, min(Of), {acc: Acc}, {acc: Of}) :- Of < Acc.
min(reductor, finalize, min(_), {initial: true}, _).
min(reductor, finalize, min(_), {acc: Min}, Min).

min(reductor, initialize, min(_, _), {initial: true}).
min(reductor, accumulate, min(Of, Witness), {initial: true}, {acc: Of, witness: Witness}).
min(reductor, accumulate, min(Of, NewWitness), {acc: Acc, witness: OldWitness}, {acc: Acc, witness: OldWitness}) :- Of >= Acc.
min(reductor, accumulate, min(Of, NewWitness), {acc: Acc, witness: OldWitness}, {acc: Of, witness: NewWitness}) :- Of < Acc.
min(reductor, finalize, min(_, _), {initial: true}, _).
min(reductor, finalize, min(_, _), {acc: Acc, witness: Witness}, {acc: Acc, witness: Witness}).

max(reductor, initialize, max(_), {initial: true}).
max(reductor, accumulate, max(Of), {initial: true}, {acc: Of}).
max(reductor, accumulate, max(Of), {acc: Acc}, {acc: Acc}) :- Of =< Acc.
max(reductor, accumulate, max(Of), {acc: Acc}, {acc: Of}) :- Of > Acc.
max(reductor, finalize, max(_), {initial: true}, _).
max(reductor, finalize, max(_), {acc: Max}, Max).

min(reductor, initialize, max(_, _), {initial: true}).
min(reductor, accumulate, max(Of, Witness), {initial: true}, {acc: Of, witness: Witness}).
min(reductor, accumulate, max(Of, NewWitness), {acc: Acc, witness: OldWitness}, {acc: Acc, witness: OldWitness}) :- Of =< Acc.
min(reductor, accumulate, max(Of, NewWitness), {acc: Acc, witness: OldWitness}, {acc: Of, witness: NewWitness}) :- Of > Acc.
min(reductor, finalize, max(_, _), {initial: true}, _).
min(reductor, finalize, max(_, _), {acc: Acc, witness: Witness}, {acc: Acc, witness: Witness}).

avg(reductor, initialize, avg(_), {initial: true}).
avg(reductor, accumulate, avg(Of), {initial: true}, {sum: Of, count: 1}).
avg(reductor, accumulate, avg(Of), {sum: CurrentSum, count: CurrentCount}, {sum: NewSum, count: NewCount}) :-
    NewSum is CurrentSum + Of,
    NewCount is CurrentCount + 1
    .
avg(reductor, finalize, avg(_), {initial: true}, _).
avg(reductor, finalize, avg(_), {sum: Sum, count: Count}, Avg) :- var(Avg), Avg is decimal(Sum) / decimal(Count).

sum(reductor, initialize, sum(_), 0).
sum(reductor, accumulate, sum(Of), Acc, NAcc) :- NAcc is Acc + Of.
sum(reductor, finalize, sum(_), Sum, Sum).

percentile_discrete(reductor, initialize, percentile_discrete(P, _), Accumulator) :-
    percentile_discrete(reductor, initialize, percentile_discrete(P, _, asc), Accumulator).
percentile_discrete(reductor, initialize, percentile_discrete(P, _, Direction), []) :-
    require(member(Direction, [asc, desc]), "sort direction must be asc or desc"),
    require((P > 0, P =< 1), "the percentile must be in range (0, 1]").
percentile_discrete(reductor, accumulate, percentile_discrete(_, Element), Elements, [Element|Elements]).
percentile_discrete(reductor, accumulate, percentile_discrete(_, Element, _), Elements, [Element|Elements]).
percentile_discrete(reductor, finalize, percentile_discrete(P, _), Elements, PValue) :-
    percentile_discrete(reductor, finalize, percentile_discrete(P, _, asc), Elements, PValue).
percentile_discrete(reductor, finalize, percentile_discrete(P, _, asc), Elements, PValue) :-
    msort(Elements, SortedElements),
    length(SortedElements, NElements),
    Rank is ceil(decimal(P) * decimal(NElements)) - 1,
    index_unifying(SortedElements, Rank, PValue).
percentile_discrete(reductor, finalize, percentile_discrete(P, _, desc), Elements, PValue) :-
    msort(Elements, SortedElements),
    reverse(SortedElements, ReversedElements),
    length(ReversedElements, NElements),
    Rank is ceil(decimal(P) * decimal(NElements)) - 1,
    index_unifying(ReversedElements, Rank, PValue).

