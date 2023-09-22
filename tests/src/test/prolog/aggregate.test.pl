:- use_module(essential($equality)).
:- use_module(essential($dynamic)).
:- use_module(essential($typesafety), [var/1]).
:- use_module(library(aggregate)).
:- use_module(library(lists), [member/2, sort/2, msort/2]).

pred(1).
pred(2).
pred(3).
pred(4).
pred(5).
pred(6).

test "operators" by [
    (count as Count) == as(count, Count)
].

test "count results" by [
    reduce([count as Count], pred(_)),
    Count == 6
].

test "list" by [
    reduce([list(V) as List], pred(V)),
    List == [1, 2, 3, 4, 5, 6]
].

test "min" by [
    reduce([min(V) as Min], pred(V)),
    Min == 1
].

test "min of empty input" by [
    reduce([min(V) as Min], 1 = 2),
    var(Min)
].

test "max" by [
    reduce([max(V) as Max], pred(V)),
    Max == 6
].

test "max of empty input" by [
    reduce([max(V) as Max], 1 = 2),
    var(Max)
].

test "avg" by [
    reduce([avg(V) as Avg], pred(V)),
    Avg == 3.5
].

test "avg of empty input" by [
    reduce([avg(V) as Avg], 1 = 2),
    var(Avg)
].

test "sum" by [
    reduce([sum(V) as Sum], pred(V)),
    Sum == 21
].

test "sum of empty input" by [
    reduce([sum(V) as Sum], 1 = 2),
    Sum == 0
].

test "set" by [
    reduce([set(V) as Set], member(V, [a, b, a, c, b, c, a, 2])),
    sort(Set, Sorted),
    Sorted == [2, a, b, c]
].

test "single reductor without enclosing list" by [
    reduce(count as Count, pred(_)),
    Count == 6
].

pred(a, g(a), 1).
pred(a, g(a), 2).
pred(a, g(b), 3).
pred(b, g(b), 4).
pred(b, g(c), 5).
pred(b, g(c), 6).

test "grouping by default" by [
    findall([Group1, Group2, VMax], reduce([max(V) as VMax], pred(Group1, Group2, V)), Results),
    Results == [
        [a, g(a), 2],
        [a, g(b), 3],
        [b, g(b), 4],
        [b, g(c), 6]
    ]
].

test "grouping with existential" by [
    findall([Group1, VMax], reduce([max(V) as VMax], Group2^pred(Group1, Group2, V)), Results),
    Results == [
        [a, 3],
        [b, 6]
    ]
].

test "simple grouping" by [
    findall(
        [Group, Max],
        reduce(
            [max(V) as Max],
            member([Group, V], [[a, 1], [a, 2], [b, 1], [b, 3]])
        ),
        Results
    ),
    Results == [
        [a, 2],
        [b, 3]
    ]
].

test "percentile_discrete with default sort" by [
    Elements = [10, 25, 1, 2, 90, 2512, 551, 8832, 401, 99, 211, 6621, 9662],
    reduce([percentile_discrete(0.1, V) as PV1], member(V, Elements)), PV1 == 2,
    reduce([percentile_discrete(0.58, V) as PV2], member(V, Elements)), PV2 == 401,
    reduce([percentile_discrete(1, V) as PV3], member(V, Elements)), PV3 == 9662
].

test "percentile_discrete with explicit asc sort" by [
    Elements = [10, 25, 1, 2, 90, 2512, 551, 8832, 401, 99, 211, 6621, 9662],
    reduce([percentile_discrete(0.1, V, asc) as PV1], member(V, Elements)), PV1 == 2,
    reduce([percentile_discrete(0.58, V, asc) as PV2], member(V, Elements)), PV2 == 401,
    reduce([percentile_discrete(1, V, asc) as PV3], member(V, Elements)), PV3 == 9662
].

test "percentile_discrete with explicit desc sort" by [
    Elements = [10, 25, 1, 2, 90, 2512, 551, 8832, 401, 99, 211, 6621, 9662],
    reduce([percentile_discrete(0.1, V, desc) as PV1], member(V, Elements)), PV1 == 8832,
    reduce([percentile_discrete(0.58, V, desc) as PV2], member(V, Elements)), PV2 == 99,
    reduce([percentile_discrete(1, V, desc) as PV3], member(V, Elements)), PV3 == 1
].

test "percentile_continuous with default sort" by [
    Elements = [10, 25, 1, 2, 90, 2512, 551, 8832, 401, 99, 211, 6621, 9662],
    reduce([percentile_continuous(0.1, V) as PV1], member(V, Elements)), PV1 == 3.6,
    reduce([percentile_continuous(0.58, V) as PV2], member(V, Elements)), PV2 == 393.4,
    reduce([percentile_continuous(0.98, V) as PV3], member(V, Elements)), PV3 == 9462.8,
    reduce([percentile_continuous(1, V) as PV4], member(V, Elements)), PV4 == 9662.0
].

test "percentile_continuous with explicit asc sort" by [
    Elements = [10, 25, 1, 2, 90, 2512, 551, 8832, 401, 99, 211, 6621, 9662],
    reduce([percentile_continuous(0.1, V, asc) as PV1], member(V, Elements)), PV1 == 3.6,
    reduce([percentile_continuous(0.58, V, asc) as PV2], member(V, Elements)), PV2 == 393.4,
    reduce([percentile_continuous(0.98, V, asc) as PV3], member(V, Elements)), PV3 == 9462.8,
    reduce([percentile_continuous(1, V, asc) as PV4], member(V, Elements)), PV4 == 9662.0
].

test "percentile_continuous with explicit desc sort" by [
    Elements = [10, 25, 1, 2, 90, 2512, 551, 8832, 401, 99, 211, 6621, 9662],
    reduce([percentile_continuous(0.1, V, desc) as PV1], member(V, Elements)), PV1 == 8389.8,
    reduce([percentile_continuous(0.58, V, desc) as PV2], member(V, Elements)), PV2 == 103.48,
    reduce([percentile_continuous(0.98, V, desc) as PV3], member(V, Elements)), PV3 == 1.24,
    reduce([percentile_continuous(1, V, desc) as PV4], member(V, Elements)), PV4 == 1.0
].

test "standard deviation and variance" by [
    reduce([variance(V) as Variance, stddev(V) as StdDev], pred(V)),
    Variance == 2.9166666666666666666666666666666,
    StdDev == 1.7078251276599330638701731134201
].

test "boolean_and all false" by [
    reduce([boolean_and(V) as R], member(V, [false, false, false])),
    R == false
].

test "boolean_and all true" by [
    reduce([boolean_and(V) as R], member(V, [true, true, true])),
    R == true
].

test "boolean_and mixed true first" by [
    reduce([boolean_and(V) as R], member(V, [true, false, true, false])),
    R == false
].

test "boolean_and mixed false first" by [
    reduce([boolean_and(V) as R], member(V, [false, true, false, true])),
    R == false
].

test "boolean_and mixed with custom atoms" by [
    reduce([boolean_and(V, truthy, falsy) as R], member(V, [truthy, falsy])),
    R == falsy
].

test "boolean_and all true with custom atoms" by [
    reduce([boolean_and(V, truthy, falsy) as R], member(V, [truthy, truthy, truthy])),
    R == truthy
].

test "boolean_and defaults to false" by [
    reduce([boolean_and(V) as R], member(V, [])),
    R == false
].

test "boolean_or all false" by [
    reduce([boolean_or(V) as R], member(V, [false, false, false])),
    R == false
].

test "boolean_or all true" by [
    reduce([boolean_or(V) as R], member(V, [true, true, true])),
    R == true
].

test "boolean_or mixed true first" by [
    reduce([boolean_or(V) as R], member(V, [true, false, true, false])),
    R == true
].

test "boolean_or mixed false first" by [
    reduce([boolean_or(V) as R], member(V, [false, true, false, true])),
    R == true
].

test "boolean_or mixed with custom atoms" by [
    reduce([boolean_or(V, truthy, falsy) as R], member(V, [truthy, falsy])),
    R == truthy
].

test "boolean_or all true with custom atoms" by [
    reduce([boolean_or(V, truthy, falsy) as R], member(V, [truthy, truthy, truthy])),
    R == truthy
].

test "boolean_or defaults to false" by [
    reduce([boolean_or(V) as R], member(V, [])),
    R == false
].

dummy_reductor(reductor, initialize, dummy_reductor(), bla).
dummy_reductor(reductor, accumulate, dummy_reductor(), C, C).
dummy_reductor(reductor, finalize, dummy_reductor(), C, C).

test "reduce/2 should instantiate results even when generator has zero solutions" by [
    reduce([dummy_reductor() as R], 1 = 2),
    R == bla
].