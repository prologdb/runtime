:- use_module(essential($equality)).
:- use_module(essential($dynamic)).
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

test "max" by [
    reduce([max(V) as Max], pred(V)),
    Max == 6
].

test "avg" by [
    reduce([avg(V) as Avg], pred(V)),
    Avg == 3.5
].

test "sum" by [
    reduce([sum(V) as Avg], pred(V)),
    Avg == 21
].

test "set" by [
    reduce([set(V) as Set], member(V, [a, b, a, c, b, c, a, 2])),
    sort(Set, Sorted),
    Sorted == [2, a, b, c]
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
    reduce([percentile_continuous(0.1, V) as PV1], member(V, Elements)), PV1 == 3.6000000000000014,
    reduce([percentile_continuous(0.58, V) as PV2], member(V, Elements)), PV2 == 393.39999999999986,
    reduce([percentile_continuous(0.98, V) as PV3], member(V, Elements)), PV3 == 9462.8,
    reduce([percentile_continuous(1, V) as PV4], member(V, Elements)), PV4 == 9662.0
].

test "percentile_continuous with explicit asc sort" by [
    Elements = [10, 25, 1, 2, 90, 2512, 551, 8832, 401, 99, 211, 6621, 9662],
    reduce([percentile_continuous(0.1, V, asc) as PV1], member(V, Elements)), PV1 == 3.6000000000000014,
    reduce([percentile_continuous(0.58, V, asc) as PV2], member(V, Elements)), PV2 == 393.39999999999986,
    reduce([percentile_continuous(0.98, V, asc) as PV3], member(V, Elements)), PV3 == 9462.8,
    reduce([percentile_continuous(1, V, asc) as PV4], member(V, Elements)), PV4 == 9662.0
].

test "percentile_continuous with explicit desc sort" by [
    Elements = [10, 25, 1, 2, 90, 2512, 551, 8832, 401, 99, 211, 6621, 9662],
    reduce([percentile_continuous(0.1, V, desc) as PV1], member(V, Elements)), PV1 == 8389.8,
    reduce([percentile_continuous(0.58, V, desc) as PV2], member(V, Elements)), PV2 == 103.4800000000001,
    reduce([percentile_continuous(0.98, V, desc) as PV3], member(V, Elements)), PV3 == 1.2400000000000002,
    reduce([percentile_continuous(1, V, desc) as PV4], member(V, Elements)), PV4 == 1.0
].

test "standard deviation and variance" by [
    reduce([variance(V) as Variance, stddev(V) as StdDev], pred(V)),
    Variance == 2.9166666666666665,
    StdDev == 1.707825127659933
].