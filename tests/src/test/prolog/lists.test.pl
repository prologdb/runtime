:- use_module(essential($equality)).
:- use_module(library(lists)).
:- use_module(essential($dynamic), [findall/3, findnsols/4]).

test "index_unifying/2 find by index - exists" by [
    index_unifying([a, b, c], 1, Value),
    Value = b
].

test "index_unifying/2 find by index - out of bounds" by [
    \+ index_unifying([a, b, c], 3, _)
].

test "index_unifying/2 find by value - exists" by [
    index_unifying([a, b, c], Index, c),
    Index = 2
].

test "index_unifying/2 find by value - not exists" by [
    \+ index_unifying([a, b, c], _, f)
].

test "index_unifying/2 iterating" by [
    findall([Index, Value], index_unifying([a, b, c], Index, Value), Results),
    Results == [
        [0, a],
        [1, b],
        [2, c]
    ]
].

test "between/3 range test" by [
    between(1, 5, 2),
    between(5, 1, 2),
    \+ between(1, 5, 6),
    \+ between(5, 1, 6)
].

test "between/3 range iteration ascending" by [
    findall(N, between(-3, 4, N), Ns),
    Ns == [-3, -2, -1, 0, 1, 2, 3, 4]
].

test "between/3 range iteration descending" by [
    findall(N, between(4, -3, N), Ns),
    Ns == [4, 3, 2, 1, 0, -1, -2, -3]
].