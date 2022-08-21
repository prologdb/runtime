:- use_module(essential($equality)).
:- use_module(library(lists)).
:- use_module(essential($dynamic), [findall/3]).

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