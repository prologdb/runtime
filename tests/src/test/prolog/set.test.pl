:- use_module(library(equality)).
:- use_module(library(lists)).

test "from collection to set" by [
    set([a, aa, cc], S),
    length(S, 3),
    member(a, S),
    member(aa, S),
    member(cc, S)
].

test "test set" by [
    set(_, [a, bb, c]),
    not(set(_, [a, a, bb]))
].


testCmp(a, aa).
testCmp(b, bb).
testCmp(c, cc).

test "from collection to set using comparator" by [
    set([a, aa, cc], S, testCmp),
    length(S, 2),
    (member(a, S) ; member(aa, S)),
    member(cc, S)
].

test "test set using comparator" by [
    set(_, [a, bb, c], testCmp),
    not(set(_, [a, aa, b], testCmp))
].