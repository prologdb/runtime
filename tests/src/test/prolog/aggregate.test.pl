:- use_module(essential($equality)).
:- use_module(library(aggregate)).

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

test "min" by [
    reduce([min(V) as Min], pred(V)),
    Min == 1
].