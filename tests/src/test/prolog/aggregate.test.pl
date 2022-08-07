:- use_module(essential($equality)).
:- use_module(library(aggregate)).

pred(1).
pred(2).

test "operators" by [
    (count as Count) == as(count, Count)
].

test "count results" by [
    reduce([count as Count], pred(_)),
    Count == 2
].