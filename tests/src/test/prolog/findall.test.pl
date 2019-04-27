:- use_module(library(dynamics)).
:- use_module(library(lists)).

foo(a).
foo(b).

test "findall with simple variable as template" by [
    findall(X, foo(X), R),
    member(a, R),
    member(b, R)
].

test "findall with complex template" by [
    findall(bar([X]), foo(X), R),
    member(bar([a]), R),
    member(bar([b]), R)
].