:- use_module(library(equality)).

test "negation - argument is false" by [
    not(fact(arg))
].

test "identity - identical atoms" by [
    a == a
].

test "identity - different atoms" by [
    \+ b == a
].

test "identity - equal complex" by [
    [1, a, foo(bar)] == [1, a, foo(bar)]
].

test "identity - different complex" by [
    \+ [a, c, 3.213] == some(very(complex), [term])
].