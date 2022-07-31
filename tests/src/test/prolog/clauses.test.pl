:- use_module(essential($clauses)).
:- use_module(essential($dynamic)).
:- use_module(essential($equality)).

:- dynamic foo/2.

foo(a, 1).
foo(b, 2).
foo(c, 3).

test "retract/1 returns data" by [
    findall(foo(A, B), retract(foo(A, B)), Results),
    Results = [
        foo(a, 1),
        foo(b, 2),
        foo(c, 3)
    ]
].

test "retract/1 removes data" by [
    \+ findall(_, foo(_, _), []),
    findall(_, retract(foo(_, _)), _),
    \+ foo(_, _)
].

test "retract/1 takes effect immediately" by [
    retract(foo(A, B)),
    \+ foo(A, B)
].
