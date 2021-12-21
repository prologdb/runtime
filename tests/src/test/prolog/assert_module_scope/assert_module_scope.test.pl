:- use_module(assert_module_scope(a)).
:- use_module(essential($equality)).
:- use_module(essential($clauses)).

:- dynamic foo/1.

test "assert into self positive" by [
    assert(foo(z)),
    foo(z)
].

test "assert into self negative" by [
    \+ a:foo(z)
].

test "assert into other module" by [
    assert(a:foo(b)),
    a:foo(a),
    a:foo(b)
].
