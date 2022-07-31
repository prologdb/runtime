:- use_module(test('assert_module_scope_b.pl')).
:- use_module(essential($equality)).
:- use_module(essential($clauses)).

:- dynamic foo/1.

test "assert into self positive" by [
    assert(foo(z)),
    foo(z)
].

test "assert into self negative" by [
    \+ 'assert_module_scope_b.pl':foo(z)
].

test "assert into other module" by [
    assert('assert_module_scope_b.pl':foo(b)),
    'assert_module_scope_b.pl':foo(a),
    'assert_module_scope_b.pl':foo(b)
].
