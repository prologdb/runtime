:- use_module(library(equality)).
:- use_module(library(strings)).

test "atom_string/2 conversion from atom to string" by [
    atom_string(foo, X),
    X = "foo"
].

test "atom_string/2 conversion from integer to string" by [
    atom_string(795432, X),
    X = "795432"
].

test "atom_string/2 conversion from decimal to string" by [
    atom_string(795.2213, X),
    X = "795.2213"
].

test "atom_string/2 conversion from string to atom" by [
    atom_string(X, "foobar"),
    X = foobar
].