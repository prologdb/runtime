:- use_module(library(equality)).
:- use_module(library(dynamics)).
:- use_module(library(typesafety)).

testPred(X) :- X = abc.

testPred() :- 1 = 1.

test "call/1 with compound" by [
    call(testPred(X)),
    nonvar(X),
    X = abc
].

test "compound_name_arguments destructure" by [
    compound_name_arguments(functor(arg1, arg2), F, [Arg1|T]),
    nonvar(F),
    nonvar(Arg1),
    nonvar(T),
    F = functor,
    Arg1 = arg1,
    T = [arg2]
].

test "compound_name_arguments construct" by [
    compound_name_arguments(C, functor, [arg1, arg2]),
    nonvar(C),
    C = functor(arg1, arg2)
].
