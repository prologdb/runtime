:- use_module(essential($equality)).
:- use_module(essential($dynamic)).
:- use_module(essential($typesafety)).

testPred(X, Y) :- X = abc, Y = def.

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

test "apply/2 with atom" by [
    apply(testPred, [X]),
    nonvar(X),
    X = abc
].

test "apply/2 with empty compound" by [
    apply(testPred(), [X]),
    nonvar(X),
    X = abc
].

test "apply/2 with compound" by [
    apply(testPred(X), [Y]),
    nonvar(X),
    nonvar(Y),
    X = abc,
    Y = def
].

test "apply/2 with lambda" by [
    apply(
        (X :-
            X = abc
        ),
        [Out]
    ),
    nonvar(Out),
    Out = abc
].

test "term_variables/2 with no variables" by [
    term_variables(a(1), [])
].
test "term_variables/2" by [
    term_variables(a(Z, 2, [Y, 1|X], {f:W}), [Z, Y, X, W])
].
