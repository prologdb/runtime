:- use_module(library(equality)).
:- use_module(library(math)).
:- use_module(library(typesafety)).

count(0, A, A).
count(N, A, R) :-
    N > 0,
    N2 is N - 1,
    A2 is A + 1,
    count(N2, A2, R)
    .

test "correct result" by [
    count(20, 0, R),
    R = 20
].

test "stack overflow without tco" by [
    count(100000, 0, R),
    R = 100000
].

% not imported, so can be redefined
append([], X, X).
append([A|X], B, [A|C]) :- append(X, B, C).

test "doesn't apply if head requires actions" by [
    L1 = [1, 2, 3],
    L2 = [4, 5, 6],
    append(L1, L2, R),
    var(C),
    R = [1, 2, 3, 4, 5, 6]
].