:- use_module(essential($equality)).
:- use_module(library(solution_sequences)).
:- use_module(essential($dynamic)).

pred(1).
pred(2).
pred(3).
pred(4).
pred(5).

test "limit/2" by [
    findall(A, limit(3, pred(A)), Solutions),
    Solutions = [1, 2, 3]
].