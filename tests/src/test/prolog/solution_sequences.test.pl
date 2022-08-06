:- use_module(essential($equality)).
:- use_module(library(solution_sequences)).
:- use_module(essential($dynamic)).

pred(1).
pred(2).
pred(3).
pred(4).
pred(5).

test "limit/2 with more solutions than limit" by [
    findall(A, limit(3, pred(A)), Solutions),
    Solutions = [1, 2, 3]
].

test "limit/2 with fewer solutions than limit" by [
    findall(A, limit(10, pred(A)), Solutions),
    Solutions = [1, 2, 3, 4, 5]
].

test "offset/2 with more solutions than offset" by [
    findall(A, offset(3, pred(A)), Solutions),
    Solutions = [4, 5]
].

test "offset/2 with fewer solutions than offset" by [
    findall(A, offset(6, pred(A)), Solutions),
    Solutions = []
].