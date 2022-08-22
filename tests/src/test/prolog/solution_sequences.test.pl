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
    Solutions == [1, 2, 3]
].

test "limit/2 with fewer solutions than limit" by [
    findall(A, limit(10, pred(A)), Solutions),
    Solutions == [1, 2, 3, 4, 5]
].

test "offset/2 with more solutions than offset" by [
    findall(A, offset(3, pred(A)), Solutions),
    Solutions == [4, 5]
].

test "offset/2 with fewer solutions than offset" by [
    findall(A, offset(6, pred(A)), Solutions),
    Solutions == []
].

pred_dup(1).
pred_dup(2).
pred_dup(2).
pred_dup(3).
pred_dup(4).
pred_dup(1).
pred_dup(2).
pred_dup(5).

test "distinct/1" by [
    findall(V, distinct(pred(V)), Solutions),
    Solutions == [1, 2, 3, 4, 5]
].

pred_dup(1, a).
pred_dup(2, a).
pred_dup(2, b).
pred_dup(3, b).
pred_dup(4, c).
pred_dup(1, c).
pred_dup(2, d).
pred_dup(5, d).

test "distinct/2" by [
    findall([V, Letter], distinct(Letter, pred_dup(V, Letter)), Solutions),
    Solutions == [
        [1, a],
        [2, b],
        [4, c],
        [2, d]
    ]
].