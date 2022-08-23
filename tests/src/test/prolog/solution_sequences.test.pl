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

test "call_nth/2 - index unbound" by [
    findall([Index, Letter], call_nth(pred_dup(_, Letter), Index), Solutions),
    Solutions == [
        [0, a],
        [1, a],
        [2, b],
        [3, b],
        [4, c],
        [5, c],
        [6, d],
        [7, d]
    ]
].

test "call_nth/2 - index bound" by [
    findall(Letter, call_nth(pred_dup(_, Letter), 3), Solutions),
    Solutions == [b]
].

test "call_nth/2 - index used in goal" by [
    findall([Index, Letter], call_nth(pred_dup(Index, Letter), Index), Solutions),
    Solutions == [
        [2, b],
        [3, b],
        [4, c]
    ]
].

test "order_by/2 asc" by [
    findall(Letter, order_by(asc(Number), pred_dup(Number, Letter)), Solutions),
    Solutions == [a, c, a, b, d, b, c, d]
].

test "order_by/2 desc" by [
    findall(Letter, order_by(desc(Number), pred_dup(Number, Letter)), Solutions),
    Solutions == [d, c, b, d, b, a, c, a]
].

test "group_by/4" by [
    findall([Letter, Bag], group_by(Letter, Number, pred_dup(Number, Letter), Bag), Solutions),
    Solutions == [
        [a, [1, 2]],
        [b, [2, 3]],
        [c, [4, 1]],
        [d, [2, 5]]
    ]
].