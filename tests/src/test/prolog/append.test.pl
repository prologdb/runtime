:- use_module(library(lists), [append/3, length/2, member/2]).
:- use_module(essential($equality), ['='/2]).
:- use_module(essential($dynamic), [findall/3]).
:- use_module(essential($typesafety), [is_list/1]).

test "append/3 - two empty lists" by [
    append([], [], X),
    X = []
].

test "append/3 - to empty list" by [
    append([], [a, b, c], X),
    X = [a, b, c]
].

test "append/3 - two lists" by [
    append([a, b], [c, d], X),
    X = [a, b, c, d]
].

test "append/3 - first argument missing" by [
    append(X, [c, d], [a, b, c, d]),
    X = [a, b]
].

test "append/3 - second argument missing" by [
    append([a, b], X, [a, b, c, d|T]),
    X = [c, d|T]
].

test "append/3 - combinations" by [
    findall([A, B], append(A, B, [a, b, c]), L),
    is_list(L),
    length(L, 4),
    member([[], [a, b, c]], L),
    member([[a], [b, c]], L),
    member([[a, b], [c]], L),
    member([[], [a, b, c]], L)
].
