:- use_module(library(lists)).
:- use_module(essential($dynamic)).
:- use_module(essential($equality)).
:- use_module(essential($typesafety)).

test "both arguments unbound" by [
    findall_o([Length, List], length(List, Length), [R0, R1, R2|_]),
    R0 = [0, []],
    R1 = [1, [E1_1]], var(E1_1),
    R2 = [2, [E2_1, E2_2]], var(E2_1), var(E2_2), E2_1 \== E2_2,
    R3 = [3, [E3_1, E3_2, E3_3]], var(E3_1), var(E3_2), var(E3_3), E3_1 \== E3_2, E3_1 \== E3_3, E3_2 \== E3_3
].

test "length of empty list" by [
    length([], Len),
    Len = 0
].

test "length of list with tail" by [
    findall_o([Tail, Length], length([a|Tail], Length), [R0, R1, R2, R3|_]),
    R0 = [[], 1],
    R1 = [[E1_1], 2], var(E1_1),
    R2 = [[E2_1, E2_3], 3], var(E2_1), var(E2_2), E2_1 \== E2_2,
    R3 = [[E3_1, E3_2, E3_3], 4], var(E3_1), var(E3_2), var(E3_3), E3_1 \== E3_2, E3_1 \== E3_3, E3_2 \== E3_3
].

test "generate list of fixed length" by [
    length(List, 3),
    List = [E1, E2, E3],
    var(E1), var(E2), var(E3),
    E1 \== E2, E1 \== E3, \E2 \== E3
].
