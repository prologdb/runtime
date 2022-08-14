:- use_module(library(lists)).
:- use_module(essential($equality)).
:- use_module(essential($dynamic)).
:- use_module(essential($typesafety)).

test "input without tail, output unbound" by [
    reverse([1, 2, 3], R),
    R = [3, 2, 1]
].

test "input without tail, output unbound args switched" by [
    reverse(R, [1, 2, 3]),
    R = [3, 2, 1]
].

test "input without tail, output without tail equal" by [
    reverse([1, 2, 3], [3, 2, 1])
].

test "input without tail, output without tail unequal" by [
    \+ reverse([1, 2, 3], [3, 1, 2])
].

test "input without tail, output with tail" by [
    reverse([1, 2, 3], [3|T]),
    T = [2, 1]
].

test "input without tail, output with tail args switched" by [
    reverse([3|T], [1, 2, 3]),
    T = [2, 1]
].

test "input with tail, output unbound" by [
    findall_o(R, reverse([1, 2|T], R), [R1, R2, R3, R4|_]),
    R1 = [2, 1],
    R2 = [R2Var1, 2, 1], var(R2Var1),
    R3 = [R3Var1, R3Var2, 2, 1], var(R3Var1), var(R3Var2),
    R4 = [R4Var1, R4Var2, R4Var3, 2, 1], var(R4Var1), var(R4Var2), var(R4Var3)
].

test "input with tail, output unbound args switched" by [
    findall_o(R, reverse(R, [1, 2|T]), [R1, R2, R3, R4|_]),
    R1 = [2, 1],
    R2 = [R2Var1, 2, 1], var(R2Var1),
    R3 = [R3Var1, R3Var2, 2, 1], var(R3Var1), var(R3Var2),
    R4 = [R4Var1, R4Var2, R4Var3, 2, 1], var(R4Var1), var(R4Var2), var(R4Var3)
].

test "input with tail, output without tail" by [
    reverse([1, 2|T], [4, 3, 2, 1]),
    T = [3, 4]
].

test "input with tail, output without tail args switched" by [
    reverse([4, 3, 2, 1], [1, 2|T]),
    T = [3, 4]
].

test "input with tail, output with tail" by [
    findall_o([T1, T2], reverse([1, 2, 3|T1], [5, 4, 3|T2]), [[R1T1, R1T2], [R2T1, R2T2], [R3T1, R3T2]|_]),
    R1T1 = [4, 5],
    R1T2 = [2, 1],
    R2T1 = [3, 4, 5],
    R2T2 = [3, 2, 1],
    R3T1 = [R3T1Var, 3, 4, 5],
    R3T2 = [R3T2Var, 3, 2, 1],
    var(R3T1Var),
    R3T1Var == R3T2Var
].