:- use_module(essential($math)).
:- use_module(essential($equality)).
:- use_module(essential($comparison)).

% is/2

test "is/2: both instantiated, equal" by [
    3 is 3
].

test "is/2: both instantiated, different" by [
    \+ -12 is 10000
].

test "is/2: LHS not instantiated, RHS number" by [
    X is 3,
    X = 3
].

test "is/2: LHS not instantiated, RHS expression" by [
    X is 4 * 3,
    X = 12
].

test "is/2: LHS number, RHS not instantiated" by [
    12141 is X,
    X = 12141
].

test "is/2: LHS expression, RHS not instantiated" by [
    1323 - 997 is X,
    X = 326
].

test "is/2: 64bit overflow with plus" by [
    Small = 9223372036854775807, % pow(2, 63) - 1, Javas Long.MAX_VALUE
    Large is Small + 3,
    Large == 9223372036854775810
].

test "is/2: 64bit underflow with minus" by [
    Small = -9223372036854775808, % pow(2, 63) - 1, Javas Long.MAX_VALUE
    Large is Small - 3,
    Large == -9223372036854775811
].

test "is/2: 64bit overflow with multiply" by [
    Small = 922337203685477580,
    Large is Small * 100,
    Large == 92233720368547758000
].

test "is/2: dividing integers doesn't floor" by [
    X is 2 / 3,
    X == 0.6666666666666666
].

% SWI and GNU-Prolog have this very weird behaviour:
%
% ?- 10000000000000004.0 @< 10000000000000003.
% true.
%
% That happens because the IEEE floating point inprecision results
% in the integer being equal to the larger float in binary representation
% in case of equality, the float is always considered less
%
% In GNU-Prolog, this is understandable because it only supports fixed-size
% arithmetic and overflows like most imperative languages.
% SWI, however, supports arbitrarily-sized numbers and still ahs this behaviour,
% which can lead to very surprising bugs.
% the following tests should make sure PrologDB has logical arithmetic
test "is/2 float precision in comparison" by [
   10000000000000004.0 @> 10000000000000003
].

test "is/2 precision loss in parsing versus computation" by [
    % this cannot be accurately represented by an IEEE 64-bit float
    ParsedProblematic = 10000000000000004.0,
    CalculatedProblematic is 10000000000000000.0 + 3.0,
    ParsedProblematic \= CalculatedProblematic,
    ParsedProblematic > CalculatedProblematic
].

% >/2

test "greater than - greater" by [
    5 > 2
].

test "greater than - equal" by [
    \+ 134 > 134
].

test "greater than - less" by [
    \+ 5 > 9822
].

test "greater than with variable LHS" by [
    X = 5,
    X > 2
].

test "greater than with variable RHS" by [
    X = 2,
    5 > X
].

test "greater than with both variables" by [
    X = 5,
    Y = 2,
    X > Y
].

% >=/2

test "greater than or equal - greater" by [
    5 >= 2
].

test "greater than or equal - equal" by [
    134 >= 134
].

test "greater than or equal - less" by [
    \+ 5 >= 9822
].

test "greater than or equal with variable LHS" by [
    X = 5,
    X >= 2
].

test "greater than or equal with variable RHS" by [
    X = 2,
    5 >= X
].

test "greater than or equal with both variables" by [
    X = 5,
    Y = 2,
    X >= Y
].

% </2

test "less than - greater" by [
    \+ 5 < 2
].

test "less than - equal" by [
    \+ 134 < 134
].

test "less than - less" by [
    5 < 9822
].

test "less than with variable LHS" by [
    X = 2,
    X < 5
].

test "less than with variable RHS" by [
    X = 5,
    2 < X
].

test "less than with both variables" by [
    X = 2,
    Y = 5,
    X < Y
].

% =</2

test "less than or equal - greater" by [
    \+ 5 =< 2
].

test "less than or equal - equal" by [
    134 =< 134
].

test "less than or equal - less" by [
    5 =< 9822
].

test "less than or equal with variable LHS" by [
    X = 2,
    X =< 5
].

test "less than or equal with variable RHS" by [
    X = 5,
    2 =< X
].

test "less than or equal with both variables" by [
    X = 2,
    Y = 5,
    X =< Y
].
