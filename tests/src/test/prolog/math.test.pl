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
