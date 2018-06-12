test "is/2: both instantiated, equal" by [
    3 is 3
].

test "is/2: both instantiated, different" by [
    not(-12 is 10000)
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