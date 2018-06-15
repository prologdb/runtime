test "variable is less than number" by [
    var(Z),
    Z @< 1
].

test "variable is less than string" by [
    var(Z),
    Z @< "A"
].

test "variable is less than atom" by [
    var(Z),
    Z @< a
].

test "variable is less than list" by [
    var(Z),
    Z @< [a]
].

test "variable is less than predicate" by [
    var(Z),
    Z @< foo(bar)
].

% ------------------------------

test "number is less than string" by [
    9999 @< "1"
].

test "number is less than atom" by [
    9999 @< a
].

test "number is less than list" by [
    9999 @< [a]
].

test "number is less than predicate" by [
    9999 @< a(a)
].

% ------------------------------

test "string is less than atom" by [
    "9999" @< a
].

test "string is less than list" by [
    "ZZZZ" @< [a]
].

test "string is less than predicate" by [
    "ZZZZ" @< a(a)
].

% ------------------------------

test "atom is less than list" by [
    zzzz @< [1]
].

test "atom is less than predicate" by [
    zzzz @< a(a)
].

% ------------------------------

test "list is less than predicate" by [
    [z] @< a(a)
].