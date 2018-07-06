test "string_chars/2 conversion from string to list" by [
    string_chars("foobar", X),
    X = [f, o, o, b, a, r]
].

test "string_chars/2 conversion from list to string" by [
    string_chars(X, [f, o, o, b, a, r]),
    X = "foobar"
].

test "string_chars/2 with both instantiated: equal" by [
    string_chars("foobar", [f, o, o, b, a, r])
].

test "string_chars/2 with both instantiated: not equal" by [
    \+ string_chars("foo", [b, a, r])
].