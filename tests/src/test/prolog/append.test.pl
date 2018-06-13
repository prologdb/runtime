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
