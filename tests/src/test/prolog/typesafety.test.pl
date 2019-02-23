test "atom/1 with atom" by [
    atom(a)
].

test "atom/1 with number" by [
    \+ atom(1),
    \+ atom(1.22)
].

test "atom/1 with compound term" by [
    \+ atom(fn(arg))
].

test "atom/1 with list" by [
    \+ atom([list])
].

test "atom/1 with string" by [
    \+ atom("string")
].