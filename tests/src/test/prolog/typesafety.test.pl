test "atom/1 with atom" by [
    atom(a)
].

test "atom/1 with number" by [
    not(atom(1)),
    not(atom(1.22))
].

test "atom/1 with predicate" by [
    not(atom(predicate(arg)))
].

test "atom/1 with list" by [
    not(atom([list]))
].

test "atom/1 with string" by [
    not(atom("string"))
].