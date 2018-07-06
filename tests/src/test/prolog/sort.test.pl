test "sort strings lexicogaphically" by [
    sort(["foo", "bar", "allegato", "zeppelin", "sabbath"], [E0, E1, E2, E3, E4]),
    E0 = "allegato",
    E1 = "bar",
    E2 = "foo",
    E3 = "sabbath",
    E4 = "zeppelin"
].

test "sort numbers" by [
    sort([5, 2, 1, 9, 200, 10], [E0, E1, E2, E3, E4, E5]),
    E0 = 1,
    E1 = 2,
    E2 = 5,
    E3 = 9,
    E4 = 10,
    E5 = 200
].

test "sort atoms" by [
    sort([a, zz, ff, za, zu], [E0, E1, E2, E3, E4]),
    E0 = a,
    E1 = ff,
    E2 = za,
    E3 = zu,
    E4 = zz
].

test "sort/2 removes duplicates" by [
    sort([5, 2, 1, 2, 4, 9912, 5, 22, 10], [E0, E1, E2, E3, E4, E5, E6]),
    E0 = 1,
    E1 = 2,
    E2 = 4,
    E3 = 5,
    E4 = 10,
    E5 = 22,
    E6 = 9912
].