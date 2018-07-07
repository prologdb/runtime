test "is_dict/1" by [
    is_dict({a:1}),
    not(is_dict([]))
].

test "is_dict/2" by [
    is_dict(a{a:1}, T),
    T = a,

    is_dict(X{a:1}, T),
    T = X,

    not(is_dict({}, _))
].

test "get_dict/3 with all instantiated" by [
    get_dict(a, {a:1}, 1),

    not(get_dict(a, {a:1}, z))
].

test "get_dict/3 with value unbound" by [
    get_dict(a, {a:c}, V),
    V = c
].

test "get_dict/3 with key unbound" by [
    get_dict(K, {a:1, b:2, c:3}, 3),
    K = c
].

test "get_dict/3 with key and value unbound" by [
    findall(K-V, get_dict(K, {a:1, b:2, c:3}, V), R),
    length(R, 3),
    member(a-1, R),
    member(b-2, R),
    member(c-3, R)
].