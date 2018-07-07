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