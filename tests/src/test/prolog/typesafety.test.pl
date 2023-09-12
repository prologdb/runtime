:- use_module(essential($equality)).
:- use_module(essential($typesafety)).

% -----

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

test "atom/1 with dict" by [
    \+ atom({dict: dict})
].

test "atom/1 with unbound" by [
    \+ atom(Unbound)
].

% -----

test "integer/1 with atom" by [
    \+ integer(a)
].

test "integer/1 with number" by [
    integer(1),
    \+ integer(1.0000000000000000000000000000004),
    \+ integer(1.22)
].

test "integer/1 with compound term" by [
    \+ integer(fn(arg))
].

test "integer/1 with list" by [
    \+ integer([list])
].

test "integer/1 with string" by [
    \+ integer("string")
].

test "integer/1 with dict" by [
    \+ integer({dict: dict})
].

test "integer/1 with unbound" by [
    \+ integer(Unbound)
].

% -----

test "decimal/1 with atom" by [
    \+ decimal(a)
].

test "decimal/1 with number" by [
    \+ decimal(1),
    decimal(1.0000000000000000000000000000004),
    decimal(1.22)
].

test "decimal/1 with compound term" by [
    \+ decimal(fn(arg))
].

test "decimal/1 with list" by [
    \+ decimal([list])
].

test "decimal/1 with string" by [
    \+ decimal("string")
].

test "decimal/1 with dict" by [
    \+ decimal({dict: dict})
].

test "decimal/1 with unbound" by [
    \+ decimal(Unbound)
].

% -----

test "number/1 with atom" by [
    \+ number(a)
].

test "number/1 with number" by [
    number(1),
    number(1.22)
].

test "number/1 with compound term" by [
    \+ number(fn(arg))
].

test "number/1 with list" by [
    \+ number([list])
].

test "number/1 with string" by [
    \+ number("string")
].

test "number/1 with dict" by [
    \+ number({dict: dict})
].

test "number/1 with unbound" by [
    \+ number(Unbound)
].

% -----

test "string/1 with atom" by [
    \+ string(a)
].

test "string/1 with number" by [
    \+ string(1),
    \+ string(1.22)
].

test "string/1 with compound term" by [
    \+ string(fn(arg))
].

test "string/1 with list" by [
    \+ string([list])
].

test "string/1 with string" by [
    string("string")
].

test "string/1 with dict" by [
    \+ string({dict: dict})
].

test "string/1 with unbound" by [
    \+ string(Unbound)
].

% -----

test "is_list/1 with atom" by [
    \+ is_list(a)
].

test "is_list/1 with number" by [
    \+ is_list(1),
    \+ is_list(1.22)
].

test "is_list/1 with compound term" by [
    \+ is_list(fn(arg))
].

test "is_list/1 with list" by [
    is_list([list])
].

test "is_list/1 with string" by [
    is_list("string")
].

test "is_list/1 with dict" by [
    \+ integer({dict: dict})
].

test "is_list/1 with unbound" by [
    \+ is_list(Unbound)
].

% -----

test "var/1 with atom" by [
    \+ var(a)
].

test "var/1 with number" by [
    \+ var(1),
    \+ var(1.22)
].

test "var/1 with compound term" by [
    \+ var(fn(arg))
].

test "var/1 with list" by [
    \+ var([list])
].

test "var/1 with string" by [
    \+ var("string")
].

test "var/1 with dict" by [
    \+ atom({dict: dict})
].

test "var/1 with unbound" by [
    var(Unbound)
].

% -----

test "nonvar/1 with atom" by [
    atom(a)
].

test "nonvar/1 with number" by [
    nonvar(1),
    nonvar(1.22)
].

test "nonvar/1 with compound term" by [
    nonvar(fn(arg))
].

test "nonvar/1 with list" by [
    nonvar([list])
].

test "nonvar/1 with string" by [
    nonvar("string")
].

test "nonvar/1 with dict" by [
    nonvar({dict: dict})
].

test "nonvar/1 with unbound" by [
    \+ nonvar(Unbound)
].

% -----

test "ground/1 with atom" by [
    ground(a)
].

test "ground/1 with number" by [
    ground(1),
    ground(1.22)
].

test "ground/1 with ground compound term" by [
    ground(fn(arg))
].

test "ground/1 with nonground compound term" by [
    \+ ground(fn(Unbound))
].

test "ground/1 with ground list" by [
    ground([arg])
].

test "ground/1 with nonground list" by [
    \+ ground([Unbound])
].

test "ground/1 with ground list that has a tail" by [
    \+ ground([arg|Unbound])
].

test "ground/1 with string" by [
    ground("string")
].

test "ground/1 with ground dict" by [
    ground({dict:dict})
].

test "ground/1 with nonground dict" by [
    \+ ground({dict:A})
].

test "ground/1 with ground dict that has a tail" by [
    \+ ground({dict:dict|Unbound})
].

test "ground/1 with unbound" by [
    \+ ground(Unbound)
].

% -----

test "nonground/1 with atom" by [
    \+ nonground(a)
].

test "nonground/1 with number" by [
    \+ nonground(1),
    \+ nonground(1.22)
].

test "nonground/1 with ground compound term" by [
    \+ nonground(fn(arg))
].

test "nonground/1 with nonground compound term" by [
    nonground(fn(Unbound))
].

test "nonground/1 with ground list" by [
    \+ nonground([arg])
].

test "nonground/1 with nonground list" by [
    nonground([Unbound])
].

test "nonground/1 with ground list that has a tail" by [
    nonground([arg|Unbound])
].

test "nonground/1 with string" by [
    \+ nonground("string")
].

test "nonground/1 with ground dict" by [
    \+ nonground({dict:dict})
].

test "nonground/1 with nonground dict" by [
    nonground({dict:A})
].

test "nonground/1 with ground dict that has a tail" by [
    nonground({dict:dict|Unbound})
].

test "nonground/1 with unbound" by [
    nonground(Unbound)
].

% -----

test "typeof/2 with atom" by [
    typeof(a, T),
    T = atom
].

test "typeof/2 with number" by [
    typeof(1, T1),
    T1 = number,
    typeof(1.22, T2),
    T2 = number
].

test "typeof/2 with compound term" by [
    typeof(fn(arg), T),
    T = 'compound term'
].

test "typeof/2 with list" by [
    typeof([list], T),
    T = list
].

test "typeof/2 with string" by [
    typeof("string", T),
    T = string
].

test "typeof/2 with dict" by [
    typeof({dict: dict}, T),
    T = dict
].

test "typeof/2 with unbound" by [
    typeof(Unbound, T),
    T = variable
].