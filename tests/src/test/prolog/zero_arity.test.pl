:- use_module(essential($equality)).

succeeding() :- 1 = 1.
zero_arity_invocation() :- succeeding.
zero_arity_declaration :- succeeding().

test "invocation of /0 without parenthesis" by [
    zero_arity_invocation
].

test "invoking a predicate of /0 declared without parenthesis" by [
    zero_arity_declaration
].