:- use_module(library(equality)).
:- use_module(library(typesafety)).
:- use_module(library(comparison)).

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

test "variable is less than compound term" by [
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

test "number is less than compound term" by [
    9999 @< a(a)
].

% ------------------------------

test "string is less than atom" by [
    "9999" @< a
].

test "string is less than list" by [
    "ZZZZ" @< [a]
].

test "string is less than compound term" by [
    "ZZZZ" @< a(a)
].

% ------------------------------

test "atom is less than list" by [
    zzzz @< [1]
].

test "atom is less than compound term" by [
    zzzz @< a(a)
].

% ------------------------------

test "list is less than compound term" by [
    [z] @< a(a)
].

% ------------------------------

test "integer compared to decimal: compared as decimal" by [
    % IEEE floating point inprecision results in the integer being equal
    % to the larger float in binary representation
    % in case of equality, the float is always considered less
    10000000000000004.0 @< 10000000000000003
].

% ------------------------------

test "compare integers" by [
    2 @> 1
].

test "compare floats" by [
    3.2 @> 2.9
].

test "compare atom" by [
    x @> t
].

test "empty list is less than with content" by [
    [] @< [z]
].
