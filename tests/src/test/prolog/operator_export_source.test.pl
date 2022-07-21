:- module('operator_export_source.test.pl', [
    op(600, xfx, foobar)
]).
:- use_module(essential($dynamic)).

test "exported operator is defined locally" by [
   compound_name_arguments(a foobar b, 'foobar', [a, b])
].