:- use_module(test('operator_export_source.test.pl')).
:- use_module(essential($dynamic)).

test "exported operator is defined in importing module" by [
    compound_name_arguments(a foobar b, 'foobar', [a, b])
].