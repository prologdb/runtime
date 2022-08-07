:- module(aggregate, [
    op(1100, xfx, as),
    reduce/2,
    min/4,
    min/5
]).

:- native reduce/2.
:- module_transparent(reduce/2).

min(reductor, initialize, min(_), {initial: true}).

min(reductor, accumulate, min(Of), {initial: true}, {acc: Of}).
min(reductor, accumulate, min(Of), {acc: Acc}, {acc: Acc}) :- Of >= Acc.
min(reductor, accumulate, min(Of), {acc: Acc}, {acc: Of}) :- Of < Acc.

min(reductor, finalize, min(_), {initial: true}, _).
min(reductor, finalize, min(_), {acc: Min}, Min).