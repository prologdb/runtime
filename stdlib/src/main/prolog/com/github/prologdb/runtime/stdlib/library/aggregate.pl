:- module(aggregate, [
    op(1100, xfx, as),
    reduce/2
]).

:- native reduce/2.
:- module_transparent(reduce/2).