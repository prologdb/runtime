:- module($comparison).

:- native '@<'/2.
:- native '@=<'/2.
:- native '@>'/2.
:- native '@>='/2.
:- native compare/3.

:- semidet('@<'/2).
:- semidet('@=<'/2).
:- semidet('@>'/2).
:- semidet('@>='/2).
:- det(compare/3).