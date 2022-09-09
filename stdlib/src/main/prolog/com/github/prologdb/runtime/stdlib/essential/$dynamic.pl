:- module($dynamic).

:- native findall/3.
:- native findall_o/3.
:- native findnsols/4.
:- native call/1.
:- native compound_name_arguments/3.
:- native apply/2.
:- native term_variables/2.
:- native current_op/3.
:- native current_module/1.
:- native qualify_callable/3.

:- module_transparent(findall/3).
:- module_transparent(findall_o/3).
:- module_transparent(findnsols/4).
:- module_transparent(call/1).
:- module_transparent(apply/2).
:- module_transparent(current_op/3).
:- module_transparent(current_module/1).
:- module_transparent(qualify_callable/3).
:- module_transparent(once/1).

once(Goal) :- $dynamic:findnsols(1, Goal, Goal, [Goal]).
