:- module($equality).

:- native 'not'/1.
:- native '\\+'/1.
:- native '='/2.
:- native '\\='/2.
:- native '=='/2.
:- native '\\=='/2.
:- native '=@='/2.
:- native '\\=@='/2.

:- module_transparent('not'/1).
:- module_transparent('\\+'/1).

:- semidet(not/1).
:- semidet('='/2).
:- semidet('\\='/2).
:- semidet('=='/2).
:- semidet('\\=='/2).
:- semidet('=@='/2).
:- semidet('\\=@='/2).

