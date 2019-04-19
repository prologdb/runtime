package com.github.prologdb.runtime.builtin.dict

import com.github.prologdb.runtime.builtin.nativeRule
import com.github.prologdb.runtime.term.PrologDictionary
import com.github.prologdb.runtime.unification.Unification

internal val IsDictBuiltin = nativeRule("is_dict", 1) { args, _ ->
    if (args[0] is PrologDictionary) yield(Unification.TRUE)
}
