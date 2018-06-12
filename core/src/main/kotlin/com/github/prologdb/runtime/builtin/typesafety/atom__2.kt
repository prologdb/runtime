package com.github.prologdb.runtime.builtin.typesafety

import com.github.prologdb.runtime.builtin.prologBuiltin
import com.github.prologdb.runtime.lazysequence.LazySequence
import com.github.prologdb.runtime.term.Atom
import com.github.prologdb.runtime.unification.Unification

internal val BuiltinIsAtom = prologBuiltin("atom", 1) { args, _, _ ->
    return@prologBuiltin LazySequence.ofNullable(
        Unification.whether(
            args[0] is Atom
        )
    )
}