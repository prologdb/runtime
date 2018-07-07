package com.github.prologdb.runtime.builtin.dict

import com.github.prologdb.runtime.builtin.prologBuiltin
import com.github.prologdb.runtime.lazysequence.LazySequence
import com.github.prologdb.runtime.term.PrologDictionary
import com.github.prologdb.runtime.unification.Unification

internal val IsDictBuiltin = prologBuiltin("is_dict", 1) { args, _, _ ->
    LazySequence.ofNullable(
        Unification.whether(
            args[0] is PrologDictionary
        )
    )
}

internal val IsDictOfTagBuiltin = prologBuiltin("is_dict", 2) { args, _, _ ->
    val arg0 = args[0] as? PrologDictionary ?: return@prologBuiltin Unification.NONE

    return@prologBuiltin LazySequence.ofNullable(
        arg0.tag?.unify(args[1])
    )
}