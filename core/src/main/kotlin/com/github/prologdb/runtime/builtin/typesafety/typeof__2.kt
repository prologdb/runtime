package com.github.prologdb.runtime.builtin.typesafety

import com.github.prologdb.runtime.PrologRuntimeException
import com.github.prologdb.runtime.builtin.prologBuiltin
import com.github.prologdb.runtime.lazysequence.LazySequence
import com.github.prologdb.runtime.term.Atom
import com.github.prologdb.runtime.term.List
import com.github.prologdb.runtime.term.PrologString
import com.github.prologdb.runtime.term.Variable
import com.github.prologdb.runtime.unification.Unification

val TypeofBuiltin = prologBuiltin("typeof", 2, { args, _, _ ->
    val arg0 = args[0]
    val arg1 = args[1]

    if (arg1 is Variable) {
        val actualValueArg1 = Atom(arg0.prologTypeName)
        return@prologBuiltin LazySequence.ofNullable(arg1.unify(actualValueArg1))
    } else {
        if (arg1 !is Atom) throw PrologRuntimeException("Type error: argument 2 to typeof/2 must be an atom or unbound")

        // typecheck; "abc" typeof list should also be true
        val correct =
            (arg0 is List && arg1.name == "list")
                ||
                (arg0 is PrologString && arg1.name == "string")
                ||
                (arg0.prologTypeName == arg1.name)

        return@prologBuiltin LazySequence.ofNullable(Unification.whether(correct))
    }
})