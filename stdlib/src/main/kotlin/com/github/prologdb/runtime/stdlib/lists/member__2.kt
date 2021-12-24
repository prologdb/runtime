package com.github.prologdb.runtime.stdlib.lists

import com.github.prologdb.runtime.ArgumentTypeError
import com.github.prologdb.runtime.stdlib.nativeRule
import com.github.prologdb.runtime.term.PrologList
import com.github.prologdb.runtime.term.Variable

/**
 * Implements the builtin member/2, see http://www.swi-prolog.org/pldoc/man?predicate=member/2
 */
internal val BuiltinMember2 = nativeRule("member", 2) { args, ctxt ->
    val member = args[0]
    val list = args[1]

    when (list) {
        is PrologList -> return@nativeRule yieldAllFinal(list.elements.asSequence().mapNotNull { element ->
            element.unify(member, ctxt.randomVariableScope)
        })
        is Variable -> {
            var nRandoms = 0
            while (true) {
                val elements = (0..nRandoms).map { index ->
                    if (index == 0) member else ctxt.randomVariableScope.createNewRandomVariable()
                }
                yield(list.unify(PrologList(elements, ctxt.randomVariableScope.createNewRandomVariable()), ctxt.randomVariableScope))
                nRandoms++
            }
            @Suppress("UNREACHABLE_CODE") null
        }
        else -> throw ArgumentTypeError(args.indicator, 1, list, PrologList::class.java, Variable::class.java)
    }
}
