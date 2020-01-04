package com.github.prologdb.runtime.builtin.lists

import com.github.prologdb.runtime.PrologRuntimeException
import com.github.prologdb.runtime.analyzation.constraint.ConstrainedTerm
import com.github.prologdb.runtime.builtin.builtinArgumentVariables
import com.github.prologdb.runtime.builtin.nativeRule
import com.github.prologdb.runtime.term.CompoundTerm
import com.github.prologdb.runtime.term.PrologList
import com.github.prologdb.runtime.term.Variable

/**
 * Implements the builtin member/2, see http://www.swi-prolog.org/pldoc/man?predicate=member/2
 */
internal val MemberBuiltin = nativeRule("member", 2) { args, ctxt ->
    val member = args[0]
    val list = args[1]

    when (list) {
        is PrologList -> for (element in list.elements) {
            element.unify(member, ctxt.randomVariableScope)?.let { yield(it) }
        }
        is Variable -> {
            var nRandoms = 0
            while (true) {
                val elements = (0..nRandoms).map { index ->
                    if (index == 0) member else ctxt.randomVariableScope.createNewRandomVariable()
                }
                yield(list.unify(PrologList(elements, ctxt.randomVariableScope.createNewRandomVariable()), ctxt.randomVariableScope))
                nRandoms++
            }
        }
        else -> throw PrologRuntimeException("Argument 1 to member/2 must be a list, got ${list.prologTypeName}")
    }
}.apply {
    behavesDeterministicGiven(ConstrainedTerm(
        CompoundTerm("member", arrayOf(
            builtinArgumentVariables[0],
            PrologList(listOf(builtinArgumentVariables[0]))
        )),
        emptyMap()
    ))
}
