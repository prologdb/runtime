package com.github.prologdb.runtime.builtin

import com.github.prologdb.runtime.term.Predicate
import com.github.prologdb.runtime.term.Term
import com.github.prologdb.runtime.term.Variable

internal val surrogateVarLHS = Variable("LHS")
internal val surrogateVarRHS = Variable("RHS")
internal val A = Variable("A")
internal val B = Variable("B")
internal val X = Variable("X")

abstract class BuiltinPredicate(name: String, vararg arguments: Term) : Predicate(name, arguments) {
    override val variables: Set<Variable>
        get() = emptySet()

    override fun substituteVariables(mapper: (Variable) -> Term) = this
}