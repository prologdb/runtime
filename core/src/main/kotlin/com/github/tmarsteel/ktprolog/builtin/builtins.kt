package com.github.tmarsteel.ktprolog.builtin

import com.github.tmarsteel.ktprolog.term.Predicate
import com.github.tmarsteel.ktprolog.term.Term
import com.github.tmarsteel.ktprolog.term.Variable

internal val surrogateVarLHS = Variable("LHS")
internal val surrogateVarRHS = Variable("RHS")
internal val surrogateVarX = Variable("X")

abstract class BuiltinPredicate(name: String, vararg arguments: Term) : Predicate(name, arguments) {
    override val variables: Set<Variable>
        get() = emptySet()

    override fun substituteVariables(mapper: (Variable) -> Term) = this
}