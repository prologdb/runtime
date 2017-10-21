package com.github.tmarsteel.ktprolog.term

import com.github.tmarsteel.ktprolog.knowledge.RandomVariableScope
import com.github.tmarsteel.ktprolog.unification.Unification
import com.github.tmarsteel.ktprolog.unification.VariableBucket

open class Variable(val name: String) : Term {
    override fun unify(rhs: Term, randomVariableScope: RandomVariableScope): Unification {
        if (rhs is Variable && rhs == this) return Unification.TRUE

        val vars = VariableBucket()
        vars.instantiate(this, rhs)
        return Unification(vars)
    }

    override val variables: Set<Variable>
        get() = setOf(this)

    override fun substituteVariables(mapper: (Variable) -> Term): Term {
        return mapper(this)
    }

    override fun toString(): String {
        return name
    }

    override fun equals(other: Any?): Boolean {
        if (this === other) return true
        if (other !is Variable) return false

        if (name != other.name) return false

        return true
    }

    override fun hashCode(): Int {
        return name.hashCode()
    }

    companion object {
        val ANONYMOUS: Variable = AnonymousVariable
    }
}

private object AnonymousVariable : Variable("_") {
    override fun unify(rhs: Term, randomVariableScope: RandomVariableScope): Unification {
        val randomVar = randomVariableScope.createNewRandomVariable()
        val bucket = VariableBucket()
        bucket.instantiate(randomVar, rhs)
        return Unification(bucket)
    }
}