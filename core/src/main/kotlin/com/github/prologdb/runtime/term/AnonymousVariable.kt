package com.github.prologdb.runtime.term

import com.github.prologdb.runtime.RandomVariableScope
import com.github.prologdb.runtime.unification.Unification

class AnonymousVariable : Variable("_") {
    override fun unify(rhs: Term, randomVarsScope: RandomVariableScope): Unification {
        return Unification.of(randomVarsScope.createNewRandomVariable(), rhs)
    }
}