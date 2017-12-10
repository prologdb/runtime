package com.github.prologdb.runtime.term

import com.github.prologdb.runtime.RandomVariableScope
import com.github.prologdb.runtime.unification.Unification
import com.github.prologdb.runtime.unification.VariableBucket

object AnonymousVariable : Variable("_") {
    override fun unify(rhs: Term, randomVarsScope: RandomVariableScope): Unification {
        val randomVar = randomVarsScope.createNewRandomVariable()
        val bucket = VariableBucket()
        bucket.instantiate(randomVar, rhs)
        return Unification(bucket)
    }
}