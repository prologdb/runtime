package com.github.tmarsteel.ktprolog.term

import com.github.tmarsteel.ktprolog.RandomVariableScope
import com.github.tmarsteel.ktprolog.unification.Unification
import com.github.tmarsteel.ktprolog.unification.VariableBucket

object AnonymousVariable : Variable("_") {
    override fun unify(rhs: Term, randomVarsScope: RandomVariableScope): Unification {
        val randomVar = randomVarsScope.createNewRandomVariable()
        val bucket = VariableBucket()
        bucket.instantiate(randomVar, rhs)
        return Unification(bucket)
    }
}