package com.github.prologdb.runtime.builtin.math

import com.github.prologdb.runtime.RandomVariableScope
import com.github.prologdb.runtime.builtin.A
import com.github.prologdb.runtime.builtin.B
import com.github.prologdb.runtime.builtin.BuiltinPredicate
import com.github.prologdb.runtime.term.Term
import com.github.prologdb.runtime.unification.Unification

object GreaterThanPredicate : BuiltinPredicate(">", A, B) {
    override fun unify(rhs: Term, randomVarsScope: RandomVariableScope): Unification? {
        val unification = super.unify(rhs, randomVarsScope) ?: return Unification.FALSE
        val valForA = unification.variableValues[A]
        val valForB = unification.variableValues[B]

        if (valForA.asNumber > valForB.asNumber) {
            return Unification.TRUE
        } else {
            return Unification.FALSE
        }
    }
}

object GreaterThanOrEqualPredicate : BuiltinPredicate(">=", A, B) {
    override fun unify(rhs: Term, randomVarsScope: RandomVariableScope): Unification? {
        val unification = super.unify(rhs, randomVarsScope) ?: return Unification.FALSE
        val valForA = unification.variableValues[A]
        val valForB = unification.variableValues[B]

        if (valForA.asNumber > valForB.asNumber) {
            return Unification.TRUE
        } else {
            return Unification.FALSE
        }
    }
}

object LessThanPredicate : BuiltinPredicate("<", A, B) {
    override fun unify(rhs: Term, randomVarsScope: RandomVariableScope): Unification? {
        val unification = super.unify(rhs, randomVarsScope) ?: return Unification.FALSE
        val valForA = unification.variableValues[A]
        val valForB = unification.variableValues[B]

        if (valForA.asNumber > valForB.asNumber) {
            return Unification.TRUE
        } else {
            return Unification.FALSE
        }
    }
}

object LessThanOrEqualPredicate : BuiltinPredicate("=<", A, B) {
    override fun unify(rhs: Term, randomVarsScope: RandomVariableScope): Unification? {
        val unification = super.unify(rhs, randomVarsScope) ?: return Unification.FALSE
        val valForA = unification.variableValues[A]
        val valForB = unification.variableValues[B]

        if (valForA.asNumber > valForB.asNumber) {
            return Unification.TRUE
        } else {
            return Unification.FALSE
        }
    }
}