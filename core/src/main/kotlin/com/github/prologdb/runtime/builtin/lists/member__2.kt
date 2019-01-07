package com.github.prologdb.runtime.builtin.lists

import com.github.prologdb.runtime.builtin.A
import com.github.prologdb.runtime.builtin.B
import com.github.prologdb.runtime.builtin.X
import com.github.prologdb.runtime.knowledge.Rule
import com.github.prologdb.runtime.query.OrQuery
import com.github.prologdb.runtime.query.PredicateInvocationQuery
import com.github.prologdb.runtime.term.CompoundTerm
import com.github.prologdb.runtime.term.PrologList

/**
 * Implements the builtin member/2, see http://www.swi-prolog.org/pldoc/man?predicate=member/2
 */
internal val MemberBuiltin = Rule(
    CompoundTerm("member", arrayOf(
        X,
        PrologList(listOf(A), B)
    )),
    OrQuery(arrayOf(
        PredicateInvocationQuery(CompoundTerm("=", arrayOf(X, A))),
        PredicateInvocationQuery(CompoundTerm("member", arrayOf(X, B)))
    ))
)