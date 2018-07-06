package com.github.prologdb.runtime.builtin.lists

import com.github.prologdb.runtime.builtin.A
import com.github.prologdb.runtime.builtin.B
import com.github.prologdb.runtime.builtin.X
import com.github.prologdb.runtime.knowledge.Rule
import com.github.prologdb.runtime.query.OrQuery
import com.github.prologdb.runtime.query.PredicateQuery
import com.github.prologdb.runtime.term.Predicate
import com.github.prologdb.runtime.term.PrologList

/**
 * Implements the builtin member/2, see http://www.swi-prolog.org/pldoc/man?predicate=member/2
 */
internal val MemberBuiltin = Rule(
    Predicate("member", arrayOf(
        X,
        PrologList(listOf(A), B)
    )),
    OrQuery(arrayOf(
        PredicateQuery(Predicate("=", arrayOf(X, A))),
        PredicateQuery(Predicate("member", arrayOf(X, B)))
    ))
)