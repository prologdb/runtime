package com.github.prologdb.runtime.builtin.lists

import com.github.prologdb.runtime.builtin.A
import com.github.prologdb.runtime.builtin.B
import com.github.prologdb.runtime.builtin.C
import com.github.prologdb.runtime.builtin.X
import com.github.prologdb.runtime.knowledge.ASTPrologPredicate
import com.github.prologdb.runtime.knowledge.Rule
import com.github.prologdb.runtime.knowledge.library.ClauseIndicator
import com.github.prologdb.runtime.query.PredicateInvocationQuery
import com.github.prologdb.runtime.term.CompoundTerm
import com.github.prologdb.runtime.term.PrologList

/**
 * Implements the append/3 builtin:
 *
 *     append([], L, L).
 *     append([H|T], L2, [H|R]) :- append(T, L2, R).
 */
internal val AppendBuiltin = ASTPrologPredicate(ClauseIndicator.of("append", 3), null).apply {
    assertz(
        // append([], L, L) :- list(L).
        CompoundTerm("append", arrayOf(
            PrologList(emptyList()),
            X,
            X
        ))
    )

    assertz(
        // append([H|T], L2, [H|R]) :- list(L2), append(T, L2, R).
        Rule(
            CompoundTerm("append", arrayOf(
                PrologList(listOf(A), X),
                B,
                PrologList(listOf(A), C)
            )),
            PredicateInvocationQuery(CompoundTerm("append", arrayOf(X, B, C)))
        )
    )
}
