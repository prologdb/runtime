package com.github.prologdb.runtime.knowledge.library

import com.github.prologdb.async.LazySequence
import com.github.prologdb.runtime.PrologRuntimeException
import com.github.prologdb.runtime.knowledge.Rule
import com.github.prologdb.runtime.term.Predicate
import com.github.prologdb.runtime.unification.Unification

/**
 * The most simple implementation of [MutableClauseStore] possible: is
 * based on a plain [MutableList] and uses the default implementations
 * declared in [ClauseStore] and [MutableClauseStore]
 */
class SimpleClauseStore(givenEntries: Iterable<Clause> = emptyList()) : MutableClauseStore {
    private val entries = ArrayList(givenEntries.toList())

    override val exports = entries

    override fun add(entry: Clause) {
        entries.add(entry)
    }

    override fun retractFact(fact: Predicate): LazySequence<Unification> {
        return LazySequence.fromGenerator {
            for (index in 0 until entries.size) {
                val entry = entries[index]
                if (entry is Predicate) {
                    val unification = entry.unify(fact)
                    if (unification != null) {
                        entries.removeAt(index)
                        return@fromGenerator unification
                    }
                }
            }

            null
        }
    }

    override fun retract(unifiesWith: Predicate): LazySequence<Unification> {
        return LazySequence.fromGenerator {
            for (index in 0 until entries.size) {
                val entry = entries[index]
                if (entry is Predicate) {
                    val unification = entry.unify(unifiesWith)
                    if (unification != null) {
                        entries.removeAt(index)
                        return@fromGenerator unification
                    }
                } else if (entry is Rule) {
                    val headUnification = entry.head.unify(unifiesWith)
                    if (headUnification != null) {
                        entries.removeAt(index)
                        return@fromGenerator headUnification
                    }
                } else {
                    throw PrologRuntimeException("Cannot test whether to retract an entry: is neither a fact nor a rule")
                }
            }

            null
        }
    }

    override fun abolishFacts(functor: String, arity: Int) {
        entries.removeAll(entries.filter { it.arity == arity && it is Predicate && it.name == functor  })
    }

    override fun abolish(functor: String, arity: Int) {
        entries.removeAll(entries.filter { it.arity == arity && it.name == functor })
    }
}