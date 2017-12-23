package com.github.prologdb.runtime.knowledge.library

import com.github.prologdb.runtime.PrologRuntimeException
import com.github.prologdb.runtime.knowledge.Rule
import com.github.prologdb.runtime.term.Predicate
import com.github.prologdb.runtime.unification.Unification

/**
 * The most simple implementation of [MutableLibraryEntryStore] possible: is
 * based on a plain [MutableList] and uses the default implementations
 * declared in [LibraryEntryStore] and [MutableLibraryEntryStore]
 */
class SimpleLibraryEntryStore(givenEntries: Iterable<LibraryEntry> = emptyList()) : MutableLibraryEntryStore {
    private val entries = ArrayList(givenEntries.toList())

    override val exports = entries

    override fun add(entry: LibraryEntry) {
        entries.add(entry)
    }

    override fun retractFact(fact: Predicate): Unification? {
        for (i in 0 .. entries.lastIndex) {
            val entry = entries[i]
            if (entry is Predicate) {
                val unification = entry.unify(fact)
                if (unification != null) {
                    entries.removeAt(i)
                    return unification
                }
            }
        }

        return null
    }

    override fun retract(unifiesWith: Predicate): Unification? {
        for (i in 0 .. entries.lastIndex) {
            val entry = entries[i]
            if (entry is Predicate) {
                val unification = entry.unify(unifiesWith)
                if (unification != null) {
                    entries.removeAt(i)
                    return unification
                }
            } else if (entry is Rule) {
                val headUnification = entry.head.unify(unifiesWith)
                if (headUnification != null) {
                    entries.removeAt(i)
                    return headUnification
                }
            } else {
                throw PrologRuntimeException("Cannot test whether to retract an entry: is neither a fact nor a rule")
            }
        }

        return null
    }

    override fun abolishFacts(functor: String, arity: Int): Boolean {
        return entries.removeAll(entries.filter { it.arity == arity && it is Predicate && it.name == functor  })
    }

    override fun abolish(functor: String, arity: Int): Boolean {
        return entries.removeAll(entries.filter { it.arity == arity && it.name == functor })
    }
}