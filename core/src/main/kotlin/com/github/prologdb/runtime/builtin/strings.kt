package com.github.prologdb.runtime.builtin

import com.github.prologdb.runtime.PrologRuntimeException
import com.github.prologdb.runtime.RandomVariableScope
import com.github.prologdb.runtime.knowledge.KnowledgeBase
import com.github.prologdb.runtime.knowledge.Rule
import com.github.prologdb.runtime.knowledge.library.DefaultOperatorRegistry
import com.github.prologdb.runtime.knowledge.library.DoublyIndexedLibraryEntryStore
import com.github.prologdb.runtime.knowledge.library.Library
import com.github.prologdb.runtime.knowledge.library.SimpleLibrary
import com.github.prologdb.runtime.lazysequence.LazySequence
import com.github.prologdb.runtime.query.PredicateQuery
import com.github.prologdb.runtime.term.Atom
import com.github.prologdb.runtime.term.Predicate
import com.github.prologdb.runtime.term.PrologString
import com.github.prologdb.runtime.term.Variable
import com.github.prologdb.runtime.term.List as PrologList
import com.github.prologdb.runtime.term.Integer as PrologInteger
import com.github.prologdb.runtime.unification.Unification
import com.github.prologdb.runtime.unification.VariableBucket

val StringsLibrary : Library = object : SimpleLibrary(DoublyIndexedLibraryEntryStore(), DefaultOperatorRegistry()) {
    init {
        add(StringCharsRule) // string_chars/2
    }
}

private object StringCharsPredicate : BuiltinPredicate("string_chars", A, B)

object StringCharsRule : Rule(StringCharsPredicate, PredicateQuery(StringCharsPredicate)) {
    override fun fulfill(predicate: Predicate, kb: KnowledgeBase, randomVariableScope: RandomVariableScope): LazySequence<Unification> {
        if (predicate.name != head.name || predicate.arity != head.arity) return Unification.NONE

        val valForA = predicate.arguments[0]
        val valForB = predicate.arguments[1]

        fun convertValueForBToPrologString(): PrologString {
            // single-character atoms to string
            if (valForB !is PrologList) throw PrologRuntimeException("Type Error: expected list as second argument to string_chars/2, got ${valForB.prologTypeName}")
            if (valForB.tail != null) throw PrologRuntimeException("Type Error: expected list as second argument to string_chars/2, got compound")

            val stringCharsTarget = CharArray(valForB.elements.size)
            valForB.elements.forEachIndexed { index, listElement ->
                if (listElement is Atom && listElement.name.length == 1) {
                    stringCharsTarget[index] = listElement.name[0]
                }
                else throw PrologRuntimeException("Type Error: expected character, found ${listElement.prologTypeName}")
            }

            return PrologString(stringCharsTarget)
        }

        fun convertValueForAToListOfCharacters(): PrologList {
            if (valForA !is PrologString) throw PrologRuntimeException("Type Error: expected string as first argument to string_chars/2, got ${valForA.prologTypeName}")
            return PrologList(valForA.characters.map { Atom(it.toString()) })
        }

        if (valForA is Variable && valForB is Variable) {
            throw PrologRuntimeException("Arguments are not sufficiently instantiated")
        }

        if (valForA is Variable) {
            val prologStringFromList = convertValueForBToPrologString()
            val bucket = VariableBucket()
            bucket.instantiate(valForA, prologStringFromList)
            return LazySequence.of(Unification(bucket))
        }

        if (valForB is Variable) {
            val listFromString = convertValueForAToListOfCharacters()
            val bucket = VariableBucket()
            bucket.instantiate(valForB, listFromString)
            return LazySequence.of(Unification(bucket))
        }

        val prologStringFromList = convertValueForBToPrologString()

        return if (valForA == prologStringFromList) {
            Unification.SINGLETON
        } else {
            Unification.NONE
        }
    }
}