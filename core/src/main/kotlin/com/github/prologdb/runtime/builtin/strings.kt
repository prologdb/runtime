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
import com.github.prologdb.runtime.unification.Unification
import com.github.prologdb.runtime.term.Integer as PrologInteger
import com.github.prologdb.runtime.term.List as PrologList

val StringsLibrary : Library = object : SimpleLibrary(DoublyIndexedLibraryEntryStore(), DefaultOperatorRegistry()) {
    init {
        add(StringCharsRule) // string_chars/2
    }
}

private object StringCharsPredicate : BuiltinPredicate("string_chars", A, B)

/**
 * Implements `string_chars/2`, see http://www.swi-prolog.org/pldoc/doc_for?object=string_chars/2
 */
object StringCharsRule : Rule(StringCharsPredicate, PredicateQuery(StringCharsPredicate)) {
    override fun fulfill(predicate: Predicate, kb: KnowledgeBase, randomVariableScope: RandomVariableScope): LazySequence<Unification> {
        if (predicate.arity != head.arity || predicate.name != head.name) return Unification.NONE

        val inputA = predicate.arguments[0]
        val inputB = predicate.arguments[1]

        fun convertInputAToListOfCharacters(): PrologList {
            if (inputA !is PrologString) throw PrologRuntimeException("Type Error: expected string as first argument to string_chars/2, got ${inputA.prologTypeName}")
            return PrologList(inputA.characters.map { Atom(it.toString()) })
        }

        fun convertInputBToPrologString(): PrologString {
            // single-character atoms to string
            if (inputB !is PrologList) throw PrologRuntimeException("Type Error: expected list as second argument to string_chars/2, got ${inputB.prologTypeName}")
            if (inputB.tail != null) throw PrologRuntimeException("Type Error: expected list as second argument to string_chars/2, got compound")

            val stringCharsTarget = CharArray(inputB.elements.size)
            inputB.elements.forEachIndexed { index, listElement ->
                if (listElement is Atom && listElement.name.length == 1) {
                    stringCharsTarget[index] = listElement.name[0]
                }
                else throw PrologRuntimeException("Type Error: expected character, found ${listElement.prologTypeName}")
            }

            return PrologString(stringCharsTarget)
        }

        if (inputA is Variable && inputB is Variable) {
            throw PrologRuntimeException("Arguments are not sufficiently instantiated")
        }

        if (inputA is PrologString) {
            val referenceValueForB = convertInputAToListOfCharacters()
            val unificationResult = referenceValueForB.unify(inputB)
            return if (unificationResult == null) Unification.NONE else LazySequence.of(unificationResult)
        }

        if (inputB is PrologList) {
            val referenceValueForA = convertInputBToPrologString()
            val unificationResult = referenceValueForA.unify(inputA)
            return if (unificationResult == null) Unification.NONE else LazySequence.of(unificationResult)
        }

        return Unification.NONE
    }
}

private object StringCodesPredicate : BuiltinPredicate("string_codes", A, B)

/**
 * Implements the `string_codes/2` builtin, see http://www.swi-prolog.org/pldoc/doc_for?object=string_codes/2
 *
object StringCodesRule : Rule(StringCodesPredicate, PredicateQuery(StringCodesPredicate)) {
    override fun fulfill(predicate: Predicate, kb: KnowledgeBase, randomVariableScope: RandomVariableScope): LazySequence<Unification> {

    }
}*/