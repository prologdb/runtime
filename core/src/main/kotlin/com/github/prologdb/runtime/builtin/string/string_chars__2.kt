package com.github.prologdb.runtime.builtin.string

import com.github.prologdb.runtime.PrologRuntimeException
import com.github.prologdb.runtime.RandomVariableScope
import com.github.prologdb.runtime.builtin.A
import com.github.prologdb.runtime.builtin.B
import com.github.prologdb.runtime.builtin.BuiltinPredicate
import com.github.prologdb.runtime.knowledge.KnowledgeBase
import com.github.prologdb.runtime.knowledge.Rule
import com.github.prologdb.runtime.lazysequence.LazySequence
import com.github.prologdb.runtime.query.PredicateQuery
import com.github.prologdb.runtime.term.*
import com.github.prologdb.runtime.term.List
import com.github.prologdb.runtime.unification.Unification

private object StringCharsPredicate : BuiltinPredicate("string_chars", A, B)
/**
 * Implements `string_chars/2`, see http://www.swi-prolog.org/pldoc/doc_for?object=string_chars/2
 */
object StringCharsRule : Rule(StringCharsPredicate, PredicateQuery(StringCharsPredicate)) {
    override fun fulfill(predicate: Predicate, kb: KnowledgeBase, randomVariableScope: RandomVariableScope): LazySequence<Unification> {
        if (predicate.arity != head.arity || predicate.name != head.name) return Unification.NONE

        val inputA = predicate.arguments[0]
        val inputB = predicate.arguments[1]

        fun convertInputAToListOfCharacters(): List {
            if (inputA !is PrologString) throw PrologRuntimeException("Type Error: expected string as first argument to string_chars/2, got ${inputA.prologTypeName}")
            return List(inputA.characters.map { Atom(it.toString()) })
        }

        fun convertInputBToPrologString(): PrologString {
            // single-character atoms to string
            if (inputB !is List) throw PrologRuntimeException("Type Error: expected list as second argument to string_chars/2, got ${inputB.prologTypeName}")
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

        if (inputB is List) {
            val referenceValueForA = convertInputBToPrologString()
            val unificationResult = referenceValueForA.unify(inputA)
            return if (unificationResult == null) Unification.NONE else LazySequence.of(unificationResult)
        }

        return Unification.NONE
    }
}