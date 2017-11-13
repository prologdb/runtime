package com.github.tmarsteel.ktprolog.knowledge

import com.github.tmarsteel.ktprolog.RandomVariableScope
import com.github.tmarsteel.ktprolog.knowledge.library.Library
import com.github.tmarsteel.ktprolog.unification.Unification
import com.github.tmarsteel.ktprolog.query.Query
import com.github.tmarsteel.ktprolog.term.Predicate

interface KnowledgeBase {

    fun fulfill(predicate: Predicate, randomVarsScope: RandomVariableScope = RandomVariableScope()): Sequence<Unification>

    fun fulfill(query: Query, randomVarsScope: RandomVariableScope = RandomVariableScope()): Sequence<Unification> = query.findProofWithin(kb = this, randomVarsScope = randomVarsScope)

    companion object {
        val EMPTY = EmptyKnowledgeBase()
    }
}

interface MutableKnowledgeBase : KnowledgeBase {
    fun assert(predicate: Predicate)
    fun defineRule(rule: Rule)
    fun load(library: Library)
}

class EmptyKnowledgeBase : KnowledgeBase {
    override fun fulfill(predicate: Predicate, randomVarsScope: RandomVariableScope) = Unification.NONE
}

/**
 * Holds operators defined in a knowledge base. This might seem like a concern of the parser since, at the core level,
 * it does not make a difference whether `+(1,2)` was parsed from `1 + 2` or from `+(1,2)`. But operator definitions
 * are at the very heart of the language (e.g. =/2). It would be possible to define the core without the syntax sugar
 * but that would make it a real chore to go from core + parser to a working, compliant REPL. The core concerns itself
 * with operator definitions because it makes the library easier to use.
 */
interface OperatorRegistry {
    fun findPrefixOperators(name: String): Set<OperatorDefinition>
    fun defineOperator(definition: OperatorDefinition)
}

/**
 * Defines an operator to use in a prolog program.
 */
interface OperatorDefinition {
    /**
     * The name of the operator
     */
    val name: String

    /**
     * The precedence, between 0 and 1200.
     */
    val precedence: Short

    /**
     * The type of this operator ; defines how it relates towards its surroundings
     */
    val type: OperatorType
}

enum class OperatorType(val arity: Int) {
    FX(1),
    FY(1),
    XFX(2),
    XFY(2),
    YFX(2),
    XF(1),
    YF(1)
}