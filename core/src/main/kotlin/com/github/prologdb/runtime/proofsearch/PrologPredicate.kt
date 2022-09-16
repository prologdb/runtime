package com.github.prologdb.runtime.proofsearch

import com.github.prologdb.async.LazySequence
import com.github.prologdb.async.LazySequenceBuilder
import com.github.prologdb.async.buildLazySequence
import com.github.prologdb.async.flatMapRemaining
import com.github.prologdb.async.mapRemaining
import com.github.prologdb.async.mapRemainingNotNull
import com.github.prologdb.runtime.Clause
import com.github.prologdb.runtime.ClauseIndicator
import com.github.prologdb.runtime.FullyQualifiedClauseIndicator
import com.github.prologdb.runtime.PredicateNotDynamicException
import com.github.prologdb.runtime.PrologInternalError
import com.github.prologdb.runtime.RandomVariableScope
import com.github.prologdb.runtime.VariableMapping
import com.github.prologdb.runtime.module.Module
import com.github.prologdb.runtime.query.AndQuery
import com.github.prologdb.runtime.query.OrQuery
import com.github.prologdb.runtime.query.PredicateInvocationQuery
import com.github.prologdb.runtime.query.Query
import com.github.prologdb.runtime.term.CompoundTerm
import com.github.prologdb.runtime.term.unify
import com.github.prologdb.runtime.term.variables
import com.github.prologdb.runtime.unification.MutableUnification
import com.github.prologdb.runtime.unification.Unification
import com.github.prologdb.runtime.unification.VariableDiscrepancyException
import java.util.concurrent.CopyOnWriteArrayList

/**
 * A predicate combines all clauses of a certain functor and arity. Calling the predicate
 * attempts to call every clause.
 */
interface PrologPredicate : PrologCallable {
    /**
     * Provides AST for the `listing` and `listing/1` predicates.
     */
    val clauses: List<Clause>

    val fqIndicator: FullyQualifiedClauseIndicator
}

interface DynamicPrologPredicate : PrologPredicate {
    /**
     * Inserts the given clause at the end of the existing clause-list to be evaluated in
     * [fulfill]. See prolog `assertz/1`.
     *
     * @throws PredicateNotDynamicException if this predicate [isSealed]
     */
    @Throws(PredicateNotDynamicException::class)
    fun assertz(clause: Clause)

    /**
     * Retracts facts and rules matching the given [CompoundTerm].
     */
    val retract: suspend LazySequenceBuilder<Unification>.(CompoundTerm, ProofSearchContext) -> Unification?

    /**
     * Prevents any further modification of this predicate through [assertz] or [retract].
     */
    fun seal()

    /**
     * If this predicate is sealed (see [seal])
     */
    val isSealed: Boolean
}

/**
 * A predicate based on existing AST that is being interpreted with every call to [fulfill].
 */
class ASTPrologPredicate(
    val indicator: ClauseIndicator,
    private val declaringModule: Module,
    val isModuleTransparent: Boolean
) : DynamicPrologPredicate {
    private val _clauses: MutableList<Clause> = CopyOnWriteArrayList()
    override val clauses = _clauses

    override val functor = indicator.functor
    override val arity   = indicator.arity

    override val fqIndicator = FullyQualifiedClauseIndicator(declaringModule.declaration.moduleName, indicator)

    private var tailCallData: TailCallData? = null
    private var tailCallInitialized: Boolean = false

    /**
     * Set to true by [seal] (e.g. by `compile_predicates/1`).
     */
    @Volatile
    override var isSealed = false
        private set

    override fun seal() {
        isSealed = true
    }

    override val fulfill: PrologCallableFulfill = fulfill@{ initialArguments, invocationCtxt ->
        if (clauses.isEmpty()) {
            return@fulfill Unification.FALSE
        }

        if (!tailCallInitialized) updateTailCall()

        val ctxt = if (isModuleTransparent) invocationCtxt else {
            invocationCtxt.deriveForModuleContext(declaringModule.declaration.moduleName)
        }

        val lastClause = clauses.last()
        var arguments = initialArguments
        val stateCarry = MutableUnification.createTrue()
        tailCallLoop@ while (true) {
            clauses@ for (clause in clauses) {
                if (clause is CompoundTerm) {
                    val randomizedClauseArgs = ctxt.randomVariableScope.withRandomVariables(
                        clause.arguments,
                        VariableMapping()
                    )
                    val unification = arguments.unify(randomizedClauseArgs, ctxt.randomVariableScope)
                    try {
                        unification?.incorporate(stateCarry, RandomVariableScope())
                    }
                    catch (ex: VariableDiscrepancyException) {
                        throw PrologInternalError("This should be unreachable. The variables should have been included in the arguments unified with the fact as necessary; the unification shouldn't have succeeded in this case - yet it has happened.", ex)
                    }
                    unification?.retainAll(initialArguments.variables)
                    if (lastClause === clause) return@fulfill unification else {
                        unification?.let { yield(it) }
                    }
                } else if (clause is Rule) {
                    if (lastClause !== clause) {
                        clause.fulfill(this, arguments, ctxt)?.let { unification ->
                            try {
                                yield(unification.combinedWith(stateCarry, ctxt.randomVariableScope))
                            }
                            catch (ex: VariableDiscrepancyException) {
                                throw PrologInternalError("This should be unreachable. The variables should have been included in the arguments unified with the fact as necessary; the rule shouldn't have succeeded in this case - yet it has happened.", ex)
                            }
                        }
                    } else {
                        val localTailCallData = tailCallData
                            ?: return@fulfill clause.fulfill(this, arguments, ctxt)

                        localTailCallData.bodyPriorToTailCall?.fulfill?.invoke(this, arguments, ctxt)?.let { yield(it) }
                        val bodyForTailCall = localTailCallData.bodyForTailCall ?: Rule(clause.head, AndQuery(emptyArray()))
                        val lastClauseCallPreparation = bodyForTailCall.prepareCall(arguments, ctxt)
                            ?: return@fulfill null

                        val lastClausePartialResults = buildLazySequence<Unification>(principal) {
                            return@buildLazySequence bodyForTailCall.fulfillPreparedCall(
                                this@buildLazySequence,
                                lastClauseCallPreparation
                            )
                        }

                        val firstPartialResult = lastClausePartialResults.tryAdvance() ?: return@fulfill null
                        if (lastClausePartialResults.state == LazySequence.State.DEPLETED) {
                            val tailCallArguments = lastClauseCallPreparation.getTailCallArguments(firstPartialResult, localTailCallData.tailCallInvocation)
                            if (tailCallArguments != null) {
                                arguments = tailCallArguments
                                try {
                                    stateCarry.incorporate(
                                        lastClauseCallPreparation.untranslateResult(firstPartialResult),
                                        ctxt.randomVariableScope
                                    )
                                    continue@tailCallLoop
                                }
                                catch (ex: VariableDiscrepancyException) {
                                    throw PrologInternalError("This should be unreachable. The variables should have been included in the arguments unified with the fact as necessary; the unification shouldn't have succeeded in this case - yet it has happened.", ex)
                                }
                            }
                        }

                        val partialSolutions = buildLazySequence<Unification>(principal) {
                            yield(firstPartialResult)
                            yieldAllFinal(lastClausePartialResults)
                        }
                            .mapRemainingNotNull { result ->
                                try {
                                    result.combinedWith(stateCarry, ctxt.randomVariableScope)
                                }
                                catch (ex: VariableDiscrepancyException) {
                                    throw PrologInternalError("This should be unreachable. The variables should have been included in the arguments unified with the fact as necessary; the unification shouldn't have succeeded in this case - yet it has happened.", ex)
                                }
                            }

                        val fullSolutions = partialSolutions.flatMapRemaining<Unification, Unification> { partialResult ->
                            val tailCallConcrete = lastClauseCallPreparation.translateClausePart(localTailCallData.tailCallInvocation)
                                .substituteVariables(partialResult.asSubstitutionMapper())

                            return@flatMapRemaining yieldAllFinal(
                                buildLazySequence(ctxt.principal) {
                                    ctxt.fulfillAttach.invoke(
                                        this,
                                        PredicateInvocationQuery(tailCallConcrete, tailCallConcrete.sourceInformation),
                                        partialResult,
                                    )
                                }
                                    .mapRemaining {
                                        try {
                                            it.combinedWith(partialResult, ctxt.randomVariableScope)
                                        }
                                        catch (ex: VariableDiscrepancyException) {
                                            throw PrologInternalError("This should be unreachable. The variables should have been included in the arguments unified with the fact as necessary; the unification shouldn't have succeeded in this case - yet it has happened.", ex)
                                        }
                                    }
                            )
                        }
                        val untranslatedSolutions = fullSolutions.mapRemaining { lastClauseCallPreparation.untranslateResult(it) }
                        return@fulfill yieldAllFinal(untranslatedSolutions)
                    }
                } else {
                    throw PrologInternalError("Unsupported clause type ${clause.javaClass.name} in predicate $indicator")
                }
            }
        }

        @Suppress("UNREACHABLE_CODE") return@fulfill null
    }

    override fun assertz(clause: Clause) {
        if (ClauseIndicator.of(clause) != indicator) {
            throw PrologInternalError("Cannot add clause $clause to predicate $indicator: different indicator")
        }

        if (isSealed) {
            throw PredicateNotDynamicException(fqIndicator)
        }

        _clauses.add(clause)

        invalidateTailCall()
    }

    override val retract: suspend LazySequenceBuilder<Unification>.(CompoundTerm, ProofSearchContext) -> Unification? =
        retract@{ matching, ctxt ->
            if (isSealed) {
                throw PredicateNotDynamicException(fqIndicator)
            }

            while (true) {
                val clause = clauses.firstOrNull() ?: break
                val clauseId = clause as? CompoundTerm ?: (clause as Rule).head
                val randomClauseIdMapping = VariableMapping()
                val randomClauseId = ctxt.randomVariableScope.withRandomVariables(clauseId, randomClauseIdMapping)
                val unification = matching.unify(randomClauseId, ctxt.randomVariableScope)
                if (unification != null) {
                    clauses.remove(clause)
                    invalidateTailCall()
                    val result = unification.createMutableCopy()
                    result.retainAll(matching.variables)
                    yield(result)
                }
            }

            return@retract Unification.FALSE
    }

    /**
     * If the last goal in this query is a recursive call, returns a rule with that last goal omitted
     * and a separate reference onto that last, omitted goal.
     */
    private fun Rule.toTailCall(): TailCallData? {
        fun Query.toTailCall(): Triple<OrQuery?, AndQuery?, CompoundTerm>? {
            val priorGoals = mutableListOf<Query>()
            var self: Query = this
            while (self is OrQuery) {
                for (i in 0 until self.goals.lastIndex) {
                    priorGoals.add(self.goals[i])
                }
                self = self.goals.last()
            }

            val bodyPriorToTailCall: OrQuery? = priorGoals
                .takeIf { it.isNotEmpty() }
                ?.let { OrQuery(it.toTypedArray()) }

            return when (self) {
                is AndQuery -> {
                    val subResult = self.goals.last().toTailCall() ?: return null
                    val bodyForTailCall: AndQuery = if (subResult.second == null) {
                        AndQuery(Array(self.goals.size - 1) { self.goals[it] })
                    } else {
                        AndQuery(Array(self.goals.size) { index ->
                            if (index == 0) subResult.second!! else self.goals[index - 1]
                        })
                    }

                    Triple(
                        bodyPriorToTailCall,
                        bodyForTailCall,
                        subResult.third,
                    )
                }

                is PredicateInvocationQuery -> {
                    if (self.goal.arity == this@ASTPrologPredicate.arity && self.goal.functor == this@ASTPrologPredicate.functor) {
                        Triple(
                            bodyPriorToTailCall,
                            null,
                            self.goal,
                        )
                    } else {
                        null
                    }
                }

                is OrQuery -> error("IntelliJ realizes this branch is not reachable but Kotlin won't let me omit this branch.")
            }
        }

        val (queryBeforeTailCall, queryForTailCall, tailCallInvocation) = query.toTailCall() ?: return null
        return TailCallData(
            queryBeforeTailCall?.let { Rule(head, it) },
            queryForTailCall?.let { Rule(head, it) },
            tailCallInvocation,
        )
    }

    private fun updateTailCall() {
        tailCallData = (clauses.lastOrNull() as? Rule)?.toTailCall()
        tailCallInitialized = true
    }

    private fun invalidateTailCall() {
        tailCallInitialized = false
        tailCallData = null
    }

    private data class TailCallData(
        /**
         * Code to run before doing any TCO. This happens if the rule is a `;/2` at its root. In that case,
         * the first part of the disjunction has to be run without doing a tail-call afterwards.
         */
        val bodyPriorToTailCall: Rule?,

        /**
         * The part of the body after which a TCO can happen
         */
        val bodyForTailCall: Rule?,

        /**
         * The actual tail call, as it is declared after [bodyForTailCall], which can converted into a looping
         * call to save stack space iff [bodyForTailCall] behaves deterministically
         */
        val tailCallInvocation: CompoundTerm,
    )
}