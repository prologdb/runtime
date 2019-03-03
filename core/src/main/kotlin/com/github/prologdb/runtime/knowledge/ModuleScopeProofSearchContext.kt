package com.github.prologdb.runtime.knowledge

import com.github.prologdb.async.LazySequenceBuilder
import com.github.prologdb.runtime.knowledge.library.ClauseIndicator
import com.github.prologdb.runtime.knowledge.library.Module
import com.github.prologdb.runtime.term.CompoundTerm
import com.github.prologdb.runtime.unification.Unification

/**
 * All code declared inside a module only has access to predicates declared in the same module and
 * imported into that module explicitly **but not** to predicates visible in the scope where the
 * module is being imported into. This [ProofSearchContext] ensures that isolation: running code of
 * module within the proper [ModuleScopeProofSearchContext] achieves that behaviour.
 */
class ModuleScopeProofSearchContext(
    /**
     * The [ProofSearchContext] that invoked a predicate from another module which then causes the
     * necessity for this [ProofSearchContext] to exists.
     */
    private val invokedFrom: ProofSearchContext,

    private val module: Module,

    /**
     * Predicates private to the module
     */
    private val privatePredicates: Map<ClauseIndicator, PrologCallable>
) : ProofSearchContext, AbstractProofSearchContext() {
    override val principal = invokedFrom.principal
    override val randomVariableScope = invokedFrom.randomVariableScope
    override val authorization = invokedFrom.authorization

    override suspend fun LazySequenceBuilder<Unification>.doInvokePredicate(goal: CompoundTerm, indicator: ClauseIndicator) {
        privatePredicates[indicator]?.let {
            it.fulfill(this, goal, this@ModuleScopeProofSearchContext)
            return
        }

        module.findImport(indicator)?.let {
            it.fulfill(this, goal, this@ModuleScopeProofSearchContext)
            return
        }
    }
}