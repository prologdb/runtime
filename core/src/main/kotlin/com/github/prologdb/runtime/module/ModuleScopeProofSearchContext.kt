package com.github.prologdb.runtime.module

import com.github.prologdb.async.LazySequenceBuilder
import com.github.prologdb.async.Principal
import com.github.prologdb.runtime.Clause
import com.github.prologdb.runtime.ClauseIndicator
import com.github.prologdb.runtime.DefaultPrologRuntimeEnvironment
import com.github.prologdb.runtime.FullyQualifiedClauseIndicator
import com.github.prologdb.runtime.PredicateNotDefinedException
import com.github.prologdb.runtime.PredicateNotExportedException
import com.github.prologdb.runtime.PrologPermissionError
import com.github.prologdb.runtime.RandomVariableScope
import com.github.prologdb.runtime.exception.PrologStackTraceElement
import com.github.prologdb.runtime.proofsearch.AbstractProofSearchContext
import com.github.prologdb.runtime.proofsearch.Authorization
import com.github.prologdb.runtime.proofsearch.PrologCallable
import com.github.prologdb.runtime.proofsearch.ProofSearchContext
import com.github.prologdb.runtime.query.PredicateInvocationQuery
import com.github.prologdb.runtime.term.Atom
import com.github.prologdb.runtime.term.CompoundTerm
import com.github.prologdb.runtime.term.MathContext
import com.github.prologdb.runtime.term.Term
import com.github.prologdb.runtime.unification.Unification

/**
 * All code declared inside a module only has access to predicates declared in the same module and
 * imported into that module explicitly **but not** to predicates visible in the scope where the
 * module is being imported into. This [ProofSearchContext] ensures that isolation: running code of
 * module within the proper [ModuleScopeProofSearchContext] achieves that behaviour.
 */
class ModuleScopeProofSearchContext(
    override val module: Module,
    private val runtimeEnvironment: DefaultPrologRuntimeEnvironment,
    private val lookupTable: Map<ClauseIndicator, Pair<ModuleReference, PrologCallable>>,
    override val principal: Principal,
    override val randomVariableScope: RandomVariableScope,
    override val authorization: Authorization,
    override val mathContext: MathContext,
) : ProofSearchContext, AbstractProofSearchContext() {

    override val operators = module.localOperators

    override suspend fun LazySequenceBuilder<Unification>.doInvokePredicate(query: PredicateInvocationQuery, variables: Unification): Unification? {
        val (fqIndicator, callable, invocableGoal) = resolveHead(query.goal)
        if (!authorization.mayRead(fqIndicator)) throw PrologPermissionError("Not allowed to read/invoke $fqIndicator")
        return callable.fulfill(this, invocableGoal.arguments, this@ModuleScopeProofSearchContext)
    }

    override fun getStackTraceElementOf(query: PredicateInvocationQuery) = PrologStackTraceElement(
        query.goal,
        query.goal.sourceInformation.orElse(query.sourceInformation),
        module
    )

    private fun findImport(indicator: ClauseIndicator): Pair<ModuleReference, PrologCallable>? = lookupTable[indicator]

    override fun resolveModuleScopedCallable(goal: Clause): Triple<FullyQualifiedClauseIndicator, PrologCallable, Array<out Term>>? {
        if (goal.functor != ":" || goal.arity != 2 || goal !is CompoundTerm) {
            return null
        }

        val moduleNameTerm = goal.arguments[0]
        val unscopedGoal = goal.arguments[1]

        if (moduleNameTerm !is Atom || unscopedGoal !is CompoundTerm) {
            return null
        }

        val simpleIndicator = ClauseIndicator.of(unscopedGoal)

        if (moduleNameTerm.name == this.module.declaration.moduleName) {
            val callable = module.allDeclaredPredicates[simpleIndicator]
                ?: throw PredicateNotDefinedException(simpleIndicator, module)

            val fqIndicator = FullyQualifiedClauseIndicator(this.module.declaration.moduleName, simpleIndicator)
            return Triple(fqIndicator, callable, unscopedGoal.arguments)
        }

        val module = runtimeEnvironment.getLoadedModule(moduleNameTerm.name)

        val callable = module.exportedPredicates[simpleIndicator]
            ?: if (simpleIndicator in module.allDeclaredPredicates) {
                throw PredicateNotExportedException(FullyQualifiedClauseIndicator(module.declaration.moduleName, simpleIndicator), this.module)
            } else {
                throw PredicateNotDefinedException(simpleIndicator, module)
            }

        return Triple(
            FullyQualifiedClauseIndicator(module.declaration.moduleName, simpleIndicator),
            callable,
            unscopedGoal.arguments
        )
    }

    override fun resolveCallable(simpleIndicator: ClauseIndicator): Pair<FullyQualifiedClauseIndicator, PrologCallable> {
        module.allDeclaredPredicates[simpleIndicator]?.let { callable ->
            val fqIndicator = FullyQualifiedClauseIndicator(module.declaration.moduleName, simpleIndicator)
            return Pair(fqIndicator, callable)
        }

        findImport(simpleIndicator)?.let { (sourceModule, callable) ->
            val fqIndicator = FullyQualifiedClauseIndicator(sourceModule.moduleName, ClauseIndicator.of(callable))

            return Pair(fqIndicator, callable)
        }

        throw PredicateNotDefinedException(simpleIndicator, module)
    }

    override fun deriveForModuleContext(moduleName: String, restrictAuthorization: Authorization): ProofSearchContext {
        return runtimeEnvironment.deriveProofSearchContextForModule(this, moduleName, restrictAuthorization)
    }

    override fun toString() = "context of module ${module.declaration.moduleName}"
}
