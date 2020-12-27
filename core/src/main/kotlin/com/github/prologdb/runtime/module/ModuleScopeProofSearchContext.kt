package com.github.prologdb.runtime.module

import com.github.prologdb.async.LazySequenceBuilder
import com.github.prologdb.async.Principal
import com.github.prologdb.runtime.Clause
import com.github.prologdb.runtime.ClauseIndicator
import com.github.prologdb.runtime.DefaultPrologRuntimeEnvironment
import com.github.prologdb.runtime.FullyQualifiedClauseIndicator
import com.github.prologdb.runtime.PrologPermissionError
import com.github.prologdb.runtime.PrologRuntimeException
import com.github.prologdb.runtime.RandomVariableScope
import com.github.prologdb.runtime.exception.PrologStackTraceElement
import com.github.prologdb.runtime.proofsearch.AbstractProofSearchContext
import com.github.prologdb.runtime.proofsearch.Authorization
import com.github.prologdb.runtime.proofsearch.PrologCallable
import com.github.prologdb.runtime.proofsearch.ProofSearchContext
import com.github.prologdb.runtime.query.PredicateInvocationQuery
import com.github.prologdb.runtime.term.Atom
import com.github.prologdb.runtime.term.CompoundTerm
import com.github.prologdb.runtime.term.Term
import com.github.prologdb.runtime.unification.Unification
import com.github.prologdb.runtime.unification.VariableBucket

/**
 * All code declared inside a module only has access to predicates declared in the same module and
 * imported into that module explicitly **but not** to predicates visible in the scope where the
 * module is being imported into. This [ProofSearchContext] ensures that isolation: running code of
 * module within the proper [ModuleScopeProofSearchContext] achieves that behaviour.
 */
class ModuleScopeProofSearchContext(
    val module: Module,
    private val runtimeEnvironment: DefaultPrologRuntimeEnvironment,
    private val lookupTable: Map<ClauseIndicator, Pair<ModuleReference, PrologCallable>>,
    override val principal: Principal,
    override val randomVariableScope: RandomVariableScope,
    override val authorization: Authorization
) : ProofSearchContext, AbstractProofSearchContext() {

    override val operators = module.localOperators

    override suspend fun LazySequenceBuilder<Unification>.doInvokePredicate(query: PredicateInvocationQuery, variables: VariableBucket): Unification? {
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

        if (moduleNameTerm.name == this.module.name) {
            val callable = module.allDeclaredPredicates[simpleIndicator]
                ?: throw PrologRuntimeException("Predicate $simpleIndicator not defined in context of module ${this.module.name}")

            val fqIndicator = FullyQualifiedClauseIndicator(this.module.name, simpleIndicator)
            return Triple(fqIndicator, callable, unscopedGoal.arguments)
        }

        val module = runtimeEnvironment.loadedModules[moduleNameTerm.name]
            ?: throw PrologRuntimeException("Module ${moduleNameTerm.name} not loaded")

        val callable = module.exportedPredicates[simpleIndicator]
            ?: if (simpleIndicator in module.allDeclaredPredicates) {
                throw PrologRuntimeException("Predicate $simpleIndicator not exported by module ${module.name}")
            } else {
                throw PrologRuntimeException("Predicate $simpleIndicator not defined in module ${module.name}")
            }

        return Triple(
            FullyQualifiedClauseIndicator(module.name, simpleIndicator),
            callable,
            unscopedGoal.arguments
        )
    }

    override fun resolveCallable(simpleIndicator: ClauseIndicator): Pair<FullyQualifiedClauseIndicator, PrologCallable>? {
        module.allDeclaredPredicates[simpleIndicator]?.let { callable ->
            val fqIndicator = FullyQualifiedClauseIndicator(module.name, simpleIndicator)
            return Pair(fqIndicator, callable)
        }

        findImport(simpleIndicator)?.let { (sourceModule, callable) ->
            val fqIndicator = FullyQualifiedClauseIndicator(sourceModule.moduleName, ClauseIndicator.of(callable))

            return Pair(fqIndicator, callable)
        }

        return null
    }

    override fun deriveForModuleContext(moduleName: String): ProofSearchContext {
        return runtimeEnvironment.deriveProofSearchContextForModule(this, moduleName)
    }

    override fun toString() = "context of module ${module.name}"
}
