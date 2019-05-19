package com.github.prologdb.runtime.module

import com.github.prologdb.async.LazySequenceBuilder
import com.github.prologdb.async.Principal
import com.github.prologdb.runtime.*
import com.github.prologdb.runtime.proofsearch.AbstractProofSearchContext
import com.github.prologdb.runtime.proofsearch.Authorization
import com.github.prologdb.runtime.proofsearch.PrologCallable
import com.github.prologdb.runtime.proofsearch.ProofSearchContext
import com.github.prologdb.runtime.term.Atom
import com.github.prologdb.runtime.term.CompoundTerm
import com.github.prologdb.runtime.term.PrologInteger
import com.github.prologdb.runtime.term.Term
import com.github.prologdb.runtime.unification.Unification

/**
 * All code declared inside a module only has access to predicates declared in the same module and
 * imported into that module explicitly **but not** to predicates visible in the scope where the
 * module is being imported into. This [ProofSearchContext] ensures that isolation: running code of
 * module within the proper [ModuleScopeProofSearchContext] achieves that behaviour.
 */
class ModuleScopeProofSearchContext(
    internal val module: Module,
    /**
     * Predicates declared in [module], including private ones.
     */
    private val modulePredicates: Map<ClauseIndicator, PrologCallable>,

    override val principal: Principal,
    override val randomVariableScope: RandomVariableScope,
    override val authorization: Authorization,
    override val rootAvailableModules: Map<String, Module>
) : ProofSearchContext, AbstractProofSearchContext() {

    override suspend fun LazySequenceBuilder<Unification>.doInvokePredicate(goal: CompoundTerm, indicator: ClauseIndicator) {
        // attempt core builtin
        when (goal.functor) {
            "assert", "assertz" -> {
                assertz(goal.arguments)
                return
            }
            "abolish" -> {
                abolish(goal.arguments)
                return
            }
            "retract", "retractAll" -> {
                throw PrologRuntimeException("${ClauseIndicator.of(goal)} is not fully implemented yet.")
            }
        }

        // attempt modules own scope
        modulePredicates[indicator]?.let {
            it.fulfill(this, goal.arguments, this@ModuleScopeProofSearchContext)
            return
        }

        // attempt imported predicate
        findImport(indicator)?.let {
            it.fulfill(this, goal.arguments, this@ModuleScopeProofSearchContext)
            return
        }
    }

    override fun getStackTraceElementOf(goal: CompoundTerm) = PrologStackTraceElement(
        goal,
        if (goal is HasPrologSource) goal.sourceInformation else NullSourceInformation,
        module
    )

    private val importLookupCache: Map<ClauseIndicator, PrologCallable> = mutableMapOf<ClauseIndicator, PrologCallable>().also { importLookupCache ->
        module.imports.asSequence()
            .forEach { import ->
                val referencedModule = rootAvailableModules[import.moduleReference.moduleName]
                    ?: throw PrologRuntimeException("Imported module ${import.moduleReference} not loaded in proof search context")

                val visiblePredicates: Map<ClauseIndicator, PrologCallable> = when (import) {
                    is FullModuleImport -> referencedModule.exportedPredicates
                    is SelectiveModuleImport -> import.imports
                        .map { (exportedIndicator, alias) ->
                            val callable = referencedModule.exportedPredicates[exportedIndicator]
                                ?: throw PrologRuntimeException("Predicate $exportedIndicator not exported by module ${import.moduleReference}")
                            if (exportedIndicator.functor == alias) {
                                exportedIndicator to callable
                            } else {
                                ClauseIndicator.of(alias, exportedIndicator.arity) to callable
                            }
                        }
                        .toMap()
                    is ExceptModuleImport -> referencedModule.exportedPredicates
                        .filterKeys { it !in import.excluded }
                }

                importLookupCache.putAll(visiblePredicates)
            }
    }

    private fun findImport(indicator: ClauseIndicator): PrologCallable? = importLookupCache[indicator]

    private fun assertz(args: Array<out Term>) {
        if (args.size != 1) throw PrologRuntimeException("assertz/${args.size} is not defined")

        val clause = args[0] as? Clause ?: throw PrologRuntimeException("Argument 0 to assertz/1 must be a clause")

        val indicator = ClauseIndicator.of(clause)
        if (!authorization.mayWrite(indicator)) throw PrologPermissionError("Not allowed to assert $indicator")

        throw PrologRuntimeException("assertz/1 is not fully implemented yet.")
    }

    private fun abolish(args: Array<out Term>) {
        if (args.size != 1) throw PrologRuntimeException("abolish/${args.size} is not defined")
        val arg0 = args[0]
        if (arg0 !is CompoundTerm || arg0.arity != 2 || arg0.functor != "/") throw PrologRuntimeException("Argument 0 to abolish/1 must be an instance of `/`/2")
        if (arg0.arguments[0] !is Atom || arg0.arguments[1] !is PrologInteger) throw PrologRuntimeException("Argument 0 to abolish/1 must be an indicator")

        val name = (arg0.arguments[0] as Atom).name
        val arity = (arg0.arguments[1] as PrologInteger).value.toInt()

        val indicator = ClauseIndicator.of(name, arity)
        if (!authorization.mayWrite(indicator)) throw PrologPermissionError("Not allowed to write $indicator")

        throw PrologRuntimeException("abolish/1 is not fully implemented yet.")
    }
}
