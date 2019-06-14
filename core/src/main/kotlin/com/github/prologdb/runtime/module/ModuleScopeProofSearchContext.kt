package com.github.prologdb.runtime.module

import com.github.prologdb.async.LazySequenceBuilder
import com.github.prologdb.async.Principal
import com.github.prologdb.runtime.*
import com.github.prologdb.runtime.proofsearch.*
import com.github.prologdb.runtime.query.PredicateInvocationQuery
import com.github.prologdb.runtime.term.Atom
import com.github.prologdb.runtime.term.CompoundTerm
import com.github.prologdb.runtime.term.PrologInteger
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


    override suspend fun LazySequenceBuilder<Unification>.doInvokePredicate(query: PredicateInvocationQuery, variables: VariableBucket) {
        val goal = query.goal

        // attempt core builtin
        when (goal.functor) {
            "assert", "assertz" -> {
                assertz(goal.arguments)
                yield(Unification.TRUE)
                return
            }
            "abolish" -> {
                abolish(goal.arguments)
                yield(Unification.TRUE)
                return
            }
            "retract", "retractAll" -> {
                throw PrologRuntimeException("${ClauseIndicator.of(goal)} is not fully implemented yet.")
            }
        }

        val (fqIndicator, callable) = resolveCallable(ClauseIndicator.of(goal)) ?: return

        if (!authorization.mayRead(fqIndicator)) throw PrologPermissionError("Not allowed to read $fqIndicator")

        callable.fulfill(this, goal.arguments, this@ModuleScopeProofSearchContext)
        return
    }

    override fun getStackTraceElementOf(query: PredicateInvocationQuery) = PrologStackTraceElement(
        query.goal,
        if (query.goal is HasPrologSource) query.goal.sourceInformation else query.sourceInformation,
        module
    )

    private val importLookupCache: Map<ClauseIndicator, Pair<ModuleReference, PrologCallable>> = mutableMapOf<ClauseIndicator, Pair<ModuleReference, PrologCallable>>().also { importLookupCache ->
        module.imports.asSequence()
            .forEach { import ->
                val referencedModule = rootAvailableModules[import.moduleReference.moduleName]
                    ?: throw PrologRuntimeException("Imported module ${import.moduleReference} not loaded in proof search context")

                val visiblePredicates: Map<ClauseIndicator, Pair<ModuleReference, PrologCallable>> = when (import) {
                    is FullModuleImport -> referencedModule.exportedPredicates
                        .mapValues { (_, callable) ->
                            Pair(import.moduleReference, callable)
                        }
                    is SelectiveModuleImport -> import.imports
                        .map { (exportedIndicator, alias) ->
                            val callable = referencedModule.exportedPredicates[exportedIndicator]
                                ?: throw PrologRuntimeException("Predicate $exportedIndicator not exported by module ${import.moduleReference}")
                            if (exportedIndicator.functor == alias) {
                                exportedIndicator to Pair(import.moduleReference, callable)
                            } else {
                                ClauseIndicator.of(alias, exportedIndicator.arity) to Pair(import.moduleReference, callable)
                            }
                        }
                        .toMap()
                    is ExceptModuleImport -> referencedModule.exportedPredicates
                        .filterKeys { it !in import.excluded }
                        .mapValues { (_, callable) ->
                            Pair(import.moduleReference, callable)
                        }
                }

                importLookupCache.putAll(visiblePredicates)
            }
    }

    private fun findImport(indicator: ClauseIndicator): Pair<ModuleReference, PrologCallable>? = importLookupCache[indicator]

    private fun assertz(args: Array<out Term>) {
        if (args.size != 1) throw PrologRuntimeException("assertz/${args.size} is not defined")

        val clause = args[0] as? Clause ?: throw PrologRuntimeException("Argument 0 to assertz/1 must be a clause")

        val simpleIndicator = ClauseIndicator.of(clause)

        val (fqIndicator, callable) = resolveCallable(simpleIndicator)
            ?: throw PrologRuntimeException("Predicate $simpleIndicator is not known")

        if (callable is DynamicPrologPredicate) {
            callable.assertz(clause)
        } else {
            throw PredicateNotDynamicException(fqIndicator)
        }
    }

    private fun abolish(args: Array<out Term>) {
        if (args.size != 1) throw PrologRuntimeException("abolish/${args.size} is not defined")
        val arg0 = args[0]
        if (arg0 !is CompoundTerm || arg0.arity != 2 || arg0.functor != "/") throw PrologRuntimeException("Argument 0 to abolish/1 must be an instance of `/`/2")
        if (arg0.arguments[0] !is Atom || arg0.arguments[1] !is PrologInteger) throw PrologRuntimeException("Argument 0 to abolish/1 must be an indicator")

        val name = (arg0.arguments[0] as Atom).name
        val arity = (arg0.arguments[1] as PrologInteger).value.toInt()

        val (fqIndicator, callable) = resolveCallable(ClauseIndicator.of(name, arity)) ?: return

        if (!authorization.mayWrite(fqIndicator)) {
            throw PrologPermissionError("Not allowed to write $fqIndicator")
        }

        throw PrologRuntimeException("abolish/1 is not fully implemented yet.")
    }

    override fun resolveCallable(simpleIndicator: ClauseIndicator): Pair<FullyQualifiedClauseIndicator, PrologCallable>? {
        // attempt modules own scope
        modulePredicates[simpleIndicator]?.let { callable ->
            val fqIndicator = FullyQualifiedClauseIndicator(module.name, simpleIndicator)
            return Pair(fqIndicator, callable)
        }

        // attempt imported predicate
        findImport(simpleIndicator)?.let { (sourceModule, callable) ->
            val fqIndicator = FullyQualifiedClauseIndicator(sourceModule.moduleName, ClauseIndicator.of(callable))

            return Pair(fqIndicator, callable)
        }

        return null
    }
}
