package com.github.prologdb.runtime.module

import com.github.prologdb.async.Principal
import com.github.prologdb.runtime.*
import com.github.prologdb.runtime.builtin.ISOOpsOperatorRegistry
import com.github.prologdb.runtime.proofsearch.ASTPrologPredicate
import com.github.prologdb.runtime.proofsearch.Authorization
import com.github.prologdb.runtime.proofsearch.PrologCallable
import com.github.prologdb.runtime.proofsearch.ProofSearchContext
import com.github.prologdb.runtime.term.Atom
import com.github.prologdb.runtime.term.CompoundTerm
import com.github.prologdb.runtime.term.PrologList
import com.github.prologdb.runtime.term.Term
import com.github.prologdb.runtime.util.OperatorRegistry

/**
 * A module, as results from reading/consulting a prolog file.
 */
interface Module {
    val name: String

    val exportedPredicates: Map<ClauseIndicator, PrologCallable>

    val imports: List<ModuleImport>

    /**
     * Operators available in this module (including those given as context operators when parsing)
     */
    val localOperators: OperatorRegistry

    fun deriveScopedProofSearchContext(deriveFrom: ProofSearchContext): ProofSearchContext {
        return createProofSearchContext(
            deriveFrom.principal,
            deriveFrom.randomVariableScope,
            deriveFrom.authorization,
            deriveFrom.runtime
        )
    }

    fun createProofSearchContext(principal: Principal, randomVariableScope: RandomVariableScope,
                                 authorization: Authorization, runtime: PrologRuntimeEnvironment): ProofSearchContext
}

data class ModuleReference(
    /**
     * The alias for the path pointing to the module files parent directory,
     * e.g. `library` or `system`.
     */
    val pathAlias: String,

    val moduleName: String
) {
    companion object {
        /**
         * Takes a term of arity 1 where the first argument is an atom
         * @throws PrologRuntimeException If the given term is not a valid module reference.
         */
        @JvmStatic
        fun fromCompoundTerm(term: CompoundTerm): ModuleReference {
            if (term.arity != 1 || term.arguments[0] !is Atom) {
                throw PrologRuntimeException("Illegal module reference: must be of arity 1 and the sole argument must be an atom")
            }

            return ModuleReference(term.functor, (term.arguments[0] as Atom).name)
        }
    }

    override fun toString(): String {
        return "$pathAlias($moduleName)"
    }
}

sealed class ModuleImport(
    val moduleReference: ModuleReference
) {
    companion object {
        @JvmStatic
        fun fromUseModuleSyntax(useModuleArguments: Array<out Term>): ModuleImport {
            val moduleRefTerm = useModuleArguments[0]
            if (moduleRefTerm !is CompoundTerm) {
                throw PrologRuntimeException("Argument 0 to use_module/${useModuleArguments.size} must be a compound, got ${moduleRefTerm.prologTypeName}")
            }

            val moduleReference = ModuleReference.fromCompoundTerm(moduleRefTerm)

            if (useModuleArguments.size == 1) {
                return FullModuleImport(moduleReference)
            }

            val selectionTerm = useModuleArguments[1]

            if (selectionTerm is PrologList) {
                val imports = mutableMapOf<ClauseIndicator, String>()
                selectionTerm.elements.forEach { importTerm ->
                    if (importTerm !is CompoundTerm) {
                        throw PrologRuntimeException("References to single predicates in argument 1 to use_module/2 must be compounds, got ${importTerm.prologTypeName}")
                    }

                    if (importTerm.functor == "/") {
                        val indicator = ClauseIndicator.ofIdiomatic(importTerm, "argument 1 to use_module/2")
                        imports[indicator] = indicator.functor
                    } else if (importTerm.functor == "as") {
                        val indicator = ClauseIndicator.ofIdiomatic(importTerm.arguments[0], "argument 1 to use_module/2")
                        val aliasTerm = importTerm.arguments[1]
                        if (aliasTerm !is Atom) {
                            throw PrologRuntimeException("Predicate aliases in argument 1 to use_module/2 must be atoms, got ${aliasTerm.prologTypeName}")
                        }

                        imports[indicator] = aliasTerm.name
                    } else {
                        throw PrologRuntimeException("References to single predicates in argument 1 to use_module/2 must unify with either _/_ or _/_ as _")
                    }
                }

                return SelectiveModuleImport(moduleReference, imports)
            } else if (selectionTerm is CompoundTerm && selectionTerm.functor == "except" && selectionTerm.arity == 1) {
                val listTerm = selectionTerm.arguments[0]
                if (listTerm !is PrologList) {
                    throw PrologRuntimeException("Argument 0 to except/1 in argument 1 to use_module/2 must be a list, got ${listTerm.prologTypeName}")
                }

                val except = listTerm.elements
                    .map {
                        ClauseIndicator.ofIdiomatic(it, "Indicators in argument 0 to except/1 in argument 1 to use_module/2")
                    }
                    .toSet()

                return ExceptModuleImport(moduleReference, except)
            } else {
                throw PrologRuntimeException("argument 1 to use_module/2 must be either a list or an instance of except/1, got ${selectionTerm.prologTypeName}")
            }
        }


    }
}

class FullModuleImport(moduleReference: ModuleReference) : ModuleImport(moduleReference)
class SelectiveModuleImport(
    moduleReference: ModuleReference,

    /**
     * The imported predicates. The key refers to the indicator of the predicate as exported
     * by the imported module. The value is the functor to be used in the code that imports
     * the module. E.g. `:- use_module(foo, [bar/1 as baz])` will result in the key being
     * `bar/1` and the value being `baz`.
     */
    val imports: Map<ClauseIndicator, String>
) : ModuleImport(moduleReference)

class ExceptModuleImport(
    moduleReference: ModuleReference,
    val excluded: Set<ClauseIndicator>
) : ModuleImport(moduleReference)

/**
 * A [Module], constructed from AST. Predicates will be interpreted when
 * invoked; `:- compile/1` can exchange certain AST-based [PrologCallable]s
 * for compiled versions of themselves.
 */
class ASTModule(
    override val name: String,
    override val imports: List<ModuleImport>,
    val givenClauses: Iterable<Clause>,
    val dynamicPredicates: Set<ClauseIndicator>,
    val exportedPredicateIndicators: Set<ClauseIndicator>,
    override val localOperators: OperatorRegistry = ISOOpsOperatorRegistry
) : Module {
    private val allDeclaredPredicates: Map<ClauseIndicator, PrologCallable>
    init {
        val _allDeclaredPredicates = givenClauses.asSequence()
            .groupingBy { ClauseIndicator.of(it) }
            .foldTo(
                HashMap(),
                { indicator, _ -> ASTPrologPredicate(indicator, this) },
                { _, astPredicate, clause ->
                    astPredicate.assertz(clause)
                    astPredicate
                }
            )

        for (dynamicClauseIndicator in dynamicPredicates) {
            _allDeclaredPredicates.computeIfAbsent(dynamicClauseIndicator) { indicator -> ASTPrologPredicate(indicator, this) }
        }

        allDeclaredPredicates = _allDeclaredPredicates

        allDeclaredPredicates.values.forEach {
            if (it.indicator !in dynamicPredicates) {
                it.seal()
            }
        }
    }

    override val exportedPredicates: Map<ClauseIndicator, PrologCallable> = allDeclaredPredicates
        .filter { it.key in exportedPredicateIndicators }

    override fun deriveScopedProofSearchContext(deriveFrom: ProofSearchContext): ProofSearchContext {
        if (deriveFrom is ModuleScopeProofSearchContext && deriveFrom.module == this) {
            return deriveFrom
        }

        return super.deriveScopedProofSearchContext(deriveFrom)
    }

    override fun createProofSearchContext(principal: Principal, randomVariableScope: RandomVariableScope,
                                          authorization: Authorization, runtime: PrologRuntimeEnvironment): ProofSearchContext {
        return ModuleScopeProofSearchContext(
            this,
            this.allDeclaredPredicates,
            principal,
            randomVariableScope,
            authorization,
            runtime
        )
    }
}
