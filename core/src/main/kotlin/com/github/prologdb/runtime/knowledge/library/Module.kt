package com.github.prologdb.runtime.knowledge.library

import com.github.prologdb.runtime.PrologRuntimeException
import com.github.prologdb.runtime.knowledge.ASTPrologPredicate
import com.github.prologdb.runtime.knowledge.ModuleScopeProofSearchContext
import com.github.prologdb.runtime.knowledge.PrologCallable
import com.github.prologdb.runtime.knowledge.ProofSearchContext
import com.github.prologdb.runtime.term.Atom
import com.github.prologdb.runtime.term.CompoundTerm

/**
 * A module, as results from reading/consulting a prolog file.
 */
interface Module {
    val name: String

    val exportedPredicates: Map<ClauseIndicator, PrologCallable>

    /**
     * The operators defined and exported by this module (using `:- op/3`).
     */
    val exportedOperators: OperatorRegistry

    val imports: List<ModuleImport>

    fun deriveScopedProofSearchContext(deriveFrom: ProofSearchContext): ProofSearchContext
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
)

class FullModuleImport(moduleReference: ModuleReference) : ModuleImport(moduleReference)
class PartialModuleImport(
    moduleReference: ModuleReference,

    /**
     * The imported predicates. The key may contain an alias, e.g.:
     * `use_module(foo, [bar/1 as baz])` will result in a key `baz/1` referring
     * to the `bar/1` predicate of module `foo`.
     */
    val imports: Set<ClauseIndicator>
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
    override val exportedOperators: OperatorRegistry
) : Module {
    private val allDeclaredPredicates: Map<ClauseIndicator, PrologCallable>
    init {
        allDeclaredPredicates = givenClauses.asSequence()
            .groupingBy { ClauseIndicator.of(it) }
            .fold(
                { indicator, _ -> ASTPrologPredicate(indicator, this) },
                { _, astPredicate, clause ->
                    astPredicate.assertz(clause)
                    astPredicate
                }
            )

        allDeclaredPredicates.values.forEach {
            if (it.indicator !in dynamicPredicates) {
                it.seal()
            }
        }
    }

    override val exportedPredicates: Map<ClauseIndicator, PrologCallable> = allDeclaredPredicates
        .filter { it.key in exportedPredicateIndicators }

    override fun deriveScopedProofSearchContext(deriveFrom: ProofSearchContext): ProofSearchContext {
        if (deriveFrom is ModuleScopeProofSearchContext) {
            return if (deriveFrom.module == this) deriveFrom else {
                ModuleScopeProofSearchContext(deriveFrom.invokedFrom, this, this.allDeclaredPredicates)
            }
        }

        return ModuleScopeProofSearchContext(deriveFrom, this, this.allDeclaredPredicates)
    }
}
