package com.github.prologdb.runtime.stdlib

import com.github.prologdb.parser.lexer.Operator
import com.github.prologdb.runtime.ArgumentError
import com.github.prologdb.runtime.ArgumentTypeError
import com.github.prologdb.runtime.ClauseIndicator
import com.github.prologdb.runtime.FullyQualifiedClauseIndicator
import com.github.prologdb.runtime.builtin.getInvocationStackFrame
import com.github.prologdb.runtime.builtin.prologSourceInformation
import com.github.prologdb.runtime.proofsearch.CurryingCallable
import com.github.prologdb.runtime.proofsearch.PrologCallable
import com.github.prologdb.runtime.proofsearch.ProofSearchContext
import com.github.prologdb.runtime.query.AndQuery
import com.github.prologdb.runtime.query.OrQuery
import com.github.prologdb.runtime.query.PredicateInvocationQuery
import com.github.prologdb.runtime.query.Query
import com.github.prologdb.runtime.term.Atom
import com.github.prologdb.runtime.term.CompoundTerm
import com.github.prologdb.runtime.term.PrologList
import com.github.prologdb.runtime.term.PrologNumber
import com.github.prologdb.runtime.term.Term
import com.github.prologdb.runtime.term.Variable
import com.github.prologdb.runtime.term.asIntegerInRange

class TypedPredicateArguments(val indicator: ClauseIndicator, val raw: Array<out Term>) {
    val size = raw.size
    operator fun get(index: Int): Term = raw[index]

    fun <T : Term> getTyped(index: Int, type: Class<T>): T {
        val untyped = this[index]
        if (type.isInstance(untyped)) {
            @Suppress("unchecked_cast")
            return untyped as T
        }

        throw ArgumentTypeError(indicator, index, untyped, type)
    }

    inline fun <reified T : Term> getTyped(index: Int) = getTyped(index, T::class.java)

    fun getInteger(index: Int): PrologNumber {
        val term = getTyped<PrologNumber>(index)
        if (!term.isInteger) {
            throw ArgumentError(index, "must be an integer")
        }

        return term
    }

    fun getIntegerInRange(index: Int, range: LongRange): Long {
        val integer = getInteger(index)
        return integer.asIntegerInRange(range)
            ?: throw ArgumentError(index, "Must be an integer in range [${range.min()}; ${range.max()}], got $integer")
    }

    fun <T : Term> getTypedOrUnbound(index: Int, type: Class<T>): Term {
        val term = this[index]
        if (term is Variable) {
            return term
        }
        if (type.isInstance(term)) {
            return term
        }

        throw ArgumentTypeError(indicator, index, term, type, Variable::class.java)
    }

    inline fun <reified T: Term> getTypedOrUnbound(index: Int): Term = getTypedOrUnbound(index, T::class.java)

    fun getQuery(index: Int): Query {
        return compoundToQuery(getTyped(index), index)
    }

    /**
     * Parses a goal of the form `Var1^Var2^Goal`, where there can be an arbitrary number
     * of variables (including none) prepended by instances of `^/2`. If the first argument to a `^/2` instance
     * is not a [Variable], that term is ignored.
     */
    fun getQueryWithExistentialVariables(index: Int): Pair<Query, Set<Variable>> {
        val variables = mutableSetOf<Variable>()
        var pivot = get(index)
        while (pivot is CompoundTerm && pivot.functor == "^" && pivot.arity == 2) {
            val variable = pivot.arguments[0]
            if (variable is Variable) {
                variables.add(variable)
            }

            pivot = pivot.arguments[1]
        }

        if (pivot !is CompoundTerm) {
            throw ArgumentTypeError(index, pivot, CompoundTerm::class.java)
        }

        return Pair(compoundToQuery(pivot, index), variables)
    }

    /**
     * If the [index]th argument is a [PrologList], assures that it doesn't have a tail and
     * returns its [PrologList.elements]. Otherwise, returns the argument term.
     */
    fun getListWithoutTailOrSingle(index: Int): List<Term> {
        val parameter = get(index)
        if (parameter !is PrologList) {
            return listOf(parameter)
        }

        if (parameter.tail != null) {
            throw ArgumentError(index, "The list cannot have a tail")
        }

        return parameter.elements
    }

    /**
     * A callable can be references in three ways:
     * * a simple atom: refers to the `<name>/<arity>` predicate in the scope of [ctxt]
     * * an instance of `:/2` with the first argument being an atom: applies the logic of the other two options, but
     *   within the scope of the specified module
     * * any other compound: refers to the `<name>/<n arguments + arity>` predicate in the scope of [ctxt],
     *   where the actual invocations are curried with the arguments given in here.
     * @return the callable referenced at the specified index
     */
    fun getCallable(index: Int, arity: Int, ctxt: ProofSearchContext): PrologCallable {
        val termAtIndex = this[index]

        val resolveInContext: ProofSearchContext
        val termToResolve: Term

        if (termAtIndex is CompoundTerm && termAtIndex.functor == ":" && termAtIndex.arity == 2 && termAtIndex.arguments[0] is Atom) {
            val module = (termAtIndex.arguments[0] as Atom).name
            resolveInContext = ctxt.deriveForModuleContext(module)
            termToResolve = termAtIndex.arguments[1]
        } else {
            resolveInContext = ctxt
            termToResolve = termAtIndex
        }

        return when (termToResolve) {
            is Atom -> {
                val (_, callable) = resolveInContext.resolveCallable(ClauseIndicator.of(termToResolve.name, arity))
                callable
            }
            is CompoundTerm -> {
                val (_, callable) = resolveInContext.resolveCallable(ClauseIndicator.of(termToResolve.functor, termToResolve.arity + arity))
                CurryingCallable(callable, termToResolve.arguments)
            }
            else -> throw ArgumentError(index, "Cannot resolve $termAtIndex to a callable. Specify either an atom or a compound, optionally module-scoped with ':'/2")
        }
    }

    /**
     * If the term at index [index] is a qualified indicator (of the form `module:functor/arity`), returns
     * that data as it is. If its an unqualified indicator, attempts to resolve the contextual reference through
     * [ctxt] and returns the true indicator of the referred callable.
     *
     * Unlike [getCallable], this method does not guarantee that the returned predicate exists.
     */
    fun getQualifiedIndicator(index: Int, ctxt: ProofSearchContext): FullyQualifiedClauseIndicator {
        val termAtIndex = this[index]
        val expected = "an indicator (as an instance of '/'/2, optionally qualified with ':'/2)"

        if (termAtIndex !is CompoundTerm) {
            throw ArgumentTypeError(index, termAtIndex, CompoundTerm::class.java)
        }

        val givenModuleName: String?
        val simpleIndicator: Term

        when (termAtIndex.functor) {
            ":" -> {
                if (termAtIndex.arity != 2) {
                    throw ArgumentError(index, "must be $expected, got ${ClauseIndicator.of(termAtIndex)}")
                }
                val moduleNameTerm = termAtIndex.arguments[0]
                simpleIndicator = termAtIndex.arguments[1]

                if (moduleNameTerm !is Atom) {
                    throw ArgumentError(
                        index,
                        "must be $expected, but the module name is a ${moduleNameTerm.prologTypeName}"
                    )
                }

                givenModuleName = moduleNameTerm.name
            }
            "/" -> {
                givenModuleName = null
                simpleIndicator = termAtIndex
            }
            else -> {
                throw ArgumentError(index, "must be $expected, got ${ClauseIndicator.of(termAtIndex)}")
            }
        }

        if (simpleIndicator !is CompoundTerm) {
            throw ArgumentError(index, "must be $expected, but got ${simpleIndicator.prologTypeName} instead of '/'/2")
        }
        if (simpleIndicator.functor != "/" || simpleIndicator.arity != 2) {
            throw ArgumentError(index, "must be $expected, but got ${ClauseIndicator.of(simpleIndicator)} instead of '/'/2")
        }

        val functorTerm = simpleIndicator.arguments[0]
        val arityTerm = simpleIndicator.arguments[1]
        if (functorTerm !is Atom || arityTerm !is PrologNumber || !arityTerm.isInteger || arityTerm < PrologNumber(0)) {
            throw ArgumentError(index, "must be $expected")
        }
        if (arityTerm > PrologNumber(Integer.MAX_VALUE)) {
            throw ArgumentError(index, "The arity cannot be greater than ${Int.MAX_VALUE}")
        }

        if (givenModuleName != null) {
            return FullyQualifiedClauseIndicator(givenModuleName, ClauseIndicator.of(functorTerm.name, arityTerm.toInteger().toInt()))
        }

        return ctxt.resolveCallable(ClauseIndicator.of(functorTerm.name, arityTerm.toInteger().toInt())).first
    }

    private fun compoundToQuery(compoundTerm: CompoundTerm, argumentIndex: Int): Query {
        val sourceInformation = compoundTerm.sourceInformation.orElse { getInvocationStackFrame().prologSourceInformation }

        if (compoundTerm.arity != 2) {
            return PredicateInvocationQuery(compoundTerm, sourceInformation)
        }

        if (compoundTerm.functor == Operator.COMMA.text || compoundTerm.functor == Operator.SEMICOLON.text) {
            val allArgumentsCompound = compoundTerm.arguments.all { it is CompoundTerm }
            if (!allArgumentsCompound) {
                return PredicateInvocationQuery(compoundTerm, sourceInformation)
            }

            val argumentsConverted = compoundTerm.arguments.map { compoundToQuery(it as CompoundTerm, argumentIndex) }.toTypedArray()
            return when (compoundTerm.functor) {
                Operator.COMMA.text -> AndQuery(argumentsConverted)
                Operator.SEMICOLON.text -> OrQuery(argumentsConverted)
                else -> throw ArgumentError(argumentIndex, "expected comma or semicolon, got compound term ${compoundTerm.functor}")
            }
        }
        // else:
        return PredicateInvocationQuery(compoundTerm, sourceInformation)
    }

    private val FQI_INDICATOR_TEMPLATE = CompoundTerm(":", arrayOf(
        Variable("Module"),
        CompoundTerm("/", arrayOf(
            Variable("Functor"),
            Variable("Arity")
        ))
    ))
}
