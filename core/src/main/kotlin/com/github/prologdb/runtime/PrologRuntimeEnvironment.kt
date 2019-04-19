package com.github.prologdb.runtime

import com.github.prologdb.async.*
import com.github.prologdb.runtime.builtin.ISOOpsOperatorRegistry
import com.github.prologdb.runtime.knowledge.*
import com.github.prologdb.runtime.knowledge.library.*
import com.github.prologdb.runtime.term.*
import com.github.prologdb.runtime.unification.Unification
import java.util.*
import java.util.concurrent.ConcurrentHashMap

/**
 * The environment for one **instance** of a prolog program.
 */
class PrologRuntimeEnvironment(
    initialOperators: OperatorRegistry = ISOOpsOperatorRegistry,
    private val moduleLoader: ModuleLoader
) {
    private val predicateIndex: ArityMap<MutableMap<String, PrologPredicate>> = ArityMap()

    private val loadedModules: MutableMap<ModuleReference, Module> = ConcurrentHashMap()

    private val _operators = DefaultOperatorRegistry()
    val operators: OperatorRegistry = _operators
    init {
        _operators.include(initialOperators)
    }

    fun invokeDirective(name: String, authorization: Authorization, arguments: Array<out Term>): LazySequence<Unification> {
        return buildLazySequence(IrrelevantPrincipal) {
            when (name) {
                "op" -> {
                    directiveOp3(*arguments)
                    yield(Unification.TRUE)
                }
                "use_module" -> {
                    directiveUseModule1(*arguments)
                    yield(Unification.TRUE)
                }
                else -> throw PrologRuntimeException("Directive $name/${arguments.size} is not defined.")
            }
        }
    }

    private fun getPredicate(indicator: ClauseIndicator): PrologPredicate? {
        return predicateIndex.computeIfAbsent(indicator.arity, ::ConcurrentHashMap)[indicator.functor]
    }

    /**
     * Implementation for the `assertz` builtin.
     */
    fun assertz(clause: Clause) {
        val indicator = ClauseIndicator.of(clause)
        val predicate = getPredicate(indicator)

        if (predicate is DynamicPrologPredicate) {
            predicate.assertz(clause)
        } else {
            throw PredicateNotDynamicException(indicator)
        }
    }

    /**
     * Implementation for the `abolish/1` builtin.
     */
    fun abolish(predicate: ClauseIndicator) {
        val functorToPredicate = predicateIndex[predicate.arity] ?: return

        functorToPredicate.remove(predicate.functor)
    }

    private fun directiveOp3(vararg arguments: Term) {
        if (arguments.size != 3) {
            throw PrologRuntimeException("Directive op/${arguments.size} is not defined")
        }

        val priorityArgument = arguments[0]
        if (priorityArgument !is PrologNumber || !priorityArgument.isInteger) {
            throw PrologRuntimeException("operator priority must be an integer")
        }

        val precedenceAsLong = priorityArgument.toInteger()
        if (precedenceAsLong < 0 || precedenceAsLong > 1200) {
            throw PrologRuntimeException("operator precedence must be in range [0; 1200]")
        }
        val precedence = precedenceAsLong.toShort()

        val associativityArgument = arguments[1]
        if (associativityArgument !is Atom) {
            throw PrologRuntimeException("Atom expected as operator associativity but found ${associativityArgument.prologTypeName}")
        }

        val typeAsUCString = (arguments[1] as Atom).name.toUpperCase()
        val operatorType = try {
            OperatorType.valueOf(typeAsUCString)
        } catch (ex: IllegalArgumentException) {
            throw PrologRuntimeException("${typeAsUCString.toLowerCase()} is not a known operator associativity. Known: ${OperatorType.values().joinToString(transform = { it.name.toLowerCase() })}")
        }

        if (arguments[2] !is Atom) {
            throw PrologRuntimeException("Atom expected as operator functor but got ${arguments[2].prologTypeName}")
        }

        _operators.defineOperator(OperatorDefinition(precedence, operatorType, (arguments[2] as Atom).name))
    }

    fun directiveUseModule1(vararg arguments: Term) {
        if (arguments.size != 1) {
            throw PrologRuntimeException("Directive use/${arguments.size} is not defined.")
        }

        val moduleRefTerm = arguments[0]
        if (moduleRefTerm !is CompoundTerm) {
            throw PrologRuntimeException("Argument 0 to use/1 must be a compound, got ${moduleRefTerm.prologTypeName}")
        }

        val moduleReference = ModuleReference.fromCompoundTerm(moduleRefTerm)
        loadedModules.computeIfAbsent(moduleReference, moduleLoader::load)
    }

    fun newProofSearchContext(): ProofSearchContext {
        return PSContext()
    }

    private inner class PSContext : ProofSearchContext, AbstractProofSearchContext() {
        override val principal: Principal = UUID.randomUUID()

        override val randomVariableScope = RandomVariableScope()

        override val authorization: Authorization = ReadWriteAuthorization

        override val rootAvailableModules: Map<ModuleReference, Module> = loadedModules

        override suspend fun LazySequenceBuilder<Unification>.doInvokePredicate(goal: CompoundTerm, indicator: ClauseIndicator) {
            if (goal.functor in coreBuiltins) {
                val builtin = coreBuiltins[goal.functor]!!

                prologTry(goal.toStackTraceElement()) {
                    builtin(this@PrologRuntimeEnvironment, authorization, goal.arguments)
                }

                return
            }

            getPredicate(indicator)?.fulfill?.invoke(this, goal, this@PSContext)
        }
    }
}

/**
 * The absolute CORE builtins that allow anything else to work. These are the assert and retract
 * builtins.
 */
private val coreBuiltins: MutableMap<String, suspend LazySequenceBuilder<Unification>.(PrologRuntimeEnvironment, Authorization, Array<out Term>) -> Unit> = run {
    val coreBuiltins = HashMap<String, suspend LazySequenceBuilder<Unification>.(PrologRuntimeEnvironment, Authorization, Array<out Term>) -> Unit>(8)

    coreBuiltins["assertz"] = { rt, auth, args ->
        if (args.size != 1) throw PrologRuntimeException("assertz/${args.size} is not defined")

        val clause = args[0] as? Clause ?: throw PrologRuntimeException("Argument 0 to assertz/1 must be a clause")

        val indicator = ClauseIndicator.of(clause)
        if (!auth.mayWrite(indicator)) throw PrologPermissionError("Not allowed to assert $indicator")

        rt.assertz(clause)
        yield(Unification.TRUE)
    }
    coreBuiltins["assert"] = coreBuiltins["assertz"]!!

    coreBuiltins["retract"] = { rt, auth, args: Array<out Term> ->
        TODO()
    }
    coreBuiltins["retractAll"] = { rt, auth, args: Array<out Term> ->
        TODO()
    }

    coreBuiltins["abolish"] = { rt, auth, args: Array<out Term> ->
        if (args.size != 1) throw PrologRuntimeException("abolish/${args.size} is not defined")
        val arg0 = args[0]
        if (arg0 !is CompoundTerm || arg0.arity != 2 || arg0.functor != "/") throw PrologRuntimeException("Argument 0 to abolish/1 must be an instance of `/`/2")
        if (arg0.arguments[0] !is Atom || arg0.arguments[1] !is PrologInteger) throw PrologRuntimeException("Argument 0 to abolish/1 must be an indicator")

        val name = (arg0.arguments[0] as Atom).name
        val arity = (arg0.arguments[1] as PrologInteger).value.toInt()

        val indicator = ClauseIndicator.of(name, arity)
        if (!auth.mayWrite(indicator)) throw PrologPermissionError("Not allowed to write $indicator")

        rt.abolish(indicator)

        yield(Unification.TRUE)
    }
    
    coreBuiltins
}
