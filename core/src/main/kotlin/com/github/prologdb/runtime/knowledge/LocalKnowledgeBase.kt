package com.github.prologdb.runtime.knowledge

import com.github.prologdb.async.*
import com.github.prologdb.runtime.*
import com.github.prologdb.runtime.builtin.ComparisonLibrary
import com.github.prologdb.runtime.builtin.EqualityLibrary
import com.github.prologdb.runtime.builtin.ISOOpsOperatorRegistry
import com.github.prologdb.runtime.builtin.dict.DictLibrary
import com.github.prologdb.runtime.builtin.dynamic.DynamicsLibrary
import com.github.prologdb.runtime.builtin.lists.ListsLibrary
import com.github.prologdb.runtime.builtin.math.MathLibrary
import com.github.prologdb.runtime.builtin.string.StringsLibrary
import com.github.prologdb.runtime.builtin.typesafety.TypeSafetyLibrary
import com.github.prologdb.runtime.knowledge.library.*
import com.github.prologdb.runtime.query.AndQuery
import com.github.prologdb.runtime.query.OrQuery
import com.github.prologdb.runtime.query.PredicateQuery
import com.github.prologdb.runtime.query.Query
import com.github.prologdb.runtime.term.*
import com.github.prologdb.runtime.unification.Unification
import com.github.prologdb.runtime.unification.VariableBucket
import java.util.*

/**
 * A knowledge base, the most important prerequisite for a default, prolog REPL system.
 */
class LocalKnowledgeBase(internal val store: MutableClauseStore = DoublyIndexedClauseStore()) : KnowledgeBase {
    private val _operators = DefaultOperatorRegistry()
    override val operators: OperatorRegistry = _operators

    override fun fulfill(query: Query, authorization: Authorization, randomVariableScope: RandomVariableScope): LazySequence<Unification> {
        return buildLazySequence(UUID.randomUUID()) {
            DefaultProofSearchContext(principal, authorization, randomVariableScope).fulfillAttach(this, query, VariableBucket())
        }
    }

    override fun invokeDirective(name: String, authorization: Authorization, arguments: Array<out Term>): LazySequence<Unification> {
        return buildLazySequence(IrrelevantPrincipal) {
            when (name) {
                "op" -> {
                    if (arguments.size != 3) {
                        throw PrologRuntimeException("Directive op/${arguments.size} is not defined")
                    }

                    val priorityArgument = arguments[0]
                    if (priorityArgument !is PrologNumber || !priorityArgument.isInteger) {
                        throw PrologRuntimeException("operator priority must be an integer")
                    }

                    val precedenceAsLong = priorityArgument.toInteger()
                    if (precedenceAsLong < 0 || precedenceAsLong > 1200) {
                        throw PrologRuntimeException("operator precedence must be between in range [0; 1200]")
                    }
                    val precedence = precedenceAsLong.toShort()

                    if (arguments[1] !is Atom) {
                        throw PrologRuntimeException("Atom expected as operator associativity but found ${arguments[1].prologTypeName}")
                    }

                    val typeAsUCString = (arguments[1] as Atom).name.toUpperCase()
                    val operatorType = try {
                        OperatorType.valueOf(typeAsUCString)
                    }
                    catch (ex: IllegalArgumentException) {
                        throw PrologRuntimeException("${typeAsUCString.toLowerCase()} is not a known operator type")
                    }

                    if (arguments[2] !is Atom) {
                        throw PrologRuntimeException("Atom expected as operator name but got ${arguments[2].prologTypeName}")
                    }

                    _operators.defineOperator(OperatorDefinition(precedence, operatorType, (arguments[2] as Atom).name))
                    yield(Unification.TRUE)
                }
                else -> throw PrologRuntimeException("Directive $name/${arguments.size} is not defined.")
            }
        }
    }

    /**
     * The libraries loaded into this knowledge base.
     */
    private val loadedLibraries = HashSet<Library>()

    /**
     * Every loaded library that has static exports is registered here for fast access.
     */
    private val staticIndex = ArityMap<MutableMap<String, Library>>()

    /**
     * All predicates that have been determined to be dynamic (e.g. dynamic modifications
     * have been made). If, later, a library is loaded that requires one of these not to
     * be dynamic, the loading will fail.
     */
    private val dynamicPredicates = HashSet<ClauseIndicator>()

    fun load(library: Library) {
        // non-unique name?
        loadedLibraries.firstOrNull { it.name == library.name && it !== library }?.let {
            throw PrologRuntimeException("Duplicate library name ${library.name}: already loaded ${System.identityHashCode(it)}, attempted to load ${System.identityHashCode(library)}")
        }

        val staticExports = library.exports - library.dynamicExports

        // static exports of library collide with dynamic state
        staticExports.firstOrNull { it in dynamicPredicates }?.let {
            throw PrologRuntimeException("Cannot load library ${library.name}: has static export $it which has already been dynamically modified.")
        }

        // static exports of library collide with other loaded libraries?
        staticExports
            .asSequence()
            .map { indicator ->
                val library = staticIndex[indicator.arity]?.get(indicator.name)
                if (library == null) null else Pair(library, indicator)
            }
            .filterNotNull()
            .firstOrNull()
            ?.let {
                throw PrologRuntimeException("Cannot load library ${library.name}: static export ${it.second} also exported by library ${it.first.name}")
            }

        // dynamic exports of library collide with other static exports
        library.dynamicExports.
            asSequence()
            .map { indicator ->
                 val library = staticIndex[indicator.arity]?.get(indicator.name)
                 if (library == null) null else Pair(library, indicator)
            }
            .filterNotNull()
            .firstOrNull()
            ?.let {
                throw PrologRuntimeException("Cannot load library ${library.name}: dynamic export ${it.second} is already statically defined by library ${it.first.name}")
            }

        // all good; load dynamic exports into dynamic store
        // and link up the static ones; define operators
        library.dynamicExports.asSequence()
            .flatMap { library.findFor(it).asSequence() }
            .forEach(store::assertz)

        staticExports.forEach { indicator ->
            val nameMap = staticIndex[indicator.arity] ?: {
                val map = HashMap<String, Library>()
                staticIndex[indicator.arity] = map
                map
            }()
            nameMap[indicator.name] = library
        }

        _operators.include(library.operators)
    }



    /**
     * Assures the indicator of the given type is present in [dynamicPredicates].
     * @throws PredicateNotDynamicException If the predicate of the given type is not dynamic.
     */
    private fun assureDynamic(type: HasNameAndArity) = assureDynamic(ClauseIndicator.of(type))

    /**
     * Assures the given indicator is present in [dynamicPredicates].
     * @throws PredicateNotDynamicException If the predicate of the given type is not dynamic.
     */
    private fun assureDynamic(indicator: ClauseIndicator) {
        val library = staticIndex[indicator.arity]?.get(indicator.name)
        if (library != null) {
            // a library has registered this as static
            throw PredicateNotDynamicException(indicator, "Predicate $indicator is not dynamic: marked as static by library ${library.name}")
        }

        dynamicPredicates.add(indicator)
    }

    /**
     * THE absolute CORE builtins that allow anything else to work. These are the assertz and retract
     * builtins.
     */
    private val coreBuiltins: MutableMap<String, suspend LazySequenceBuilder<Unification>.(Authorization, Array<out Term>) -> Unit> = HashMap(8)
    init {
        coreBuiltins["assertz"] = { auth, args ->
            if (args.size != 1) throw PrologRuntimeException("assertz/${args.size} is not defined")

            val clause = args[0] as? Clause ?: throw PrologRuntimeException("Argument 0 to assertz/1 must be a clause")

            val indicator = ClauseIndicator.of(clause)
            if (!auth.mayWrite(indicator)) throw PrologPermissionError("Not allowed to assert $indicator")

            assertz(clause)
            yield(Unification.TRUE)
        }
        coreBuiltins["assert"] = coreBuiltins["assertz"]!!
        coreBuiltins["retract"] = { auth, args: Array<out Term> ->
            if (args.size != 1) throw PrologRuntimeException("retract/${args.size} is not defined")
            val arg0 = args[0] as? Predicate ?: throw PrologRuntimeException("Argument 0 to retract/1 must be a predicate")

            val indicator = ClauseIndicator.of(arg0)
            if (!auth.mayWrite(indicator)) throw PrologPermissionError("Not allowed to retract $indicator")

            assureDynamic(arg0)
            yieldAll(store.retract(arg0))
        }
        coreBuiltins["retractFact"] = { auth, args: Array<out Term> ->
            if (args.size != 1) throw PrologRuntimeException("retract/${args.size} is not defined")
            val arg0 = args[0] as? Predicate ?: throw PrologRuntimeException("Argument 0 to retract/1 must be a predicate")

            val indicator = ClauseIndicator.of(arg0)
            if (!auth.mayWrite(indicator)) throw PrologPermissionError("Not allowed to retract $indicator")

            assureDynamic(arg0)
            yieldAll(store.retractFact(arg0))
        }
        coreBuiltins["retractAll"] = { auth, args: Array<out Term> ->
            if (args.size != 1) throw PrologRuntimeException("retract/${args.size} is not defined")
            val arg0 = args[0] as? Predicate ?: throw PrologRuntimeException("Argument 0 to retract/1 must be a predicate")

            val indicator = ClauseIndicator.of(arg0)
            if (!auth.mayWrite(indicator)) throw PrologPermissionError("Not allowed to retract $indicator")

            assureDynamic(arg0)

            store.retractAll(arg0)
            yield(Unification.TRUE)
        }
        coreBuiltins["retractAllFacts"] = { auth, args: Array<out Term> ->
            if (args.size != 1) throw PrologRuntimeException("retract/${args.size} is not defined")

            val predicate = args[0] as? Predicate ?: throw PrologRuntimeException("Argument 0 to retract/1 must be a predicate")

            val indicator = ClauseIndicator.of(predicate)
            if (!auth.mayWrite(indicator)) throw PrologPermissionError("Not allowed to retract $indicator")

            store.retractAllFacts(predicate)
            yield(Unification.TRUE)
        }
        coreBuiltins["abolish"] = { auth, args: Array<out Term> ->
            if (args.size != 1) throw PrologRuntimeException("abolish/${args.size} is not defined")
            val arg0 = args[0]
            if (arg0 !is Predicate || arg0.arity != 2 || arg0.name != "/") throw PrologRuntimeException("Argument 0 to abolish/1 must be an instance of `/`/2")
            if (arg0.arguments[0] !is Atom || arg0.arguments[1] !is PrologInteger) throw PrologRuntimeException("Argument 0 to abolish/1 must be a predicate indicator")

            val name = (arg0.arguments[0] as Atom).name
            val arity = (arg0.arguments[1] as PrologInteger).value.toInt()

            val indicator = ClauseIndicator.of(name, arity)
            if (!auth.mayWrite(indicator)) throw PrologPermissionError("Not allowed to write $indicator")

            assureDynamic(indicator)

            store.abolish(name, arity)
            yield(Unification.TRUE)
        }
        coreBuiltins["abolishFacts"] = { auth, args: Array<out Term> ->
            if (args.size != 1) throw PrologRuntimeException("abolishFacts/${args.size} is not defined")
            val arg0 = args[0]
            if (arg0 !is Predicate || arg0.arity != 2 || arg0.name != "/") throw PrologRuntimeException("Argument 0 to abolishFacts/1 must be an instance of `/`/2")
            if (arg0.arguments[0] !is Atom || arg0.arguments[1] !is PrologInteger) throw PrologRuntimeException("Argument 0 to abolishFacts/1 must be a predicate indicator")

            val name = (arg0.arguments[0] as Atom).name
            val arity = (arg0.arguments[1] as PrologInteger).value.toInt()

            val indicator = ClauseIndicator.of(name, arity)
            if (!auth.mayWrite(indicator)) throw PrologPermissionError("Not allowed to write $indicator")

            assureDynamic(indicator)

            store.abolishFacts(name, arity)
            yield(Unification.TRUE)
        }
    }

    /**
     * The core implementation of the assertz builtin.
     */
    fun assertz(clause: Clause) {
        // TODO: detect :-/2 instances and convert to rule
        assureDynamic(clause)
        store.assertz(clause)
    }

    private fun findFor(predicate: Predicate): Iterable<Clause> {
        val staticLibrary = staticIndex[predicate.arity]?.get(predicate.name)
        return staticLibrary?.findFor(predicate) ?: store.findFor(predicate)
    }

    private inner class DefaultProofSearchContext(
        override val principal: Principal,
        override val authorization: Authorization,
        override val randomVariableScope: RandomVariableScope
    ) : ProofSearchContext
    {
        override val fulfillAttach: suspend LazySequenceBuilder<Unification>.(Query, VariableBucket) -> Unit = { q, variables ->
            when (q) {
                is AndQuery -> fulfillAndQuery(q, variables)
                is OrQuery -> for (goal in q.goals) fulfillOrQuery(q, variables)
                is PredicateQuery -> fulfillPredicate(q, variables)
            }
        }

        private suspend fun LazySequenceBuilder<Unification>.fulfillAndQuery(query: AndQuery, initialVariables: VariableBucket) {
            val substitutedGoals = query.goals
                .map { it.substituteVariables(initialVariables) }


            fulfillAllGoals(substitutedGoals, this@DefaultProofSearchContext, initialVariables.copy())
        }

        private suspend fun LazySequenceBuilder<Unification>.fulfillAllGoals(goals: List<Query>, context: ProofSearchContext,
                                                                             vars: VariableBucket = VariableBucket()) {
            val goal = goals[0].substituteVariables(vars)

            buildLazySequence<Unification>(context.principal) {
                context.fulfillAttach(this, goal, VariableBucket())
            }
                .forEachRemaining { goalUnification ->
                    val goalVars = vars.copy()
                    for ((variable, value) in goalUnification.variableValues.values) {
                        if (value != null) {
                            // substitute all instantiated variables for simplicity and performance
                            val substitutedValue = value.substituteVariables(goalVars.asSubstitutionMapper())
                            if (goalVars.isInstantiated(variable)) {
                                if (goalVars[variable] != substitutedValue && goalVars[variable] != value) {
                                    // instantiated to different value => no unification
                                    return@forEachRemaining
                                }
                            }
                            else {
                                goalVars.instantiate(variable, substitutedValue)
                            }
                        }
                    }

                    if (goals.size == 1) {
                        // this was the last goal in the list and it is fulfilled
                        // the variable bucket now holds all necessary instantiations
                        yield(Unification(goalVars))
                    }
                    else {
                        fulfillAllGoals(goals.subList(1, goals.size), context, goalVars)
                    }
                }
        }

        private suspend fun LazySequenceBuilder<Unification>.fulfillOrQuery(query: OrQuery, initialVariables: VariableBucket) {
            for (goal in query.goals) {
                fulfillAttach(goal, initialVariables)
            }
        }

        private suspend fun LazySequenceBuilder<Unification>.fulfillPredicate(query: PredicateQuery, initialVariables: VariableBucket) {
            val predicate = query.predicate

            val indicator = ClauseIndicator.of(predicate)
            if (!authorization.mayRead(indicator)) throw PrologPermissionError("Not allowed to read $indicator")

            if (predicate.name in coreBuiltins) {
                val builtin = coreBuiltins[predicate.name]!!

                try {
                    builtin(authorization, predicate.arguments)
                } catch (ex: PrologRuntimeException) {
                    if (predicate is HasPrologSource) {
                        ex.addPrologStackFrame(PrologStackTraceElement(predicate, predicate.sourceInformation))
                    }
                    throw ex
                }

                return
            }

            // replace all variables in the term with random ones to prevent name collisions
            val termMappings = VariableMapping()
            val replaced = randomVariableScope.withRandomVariables(predicate, termMappings)

            for (clause in findFor(predicate)) {
                if (clause is Predicate) {
                    val knownPredicateReplaced = randomVariableScope.withRandomVariables(clause, VariableMapping())
                    val unification = knownPredicateReplaced.unify(replaced)
                    if (unification != null) {
                        val resolvedBucket = unification.variableValues.withVariablesResolvedFrom(termMappings)
                        resolvedBucket.retainAll(predicate.variables)
                        yield(Unification(resolvedBucket))
                    }
                }
                else if (clause is Rule) {
                    yieldAll(
                        buildLazySequence<Unification>(principal) {
                            clause.unifyWithKnowledge(this, predicate, this@DefaultProofSearchContext)
                        }.amendExceptionsWithStackTraceOnRemaining(query.stackFrame)
                    )
                }
                else {
                    throw PrologRuntimeException("Unsupported clause type ${clause.javaClass.name}")
                }
            }
        }
    }

    companion object {
        /**
         * A [LocalKnowledgeBase] that is loaded with these things:
         * * The ISO operators, see [ISOOpsOperatorRegistry]
         * * The following builtin libraries:
         *   * equality
         *   * math
         *   * lists
         *   * strings
         *   * typesafety
         *   * dynamic
         *   * dict
         */
        fun createWithDefaults(): LocalKnowledgeBase {
            val kb = LocalKnowledgeBase()

            (kb.operators as DefaultOperatorRegistry).include(ISOOpsOperatorRegistry)

            kb.load(EqualityLibrary)
            kb.load(ComparisonLibrary)
            kb.load(MathLibrary)
            kb.load(ListsLibrary)
            kb.load(StringsLibrary)
            kb.load(TypeSafetyLibrary)
            kb.load(DynamicsLibrary)
            kb.load(DictLibrary)

            return kb
        }
    }
}