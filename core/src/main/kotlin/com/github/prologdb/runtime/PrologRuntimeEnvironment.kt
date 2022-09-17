package com.github.prologdb.runtime

import com.github.prologdb.async.LazySequence
import com.github.prologdb.async.buildLazySequence
import com.github.prologdb.async.mapRemaining
import com.github.prologdb.runtime.module.InvalidImportException
import com.github.prologdb.runtime.module.Module
import com.github.prologdb.runtime.module.ModuleDeclaration
import com.github.prologdb.runtime.module.ModuleImport
import com.github.prologdb.runtime.module.ModuleLoader
import com.github.prologdb.runtime.module.ModuleNotFoundException
import com.github.prologdb.runtime.module.ModuleNotLoadedException
import com.github.prologdb.runtime.module.ModuleReference
import com.github.prologdb.runtime.module.ModuleScopeProofSearchContext
import com.github.prologdb.runtime.module.NoopModuleLoader
import com.github.prologdb.runtime.proofsearch.Authorization
import com.github.prologdb.runtime.proofsearch.PrologCallable
import com.github.prologdb.runtime.proofsearch.ProofSearchContext
import com.github.prologdb.runtime.proofsearch.ReadWriteAuthorization
import com.github.prologdb.runtime.query.Query
import com.github.prologdb.runtime.term.MathContext
import com.github.prologdb.runtime.unification.Unification
import java.util.UUID
import java.util.concurrent.ConcurrentHashMap

interface PrologRuntimeEnvironment {
    fun newProofSearchContext(moduleName: String, authorization: Authorization = ReadWriteAuthorization): ProofSearchContext
    fun deriveProofSearchContextForModule(deriveFrom: ProofSearchContext, moduleName: String): ProofSearchContext

    /**
     * Module loading is a two-step process due to exported operators and cyclic imports.
     * This method assures that the first step (parsing the [ModuleDeclaration]) of a module has
     * taken place.
     * @throws ModuleNotFoundException
     */
    fun assureModulePrimed(moduleReference: ModuleReference): ModuleDeclaration

    /**
     * This method assures that the module is fully parsed (but maybe not linked!).
     * @throws ModuleNotFoundException
     */
    fun assureModuleParsed(moduleReference: ModuleReference): Module

    /**
     * Completely loads the given module, if not loaded already.
     */
    fun assureModuleLoaded(moduleReference: ModuleReference): Module {
        assureModulePrimed(moduleReference)
        return getLoadedModule(moduleReference.moduleName)
    }

    /**
     * If a module of the given name is loaded or primed (see [assureModulePrimed], [assureModuleLoaded]),
     * returns that module.
     * @throws ModuleNotLoadedException
     */
    fun getLoadedModule(name: String): Module

    /**
     * Convenience method for Java to do a proof search with the [ProofSearchContext.fulfillAttach] coroutine
     */
    fun fulfill(inModule: String, query: Query, initialVariables: Unification, authorization: Authorization): LazySequence<Unification> {
        val psc = newProofSearchContext(inModule, authorization)
        return buildLazySequence<Unification>(psc.principal) {
            psc.fulfillAttach(this, query, initialVariables)
        }
            .mapRemaining { it.compacted(aggressive = true) }
    }

    /**
     * Convenience method for Java to do a proof search with the [ProofSearchContext.fulfillAttach] coroutine
     */
    fun fulfill(inModule: String, query: Query, initialVariables: Unification): LazySequence<Unification> {
        return fulfill(inModule, query, initialVariables, ReadWriteAuthorization)
    }

    /**
     * Convenience method for Java to do a proof search with the [ProofSearchContext.fulfillAttach] coroutine
     */
    fun fulfill(inModule: String, query: Query): LazySequence<Unification> {
        return fulfill(inModule, query, Unification.TRUE, ReadWriteAuthorization)
    }

    companion object {
        /**
         * @return a table that, considering all imports, maps unscoped predicates to the actual implementations
         */
        @JvmStatic
        fun buildModuleLookupTable(runtimeEnvironment: PrologRuntimeEnvironment, forModule: Module): Map<ClauseIndicator, Pair<ModuleReference, PrologCallable>> {
            val importLookupCache = mutableMapOf<ClauseIndicator, Pair<ModuleReference, PrologCallable>>()
            for (import in forModule.imports) {
                val referencedModule = runtimeEnvironment.assureModuleParsed(import.moduleReference)

                val visiblePredicates: Map<ClauseIndicator, Pair<ModuleReference, PrologCallable>> = when (import) {
                    is ModuleImport.Full      -> referencedModule.exportedPredicates
                        .mapValues { (_, callable) ->
                            Pair(import.moduleReference, callable)
                        }
                    is ModuleImport.Selective -> import.predicates
                        .map { (exportedIndicator, alias) ->
                            val callable = referencedModule.exportedPredicates[exportedIndicator]
                                ?: throw InvalidImportException(
                                    forModule.declaration.moduleName,
                                    import,
                                    if (exportedIndicator in referencedModule.allDeclaredPredicates) {
                                        "$exportedIndicator is private in module ${import.moduleReference}"
                                    } else {
                                        "$exportedIndicator is not defined by module ${import.moduleReference}"
                                    },
                                )

                            if (exportedIndicator.functor == alias) {
                                exportedIndicator to Pair(import.moduleReference, callable)
                            } else {
                                ClauseIndicator.of(alias, exportedIndicator.arity) to Pair(import.moduleReference, callable)
                            }
                        }
                        .toMap()

                    is ModuleImport.Except    -> referencedModule.exportedPredicates
                        .filterKeys { it !in import.excludedPredicates }
                        .mapValues { (_, callable) ->
                            Pair(import.moduleReference, callable)
                        }
                }

                importLookupCache.putAll(visiblePredicates)
            }

            return importLookupCache
        }
    }
}

open class DefaultPrologRuntimeEnvironment @JvmOverloads constructor(
    protected val moduleLoader: ModuleLoader = NoopModuleLoader,
    private val mathContext: MathContext = MathContext.DEFAULT,
) : PrologRuntimeEnvironment {

    /**
     * Maps module names to their loading states. Value can be:
     * * [ModuleLoader.PrimedStage]
     * * [ModuleLoader.ParsedStage]
     * * [LinkableStage]
     * * [LinkedStage]
     * * [Exception] (in case of an error)
     */
    private val moduleLoadingStages: MutableMap<String, Any> = ConcurrentHashMap()

    protected val moduleLookupTables: MutableMap<String, Map<ClauseIndicator, Pair<ModuleReference, PrologCallable>>> = ConcurrentHashMap()
    private val moduleLoadingMutex = Any()

    // TODO: check for indicator collisions in module exports

    override fun assureModulePrimed(moduleReference: ModuleReference): ModuleDeclaration {
        return when (val stage = moduleLoadingStages[moduleReference.moduleName]) {
            null -> {
                synchronized(moduleLoadingMutex) {
                    if (moduleReference.moduleName !in moduleLoadingStages) {
                        moduleLoadingStages[moduleReference.moduleName] =  moduleLoader.initiateLoading(moduleReference, this)
                    }
                }

                assureModulePrimed(moduleReference)
            }
            is ModuleLoader.PrimedStage -> stage.declaration
            is ModuleLoader.ParsedStage -> stage.module.declaration
            is LinkableStage -> stage.module.declaration
            is LinkedStage -> stage.module.declaration
            is Exception -> throw stage
            else -> error("${this::moduleLoadingStages.name} contained value of unexpected type ${stage::class.qualifiedName}")
        }
    }

    override fun assureModuleParsed(moduleReference: ModuleReference): Module {
        assureModulePrimed(moduleReference)
        return assureModuleParsed(moduleReference.moduleName)
    }

    private fun assureModuleParsed(name: String): Module {
        return when (val stage = moduleLoadingStages[name]) {
            is ModuleLoader.PrimedStage -> {
                synchronized(moduleLoadingMutex) {
                    val antiRaceStage = moduleLoadingStages[name]
                    if (antiRaceStage !== stage) {
                        return assureModuleParsed(name)
                    }

                    val parsedStage = try {
                        stage.proceed()
                    } catch (ex: Exception) {
                        moduleLoadingStages[name] = ex
                        throw ex
                    }

                    moduleLoadingStages[name] = parsedStage
                    parsedStage.module
                }
            }
            is ModuleLoader.ParsedStage -> stage.module
            is LinkableStage -> stage.module
            is LinkedStage-> stage.module
            is Exception -> throw stage
            null -> throw IllegalStateException("Module $name was not primed. Call ${this::assureModulePrimed.name} first")
            else -> error("${this::moduleLoadingStages.name} contained value of unexpected type ${stage::class.qualifiedName}")
        }
    }

    /**
     * Brings the given module (and all its transitive dependencies) from [ModuleLoader.PrimedStage]
     * to [LinkableStage] (important for cyclic imports)
     */
    private fun assureModuleLinkable(name: String): Module {
        assureModuleParsed(name)

        return when (val stage = moduleLoadingStages[name]) {
            is ModuleLoader.ParsedStage -> {
                synchronized(moduleLoadingMutex) {
                    val antiRaceStage = moduleLoadingStages[name]
                    if (antiRaceStage !== stage) {
                        return assureModuleLinkable(name)
                    }

                    val allImports = assureAllImportsParsed(stage.module)
                    moduleLoadingStages[name] = LinkableStage(stage.module, allImports)
                    stage.module
                }
            }
            is LinkableStage -> stage.module
            is LinkedStage -> stage.module
            is ModuleLoader.PrimedStage -> error("Unexpected stage ${stage::class.simpleName} for module $name")
            is Exception -> throw stage
            null -> throw IllegalStateException("Module $name was not primed. Call ${this::assureModulePrimed.name} first")
            else -> error("${this::moduleLoadingStages.name} contained value of unexpected type ${stage::class.qualifiedName}")
        }
    }

    private fun assureModuleLinked(name: String): Module {
        assureModuleLinkable(name)

        fun link(module: Module) {
            if (module.declaration.moduleName !in moduleLookupTables) {
                moduleLookupTables[module.declaration.moduleName] = PrologRuntimeEnvironment.buildModuleLookupTable(this, module)
            }
        }

        return when (val stage = moduleLoadingStages[name]) {
            is LinkableStage -> {
                synchronized(moduleLoadingMutex) {
                    val antiRaceStage = moduleLoadingStages[name]
                    if (antiRaceStage !== stage) {
                        return assureModuleLinked(name)
                    }

                    try {
                        link(stage.module)
                        for (import in stage.allImports) {
                            val importedModule = assureModuleLinkable(import.moduleName)
                            link(importedModule)
                            moduleLoadingStages[importedModule.declaration.moduleName] = LinkedStage(importedModule)
                        }
                    }
                    catch (ex: Exception) {
                        moduleLoadingStages[name] = ex
                        throw ex
                    }

                    moduleLoadingStages[name] = LinkedStage(stage.module)
                    stage.module
                }
            }
            is LinkedStage -> stage.module
            is ModuleLoader.PrimedStage,
            is ModuleLoader.ParsedStage -> error("Unexpected stage ${stage::class.simpleName} for module $name")
            is Exception -> throw stage
            null -> throw IllegalStateException("Module $name was not primed. Call ${this::assureModulePrimed.name} first")
            else -> error("${this::moduleLoadingStages.name} contained value of unexpected type ${stage::class.qualifiedName}")
        }
    }

    /**
     * @return [collectTo]
     */
    private fun assureAllImportsParsed(module: Module, collectTo: MutableSet<ModuleReference> = HashSet(module.imports.size * 2)): Set<ModuleReference> {
        for (import in module.imports) {
            if (import.moduleReference in collectTo) {
                continue
            }

            collectTo.add(import.moduleReference)

            assureModulePrimed(import.moduleReference)
            val importedModule = assureModuleParsed(import.moduleReference.moduleName)
            assureAllImportsParsed(importedModule, collectTo)
        }

        return collectTo
    }

    override fun getLoadedModule(name: String): Module {
        if (name !in moduleLoadingStages) {
            throw ModuleNotLoadedException(name)
        }

        return try {
            assureModuleLinked(name)
        }
        catch (ex: Exception) {
            throw ModuleNotLoadedException(name, ex)
        }
    }

    override fun newProofSearchContext(moduleName: String, authorization: Authorization): ProofSearchContext {
        val module = getLoadedModule(moduleName)

        return ModuleScopeProofSearchContext(
            module,
            this,
            moduleLookupTables.getValue(module.declaration.moduleName),
            UUID.randomUUID(),
            RandomVariableScope(),
            authorization,
            mathContext,
        )
    }

    override fun deriveProofSearchContextForModule(deriveFrom: ProofSearchContext, moduleName: String): ProofSearchContext {
        if (deriveFrom.module.declaration.moduleName == moduleName) {
            return deriveFrom
        }

        val module = getLoadedModule(moduleName)
        return ModuleScopeProofSearchContext(
            module,
            this,
            moduleLookupTables.getValue(module.declaration.moduleName),
            deriveFrom.principal,
            deriveFrom.randomVariableScope,
            deriveFrom.authorization,
            mathContext,
        )
    }

    private class LinkableStage(
        val module: Module,
        val allImports: Set<ModuleReference>,
    )

    private class LinkedStage(
        val module: Module
    )
}
