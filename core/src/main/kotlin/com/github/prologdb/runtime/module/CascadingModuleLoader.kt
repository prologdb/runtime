package com.github.prologdb.runtime.module

/**
 * Composed of multiple [ModuleLoader]s. Tries to load modules from each
 * of them, in user-defined order, and returns the first successful result.
 */
class CascadingModuleLoader(
    private val delegates: List<ModuleLoader>
) : ModuleLoader {
    constructor(vararg delegates: ModuleLoader) : this(delegates.toList())

    override fun load(reference: ModuleReference): Module {
        var notFoundException: ModuleNotFoundException? = null

        for (loader in delegates) {
            try {
                return loader.load(reference)
            } catch (ex: ModuleNotFoundException) {
                notFoundException = notFoundException
                    ?.also { it.addSuppressed(ex) }
                    ?: ex
            }
        }

        throw notFoundException ?: ModuleNotFoundException(reference)
    }
}
