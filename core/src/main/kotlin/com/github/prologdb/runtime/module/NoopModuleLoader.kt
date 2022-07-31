package com.github.prologdb.runtime.module

import com.github.prologdb.runtime.PrologRuntimeEnvironment

/**
 * Throws an exception when attempting to load any module.
 */
object NoopModuleLoader : ModuleLoader {
    /**
     * @throws ModuleNotFoundException
     */
    override fun initiateLoading(reference: ModuleReference, runtime: PrologRuntimeEnvironment): Nothing {
        throw ModuleNotFoundException(reference)
    }
}
