package com.github.prologdb.runtime.module

/**
 * Throws an exception when attempting to load any module.
 */
object NoopModuleLoader : ModuleLoader {
    override fun load(reference: ModuleReference): Module {
        throw ModuleNotFoundException(reference)
    }
}
