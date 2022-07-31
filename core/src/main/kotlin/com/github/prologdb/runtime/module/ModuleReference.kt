package com.github.prologdb.runtime.module

data class ModuleReference(
    /**
     * The alias for the path pointing to the module files parent directory,
     * e.g. `library` or `system`.
     */
    val pathAlias: String,

    val moduleName: String
) {
    override fun toString(): String {
        return "$pathAlias($moduleName)"
    }
}
