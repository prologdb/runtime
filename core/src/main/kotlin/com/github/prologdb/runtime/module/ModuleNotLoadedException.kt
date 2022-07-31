package com.github.prologdb.runtime.module

import com.github.prologdb.runtime.PrologException

/**
 * Thrown when a module is expected to be loaded, but isn't. This can indicate that code dynamically
 * references a module that it didn't load (e.g. `ModuleName:goal()`), or that the interpreter failed
 * to load a module that it should have loaded.
 */
class ModuleNotLoadedException @JvmOverloads constructor(val moduleName: String, cause: Throwable? = null) : PrologException(
    if (cause != null) "Module $moduleName failed to load: ${cause.message}" else "Module $moduleName not loaded",
    cause
)