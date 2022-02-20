package com.github.prologdb.runtime.module

import com.github.prologdb.runtime.PrologInternalError

/**
 * Thrown when a module is expected to be loaded, but isn't. In contrast to [ModuleNotFoundException],
 * this one indicates a bug in the interpreter, rather than a user-error.
 */
class ModuleNotLoadedException(val moduleName: String) : PrologInternalError("Module $moduleName is not loaded") {
    constructor(reference: ModuleReference) : this(reference.moduleName)
}