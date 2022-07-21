package com.github.prologdb.runtime.module

import com.github.prologdb.runtime.PrologException

/**
 * Thrown when a module is to be loaded but is not found.
 */
class ModuleNotFoundException(val reference: ModuleReference, cause: Throwable? = null) : PrologException("Module $reference not found", cause)
