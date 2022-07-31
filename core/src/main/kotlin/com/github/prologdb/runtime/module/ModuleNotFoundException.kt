package com.github.prologdb.runtime.module

import com.github.prologdb.runtime.PrologException

/**
 * Thrown when a module is to be loaded but is not found.
 */
class ModuleNotFoundException(val reference: ModuleReference, message: String? = null, cause: Throwable? = null) : PrologException(message ?: "Module $reference not found", cause)
