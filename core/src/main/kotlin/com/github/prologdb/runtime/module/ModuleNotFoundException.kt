package com.github.prologdb.runtime.module

import com.github.prologdb.runtime.PrologException

/**
 * Thrown when a module is to be loaded but is not found.
 */
class ModuleNotFoundException(val reference: ModuleReference, cause: Throwable? = null) :
    PrologException("Module $reference not found", cause) {
    private val _loadChain = mutableListOf<String>()
    val loadChain: List<String> = _loadChain

    fun addLoadChainElement(element: String) {
        _loadChain.add(element)
    }

    override val message: String
        get() = if (loadChain.isEmpty()) super.message!! else {
            super.message + loadChain.joinToString(
                prefix = "\n\tmodule ${reference.moduleName}\n\t",
                separator = "\n\t"
            )
        }
}
