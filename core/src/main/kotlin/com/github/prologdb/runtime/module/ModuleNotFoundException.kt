package com.github.prologdb.runtime.module

import com.github.prologdb.runtime.PrologRuntimeException

class ModuleNotFoundException(val reference: ModuleReference) : PrologRuntimeException("Module $reference not found") {
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
