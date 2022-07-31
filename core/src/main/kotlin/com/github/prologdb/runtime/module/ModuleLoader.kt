package com.github.prologdb.runtime.module

import com.github.prologdb.runtime.PrologRuntimeEnvironment

interface ModuleLoader {
    fun initiateLoading(reference: ModuleReference, runtime: PrologRuntimeEnvironment): PrimedStage

    interface PrimedStage {
        val declaration: ModuleDeclaration

        fun proceed(): ParsedStage
    }

    interface ParsedStage {
        val module: Module
    }
}

