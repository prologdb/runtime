package com.github.prologdb.nativetests

import com.github.prologdb.runtime.PrologRuntimeEnvironment
import com.github.prologdb.runtime.module.CascadingModuleLoader
import com.github.prologdb.runtime.module.ModuleLoader
import com.github.prologdb.runtime.module.ModuleReference
import com.github.prologdb.runtime.stdlib.loader.ClasspathPrologSourceModuleLoader
import com.github.prologdb.runtime.stdlib.loader.StandardLibraryModuleLoader
import java.nio.file.Path

object TestingModuleLoader : ModuleLoader {
    private val delegate = CascadingModuleLoader(
        StandardLibraryModuleLoader,
        ClasspathPrologSourceModuleLoader(
            classLoader = PrologTest::class.java.classLoader,
            moduleReferenceToClasspathPath = { ref -> ref.moduleName },
            sourceFileVisitorSupplier = { _, runtime -> TestSourceFileVisitor(runtime) }
        )
    )

    fun moduleReference(path: Path) = ModuleReference("test", path.fileName.toString())

    override fun initiateLoading(reference: ModuleReference, runtime: PrologRuntimeEnvironment) = delegate.initiateLoading(reference, runtime)
}
