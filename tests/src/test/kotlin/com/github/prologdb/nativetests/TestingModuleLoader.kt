package com.github.prologdb.nativetests

import com.github.prologdb.runtime.module.CascadingModuleLoader
import com.github.prologdb.runtime.stdlib.loader.ClasspathPrologSourceModuleLoader
import com.github.prologdb.runtime.stdlib.loader.StandardLibraryModuleLoader

val TestingModuleLoader = CascadingModuleLoader(
    StandardLibraryModuleLoader,
    ClasspathPrologSourceModuleLoader(classLoader = PrologTest::class.java.classLoader)
)
