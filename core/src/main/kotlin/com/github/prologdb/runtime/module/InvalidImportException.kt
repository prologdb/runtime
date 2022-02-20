package com.github.prologdb.runtime.module

import com.github.prologdb.runtime.PrologException

class InvalidImportException(
    val importingModuleName: String,
    val import: ModuleImport,
    message: String
) : PrologException(message)