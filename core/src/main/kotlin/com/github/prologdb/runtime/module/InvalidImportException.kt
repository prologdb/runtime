package com.github.prologdb.runtime.module

import com.github.prologdb.runtime.PrologRuntimeException

class InvalidImportException(
    val importingModuleName: String,
    val import: ModuleImport,
    message: String
) : PrologRuntimeException(message)