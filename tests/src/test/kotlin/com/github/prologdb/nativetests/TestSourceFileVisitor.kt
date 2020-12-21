package com.github.prologdb.nativetests

import com.github.prologdb.parser.ModuleDeclaration
import com.github.prologdb.parser.parser.DefaultModuleSourceFileVisitor
import com.github.prologdb.runtime.util.OperatorDefinition
import com.github.prologdb.runtime.util.OperatorType
import java.nio.file.Path

class TestSourceFileVisitor(private val filePath: Path) : DefaultModuleSourceFileVisitor(ModuleDeclaration(filePath.toString(), null)) {
    init {
        operators.defineOperator(OperatorDefinition(100, OperatorType.FX, "test"))
        operators.defineOperator(OperatorDefinition(800, OperatorType.XFX, "by"))
    }
}