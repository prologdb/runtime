package com.github.prologdb.runtime.builtin.math

import com.github.prologdb.runtime.knowledge.library.*

val MathLibrary : Library = object : SimpleLibrary(DoublyIndexedLibraryEntryStore(), DefaultOperatorRegistry()) {

    init {
        add(BuiltinIs)
        add(LessThanPredicate)
        add(LessThanOrEqualPredicate)
        add(GreaterThanPredicate)
        add(GreaterThanOrEqualPredicate)

        defineOperator(OperatorDefinition(700, OperatorType.XFX, "<"))
        defineOperator(OperatorDefinition(700, OperatorType.XFX, "=<"))
        defineOperator(OperatorDefinition(700, OperatorType.XFX, "=\\="))
        defineOperator(OperatorDefinition(700, OperatorType.XFX, ">"))
        defineOperator(OperatorDefinition(700, OperatorType.XFX, ">="))
        defineOperator(OperatorDefinition(700, OperatorType.XFX, "is"))

        defineOperator(OperatorDefinition(500, OperatorType.YFX, "+"))
        defineOperator(OperatorDefinition(500, OperatorType.YFX, "-"))
        defineOperator(OperatorDefinition(500, OperatorType.YFX, "xor"))

        defineOperator(OperatorDefinition(400, OperatorType.YFX, "*"))

        defineOperator(OperatorDefinition(400, OperatorType.YFX, "mod"))

        defineOperator(OperatorDefinition(200, OperatorType.XFX, "**"))

        defineOperator(OperatorDefinition(200, OperatorType.XFY, "^"))

        defineOperator(OperatorDefinition(200, OperatorType.FY, "+"))
        defineOperator(OperatorDefinition(200, OperatorType.FY, "-"))
    }
}