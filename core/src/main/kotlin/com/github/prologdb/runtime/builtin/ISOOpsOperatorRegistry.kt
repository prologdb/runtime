@file:JvmMultifileClass
@file:JvmName("ISOOpsOperatorRegistry")
package com.github.prologdb.runtime.builtin

import com.github.prologdb.runtime.util.*

/**
 * An [OperatorRegistry] that contains these ISO operators:
 *
 * ```
 * |Priority|Associativity|Name     |
 * |--------|-------------|---------|
 * |1200    |XFX          |`:-`     |
 * |        |FX           |`:-`     |
 * |        |FX           |`?-`     |
 * |1150    |FX           |`dynamic`|
 * |1100    |XFY          |`;`      |
 * |        |XFY          |`|`      |
 * |1000    |XFY          |`,`      |
 * |900     |FY           |`\+`     |
 * |        |XFY          |`typeof` |
 * |600     |XFY          |`:`      |
 * |700     |XFX          |`=`      |
 * |        |XFX          |`==`     |
 * |        |XFX          |`\=`     |
 * |        |XFX          |`\==`    |
 * |        |XFX          |`@<`     |
 * |        |XFX          |`@=<`    |
 * |        |XFX          |`@>`     |
 * |        |XFX          |`@>=`    |
 * |        |XFX          |`=@=`    |
 * |        |XFX          |`\=@=`   |
 * |        |XFX          |`<`      |
 * |        |XFX          |`=<`     |
 * |        |XFX          |`=\=`    |
 * |        |XFX          |`>`      |
 * |        |XFX          |`>=`     |
 * |        |XFX          |`is`     |
 * |500     |YFX          |`+`      |
 * |        |YFX          |`-`      |
 * |        |YFX          |`xor`    |
 * |400     |YFX          |`/`      |
 * |        |YFX          |`*`      |
 * |        |YFX          |`mod`    |
 * |200     |XFX          |`**`     |
 * |        |XFY          |`^`      |
 * |        |FY           |`+`      |
 * |        |FY           |`-`      |
 * ```
 */
@get:JvmName("getInstance")
val ISOOpsOperatorRegistry: OperatorRegistry = DefaultOperatorRegistry().apply {
    defineOperator(1200, OperatorType.XFX, ":-")
    defineOperator(1200, OperatorType.FX, ":-")
    defineOperator(1200, OperatorType.FX, "?-")
    defineOperator(1150, OperatorType.FX, "dynamic")
    defineOperator(1100, OperatorType.XFY, ";")
    defineOperator(1100, OperatorType.XFY, "|")
    defineOperator(1000, OperatorType.XFY, ",")
    defineOperator(900, OperatorType.FY, "\\+")
    defineOperator(900, OperatorType.XFY, "typeof")
    defineOperator(600, OperatorType.XFY, ":")
    defineOperator(700, OperatorType.XFX, "=")
    defineOperator(700, OperatorType.XFX, "==")
    defineOperator(700, OperatorType.XFX, "\\=")
    defineOperator(700, OperatorType.XFX, "\\==")
    defineOperator(700, OperatorType.XFX, "@<")
    defineOperator(700, OperatorType.XFX, "@=<")
    defineOperator(700, OperatorType.XFX, "@>")
    defineOperator(700, OperatorType.XFX, "@>=")
    defineOperator(700, OperatorType.XFX, "=@=")
    defineOperator(700, OperatorType.XFX, "\\=@=")
    defineOperator(700, OperatorType.XFX, "<")
    defineOperator(700, OperatorType.XFX, "=<")
    defineOperator(700, OperatorType.XFX, "=\\=")
    defineOperator(700, OperatorType.XFX, ">")
    defineOperator(700, OperatorType.XFX, ">=")
    defineOperator(700, OperatorType.XFX, "is")
    defineOperator(500, OperatorType.YFX, "+")
    defineOperator(500, OperatorType.YFX, "-")
    defineOperator(500, OperatorType.YFX, "xor")
    defineOperator(400, OperatorType.YFX, "/")
    defineOperator(400, OperatorType.YFX, "*")
    defineOperator(400, OperatorType.YFX, "mod")
    defineOperator(200, OperatorType.XFX, "**")
    defineOperator(200, OperatorType.XFY, "^")
    defineOperator(200, OperatorType.FY, "+")
    defineOperator(200, OperatorType.FY, "-")
}

private fun OperatorRegistrationTarget.defineOperator(precedence: Short, type: OperatorType, name: String) {
    defineOperator(OperatorDefinition(precedence, type, name))
}
