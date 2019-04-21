package com.github.prologdb.runtime.builtin

import com.github.prologdb.runtime.knowledge.library.*

/**
 * An [OperatorRegistry] that contains these ISO operators:
 *
 * ```prolog
 * :- op(1200, xfx, `:-`     )
 * :- op(1200, fx,  `:-`     )
 * :- op(1200, fx,  `?-`     )
 * :- op(1150, fx,  `dynamic`)
 * :- op(1100, xfy, `;`      )
 * :- op(1100, xfy, `|`      )
 * :- op(1000, xfy, `,`      )
 * :- op(600,  xfy, `:`      )
 * :- op(400,  yfx, `/`      )
 * ```
 *
 * Others are to be found in [EqualityLibrary] and [MathLibrary]
 */
val ISOOpsOperatorRegistry = DefaultOperatorRegistry().apply {
    defineOperator(1200, OperatorType.XFX, ":-")
    defineOperator(1200, OperatorType.FX, ":-")
    defineOperator(1200, OperatorType.FX, "?-")
    defineOperator(1150, OperatorType.FX, "dynamic")
    defineOperator(1100, OperatorType.XFY, ";")
    defineOperator(1100, OperatorType.XFY, "|")
    defineOperator(1000, OperatorType.XFY, ",")
    defineOperator(900, OperatorType.FY, "\\+")
    defineOperator(600, OperatorType.XFY, ":")
    defineOperator(700, OperatorType.XFX, "=")
    defineOperator(700, OperatorType.XFX, "==")
    defineOperator(700, OperatorType.XFX, "\\=")
    defineOperator(700, OperatorType.XFX, "\\==")
    defineOperator(400, OperatorType.YFX, "/")
}

private fun OperatorRegistrationTarget.defineOperator(precedence: Short, type: OperatorType, name: String) {
    defineOperator(OperatorDefinition(precedence, type, name))
}
