package com.github.prologdb.runtime.builtin

import com.github.prologdb.runtime.knowledge.library.DefaultOperatorRegistry
import com.github.prologdb.runtime.knowledge.library.OperatorDefinition
import com.github.prologdb.runtime.knowledge.library.OperatorRegistry
import com.github.prologdb.runtime.knowledge.library.OperatorType

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
object ISOOpsOperatorRegistry : OperatorRegistry {
    private val delegate = DefaultOperatorRegistry()

    val COLON_DASH           = setOf(OperatorDefinition(1200, OperatorType.XFX, ":-"), OperatorDefinition(1200, OperatorType.FX, ":-"))
    val QUERY_PREFIX         = setOf(OperatorDefinition(1200, OperatorType.FX, "?-"))
    val DIRECTIVE_DYNAMIC    = setOf(OperatorDefinition(1150, OperatorType.FX, "dynamic"))
    val QUERY_OR             = setOf(OperatorDefinition(1100, OperatorType.XFY, ";"))
    val LIST_TAIL_SEPARATOR  = setOf(OperatorDefinition(1100, OperatorType.XFY, "|"))
    val QUERY_AND            = setOf(OperatorDefinition(1000, OperatorType.XFY, ","))
    val DICT_ENTRY_SEPARATOR = setOf(OperatorDefinition(600, OperatorType.XFY, ":"))
    // equality and inequality operators are defined in EqualityLibrary
    // math operators are defined in MathLibrary; /2 is an exception
    // because it is used to denote predicates with dynamic, e.g.: :- dynamic predicateName/3.
    val CLAUSE_INDICATOR     = setOf(OperatorDefinition(400, OperatorType.YFX, "/"))

    override fun getOperatorDefinitionsFor(name: String): Set<OperatorDefinition> = when(name) {
        // sorted by expected frequencey descending, hoping to optimize

        ","       -> QUERY_AND
        ";"       -> QUERY_OR
        "|"       -> LIST_TAIL_SEPARATOR
        "/"       -> CLAUSE_INDICATOR // likely to be used in is/2
        ":"       -> DICT_ENTRY_SEPARATOR
        ":-"      -> COLON_DASH
        "?-"      -> QUERY_PREFIX
        "dynamic" -> DIRECTIVE_DYNAMIC
        else      -> emptySet()
    }

    override val allOperators: Iterable<OperatorDefinition> = COLON_DASH + QUERY_PREFIX + DIRECTIVE_DYNAMIC +
        QUERY_OR + LIST_TAIL_SEPARATOR + QUERY_AND + DICT_ENTRY_SEPARATOR + CLAUSE_INDICATOR
}