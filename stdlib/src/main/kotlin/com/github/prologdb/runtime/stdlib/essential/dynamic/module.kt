package com.github.prologdb.runtime.stdlib.essential.dynamic

import com.github.prologdb.parser.lexer.Operator
import com.github.prologdb.runtime.term.CompoundTerm
import com.github.prologdb.runtime.term.Term

internal fun Term.commaCompoundToList(): List<Term> {
    val list = mutableListOf<Term>()
    var pivot = this
    while (pivot is CompoundTerm && pivot.arity == 2 && pivot.functor == Operator.COMMA.text) {
        list.add(pivot.arguments[0])
        pivot = pivot.arguments[1]
    }

    list.add(pivot)

    return list
}
