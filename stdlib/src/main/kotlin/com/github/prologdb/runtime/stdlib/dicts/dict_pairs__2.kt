package com.github.prologdb.runtime.stdlib.dicts

import com.github.prologdb.runtime.PrologInvocationContractViolationException
import com.github.prologdb.runtime.stdlib.nativeConversionRule
import com.github.prologdb.runtime.term.Atom
import com.github.prologdb.runtime.term.CompoundTerm
import com.github.prologdb.runtime.term.PrologDictionary
import com.github.prologdb.runtime.term.PrologList

val BuiltinDictPairs2 = nativeConversionRule<PrologDictionary, PrologList>(
    "dict_pairs",
    { dictionary ->
        PrologList(
            dictionary.pairs.entries.map { (key, value) ->
                CompoundTerm("-", arrayOf(key, value))
            },
            dictionary.tail,
        )
    },
    { list ->
        PrologDictionary(
            list.elements.associate { pair ->
                if (pair !is CompoundTerm || pair.functor != "-" || pair.arity != 2 || pair.arguments[0] !is Atom) {
                    throw PrologInvocationContractViolationException("All pairs given to dict_pairs must be instances of '-'/2 with the first argument being an atom")
                }

                pair.arguments[0] as Atom to pair.arguments[1]
            },
            list.tail,
        )
    }
)