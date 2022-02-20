package com.github.prologdb.runtime.stdlib.essential.string

import com.github.prologdb.runtime.ArgumentTypeError
import com.github.prologdb.runtime.stdlib.nativeConversionRule
import com.github.prologdb.runtime.term.Atom
import com.github.prologdb.runtime.term.PrologDecimal
import com.github.prologdb.runtime.term.PrologInteger
import com.github.prologdb.runtime.term.PrologString
import com.github.prologdb.runtime.term.Term

val BuiltinAtomString2 = nativeConversionRule<Term, PrologString>(
    "atom_string",
    { atom -> when(atom) {
        is PrologInteger -> PrologString(atom.value.toString())
        is PrologDecimal -> PrologString(atom.value.toString())
        is Atom -> PrologString(atom.name)
        else -> throw ArgumentTypeError(0, atom, PrologInteger::class.java, PrologDecimal::class.java, Atom::class.java)
    } },
    { str -> Atom(str.toKotlinString()) }
)
