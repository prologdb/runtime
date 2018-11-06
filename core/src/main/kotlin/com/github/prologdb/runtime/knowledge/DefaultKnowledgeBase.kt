package com.github.prologdb.runtime.knowledge

import com.github.prologdb.runtime.builtin.ComparisonLibrary
import com.github.prologdb.runtime.builtin.EqualityLibrary
import com.github.prologdb.runtime.builtin.dict.DictLibrary
import com.github.prologdb.runtime.builtin.dynamic.DynamicsLibrary
import com.github.prologdb.runtime.builtin.lists.ListsLibrary
import com.github.prologdb.runtime.builtin.math.MathLibrary
import com.github.prologdb.runtime.builtin.string.StringsLibrary
import com.github.prologdb.runtime.builtin.typesafety.TypeSafetyLibrary
import com.github.prologdb.runtime.knowledge.library.DefaultOperatorRegistry
import com.github.prologdb.runtime.knowledge.library.DoublyIndexedClauseStore
import com.github.prologdb.runtime.knowledge.library.Library
import com.github.prologdb.runtime.term.Predicate

class DefaultKnowledgeBase(val library: MutableLibrary) : MutableKnowledgeBase {

    constructor() : this(SimpleLibrary(
        DoublyIndexedClauseStore(),
        DefaultOperatorRegistry(true)
    )) {
        load(EqualityLibrary)
        load(ComparisonLibrary)
        load(TypeSafetyLibrary)
        load(MathLibrary)
        load(StringsLibrary)
        load(ListsLibrary)
        load(DictLibrary)
        load(DynamicsLibrary)
    }

    override val operatorRegistry = library

    override fun assert(predicate: Predicate) {
        library.add(predicate)
    }

    override fun defineRule(rule: Rule) {
        library.add(rule)
    }

    override fun load(library: Library) {
        this.library.include(library)
    }
}