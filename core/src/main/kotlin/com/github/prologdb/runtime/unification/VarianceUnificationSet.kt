package com.github.prologdb.runtime.unification

import com.github.prologdb.runtime.RandomVariableScope
import com.github.prologdb.runtime.term.equalsStructurally
import com.github.prologdb.runtime.util.MappingMutableIterator

/**
 * A [MutableSet]<[Unification]> that equates elements by variance (structural equality)
 * @see equalsStructurally
 */
class VarianceUnificationSet(private val randomVariableScope: RandomVariableScope) : MutableSet<Unification> {
    private val grouping = UnificationGrouping<Unit>(randomVariableScope)
    override val size: Int get() = grouping.size
    override fun contains(element: Unification): Boolean = grouping.contains(element)
    override fun containsAll(elements: Collection<Unification>): Boolean = elements.all { it in this }
    override fun isEmpty(): Boolean = size == 0
    override fun iterator(): MutableIterator<Unification> = MappingMutableIterator(grouping.iterator()) { (vars, _) -> vars }
    override fun add(element: Unification): Boolean = grouping.set(element, Unit) == null
    override fun addAll(elements: Collection<Unification>): Boolean = elements.map { add(it) }.any()
    override fun clear() {
        grouping.clear()
    }
    override fun remove(element: Unification): Boolean = grouping.remove(element) != null
    override fun removeAll(elements: Collection<Unification>): Boolean = elements.map { remove(it) }.any()
    override fun retainAll(elements: Collection<Unification>): Boolean {
        val toRetain = if (elements is MutableCollection<Unification>) elements else elements.toMutableList()
        var anyRemoved = false
        val iterator = iterator()
        groupingElements@while (iterator.hasNext()) {
            val element = iterator.next()
            val toRetainIterator = toRetain.iterator()
            retainers@while (toRetainIterator.hasNext()) {
                val toRetainElement = toRetainIterator.next()
                if (toRetainElement.equalsStructurally(element, randomVariableScope)) {
                    toRetainIterator.remove()
                    toRetainIterator.forEachRemaining {}
                    continue@groupingElements
                }
            }

            // no entry in toRetain matches the current element
            iterator.remove()
            anyRemoved = true
        }

        return anyRemoved
    }
}