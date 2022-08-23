package com.github.prologdb.runtime.util

class MappingMutableIterator<T : Any, M>(private val base: MutableIterator<T>, private val mapper: (T) -> M): MutableIterator<M> {
    override fun hasNext(): Boolean = base.hasNext()
    override fun next(): M = mapper(base.next())
    override fun remove() = base.remove()
}