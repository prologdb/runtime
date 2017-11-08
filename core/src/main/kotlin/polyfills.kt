internal fun Array<*>?.sensibleHashCode(): Int {
    if (this == null)
        return 0

    var result = 1

    for (element in this)
        result = 31 * result + if (element == null) 0 else element.hashCode()

    return result
}

internal inline fun <T, reified R>Array<T>.mapToArray(mapper: (T) -> R): Array<out R> = Array(this.size, { index -> mapper(this[index]) })