internal fun Array<*>?.sensibleHashCode(): Int {
    if (this == null)
        return 0

    var result = 1

    for (element in this)
        result = 31 * result + (element?.hashCode() ?: 0)

    return result
}

internal inline fun <T, reified R> Array<out T>.mapToArray(mapper: (T) -> R): Array<out R> = Array(this.size, { index -> mapper(this[index]) })
internal inline fun <T, reified R> Array<out T>.mapIndexedToArray(mapper: (T, Int) -> R): Array<out R> = Array(this.size, { index -> mapper(this[index], index) })