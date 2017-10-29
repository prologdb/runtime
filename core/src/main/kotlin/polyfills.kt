internal fun Array<*>?.sensibleHashCode(): Int {
    if (this == null)
        return 0

    var result = 1

    for (element in this)
        result = 31 * result + if (element == null) 0 else element.hashCode()

    return result
}

internal inline fun <T, reified R>Array<T>.mapToArray(mapper: (T) -> R): Array<out R> = Array(this.size, { index -> mapper(this[index]) })

/* JS polyfills: on the JVM, the native methods will win in the overload; on JS, those will grip */

internal fun Char.isDigit(): Boolean {
    return this in '0'..'9'
}

internal fun Char.isUpperCase(): Boolean {
    return this.toUpperCase() == this
}