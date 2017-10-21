fun Array<*>?.sensibleHashCode(): Int {
    if (this == null)
        return 0

    var result = 1

    for (element in this)
        result = 31 * result + if (element == null) 0 else element.hashCode()

    return result
}