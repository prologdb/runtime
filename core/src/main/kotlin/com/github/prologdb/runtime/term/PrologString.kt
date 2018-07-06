package com.github.prologdb.runtime.term

import com.github.prologdb.runtime.ImmutableSubList
import kotlin.math.min
import com.github.prologdb.runtime.term.Integer as PrologInteger

/**
 * An implementation of String (similar to Java) that is prolog affine.
 */
open class PrologString private constructor(
    /**
     * The unicode code points that make up the string
     */
    private val data: ImmutableSubList<PrologInteger>
) : PrologList(data) {

    constructor(chars: CharArray, beginIndex: Int = 0, length: Int = chars.size) : this(
        ImmutableSubList(chars.toList().map { PrologInteger(it.toLong()) }, beginIndex, length)
    )

    constructor(str: String) : this({
        val ar = CharArray(str.length)
        for (i in 0..str.lastIndex) {
            ar[i] = str[i]
        }
        ar
    }())

    override val prologTypeName = "string"

    init {
        if (data.any { it.value < 0 || it.value > 65535 }) throw IllegalArgumentException("Prolog strings must only contain unicode values in the range [0; 65535]")
    }

    val length: Int = data.size

    /**
     * @return The character at the given index (0-based)
     * @throws ArrayIndexOutOfBoundsException If the given index is negative or exceeds this strings length
     */
    fun charAt(index: Int): Char = data[index].value.toChar()

    /**
     * Returns a string that is a substring of this string. The
     * substring begins with the character at the specified index and
     * extends to the end of this string.
     * Examples:
     *
     *     "unhappy".substring(2) returns "happy"
     *     "Harbison".substring(3) returns "bison"
     *     "emptiness".substring(9) returns "" (an empty string)
     *
     * @param      beginIndex   the beginning index, inclusive.
     * @return     the specified substring.
     * @exception  IndexOutOfBoundsException  if
     *             `beginIndex` is negative or larger than the
     *             length of this String object.
     */
    fun substring(beginIndex: Int): PrologString {
        if (beginIndex >= length) throw IndexOutOfBoundsException()

        return PrologString(data.subList(beginIndex, length))
    }

    /**
     * Returns a string that is a substring of this string. The
     * substring begins at the specified `beginIndex` and
     * extends to the character at index `endIndex - 1`.
     * Thus the length of the substring is `endIndex-beginIndex`.
     *
     * Examples:
     *
     *     "hamburger".substring(4, 8) returns "urge"
     *     "smiles".substring(1, 5) returns "mile"
     *
     * @param      fromIndex   the beginning index, inclusive.
     * @param      toIndex     the ending index, exclusive.
     * @return     the specified substring.
     * @exception  IndexOutOfBoundsException  if the
     *             `beginIndex` is negative, or
     *             `endIndex` is larger than the length of
     *             this `String` object, or
     *             `beginIndex` is larger than
     *             `endIndex`.
     */
    fun substring(fromIndex: Int, toIndex: Int): PrologString {
        return PrologString(data.subList(fromIndex, toIndex))
    }

    val characters: Iterable<Char> by lazy { PrologStringCharacterIterable(this) }

    override val variables: Set<Variable> = emptySet()

    override fun substituteVariables(mapper: (Variable) -> Term): PrologList = this

    override fun equals(other: Any?): Boolean {
        if (other is PrologString) {
            return this.data == other.data
        }
        else return super.equals(other)
    }

    override fun hashCode() = super.hashCode()

    override fun compareTo(other: Term): Int {
        when(other) {
            // variables and numbers are, by category, lesser than strings
            is Variable, is Number -> return 1

            // lexicographical order
            is PrologString -> {
                val minLength = min(this.length, other.length)
                for (charIndex in 0 until minLength) {
                    val thisChar = this.charAt(charIndex)
                    val otherChar = other.charAt(charIndex)
                    val cmp = thisChar.compareTo(otherChar)
                    if (cmp != 0) return cmp
                }
                // all chars are equal
                return 0
            }

            // everything else is, by category, greater than strings
            else -> return -1
        }
    }

    /**
     * The kotlin string representation; is lazy initialized by
     * [toKotlinString]. Is sometimes pre-set by other code when there
     * is a shortcut (e.g. substring).
     */
    private var kotlinString: String? = null

    /**
     * Returns a plain kotlin representation of this string (as apposed
     * to [toString] which returns prolog syntax that, when parsed, would
     * result in an equal string).
     */
    fun toKotlinString(): String {
        return kotlinString ?: {
            val sb = StringBuilder(length)
            characters.forEach { sb.append(it) }
            sb.toString()
        }()
    }

    override fun toString(): String {
        return '"' + (toKotlinString()
            .replace("\\", "\\\\")
            .replace("\u0007", "\\a")
            .replace("\b", "\\b")
            .replace("\u001B", "\\e")
            .replace("\n", "\\n")
            .replace("\r", "\\r")
            .replace("\t", "\\t")
            .replace("\u000B", "\\v")
            .replace("\"", "\\\"")) + '"'
    }
}


private class PrologStringCharacterIterable(val str: PrologString) : Iterable<Char> {
    override fun iterator(): Iterator<Char> {
        return object : Iterator<Char> {
            private var currentIndex: Int = 0
            override fun hasNext(): Boolean = str.length > currentIndex

            @Synchronized
            override fun next(): Char = str.charAt(currentIndex++)
        }
    }
}