package com.github.prologdb.parser.parser

import com.github.prologdb.parser.Reporting

enum class ParseResultCertainty {
    NOT_RECOGNIZED,
    MATCHED;
}

class ParseResult<out T>(
        val item: T?,
        val certainty: ParseResultCertainty,
        val reportings: Collection<Reporting>
) {
    val isSuccess = certainty >= ParseResultCertainty.MATCHED && item != null

    fun <E : Any> map(mapper: (T) -> E): ParseResult<E> = ParseResult(item?.let(mapper), certainty, reportings)

    override fun toString(): String {
        val reportingLevelCounts = reportings
            .groupingBy { it.level }
            .eachCount()
            .entries
            .takeUnless { it.isEmpty() }
            ?.joinToString(
                prefix = "(",
                transform = { (level, count) -> "$count ${level.name.lowercase()}" },
                postfix = ")",
            )
            ?: "(no reportings)"

        fun describe(item: Any?): String = when (item) {
            is Pair<*, *> -> "Pair<${describe(item.first)}, ${describe(item.second)}>"
            is Triple<*, *, *> -> "Triple<${describe(item.first)}, ${describe(item.second)}, ${describe(item.third)}>"
            else -> item?.let { it::class.java }?.simpleName ?: "<no item>"
        }

        return "$certainty ${describe(item)} $reportingLevelCounts"
    }

    companion object {
        fun <T> of(item: T) = ParseResult(item!!, ParseResultCertainty.MATCHED, emptySet())
    }
}