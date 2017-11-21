package com.github.tmarsteel.ktprolog.parser

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

    companion object {
        fun <T> of(item: T) = ParseResult(item!!, ParseResultCertainty.MATCHED, emptySet())
    }
}