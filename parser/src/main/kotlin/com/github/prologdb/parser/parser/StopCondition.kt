package com.github.prologdb.parser.parser

import com.github.prologdb.parser.lexer.Operator
import com.github.prologdb.parser.lexer.OperatorToken
import com.github.prologdb.parser.lexer.Token
import com.github.prologdb.parser.sequence.TransactionalSequence

interface StopCondition {
    /**
     * Does not alter the current position of [tokens]. If lookahead is
     * required, the sequence has to be reset back to its original state
     * by [shouldStop] (using [TransactionalSequence.mark] and [TransactionalSequence.rollback]).
     */
    fun shouldStop(tokens: TransactionalSequence<Token>): Boolean

    /**
     * Description of what this condition tests. See e.g. [FULL_STOP].
     */
    val description: String

    companion object {
        fun atAnyOf(vararg operators: Operator): StopCondition = object : StopCondition {
            private val operatorSet = operators.toSet()

            override fun shouldStop(tokens: TransactionalSequence<Token>): Boolean {
                val token = tokens.peek() ?: return false
                if (token !is OperatorToken) {
                    return false
                }

                return token.operator in operatorSet
            }

            override val description = if (operators.size == 1) "operator ${operators.single().text}" else {
                "any of " + operators.joinToString(separator = ", ", transform = { "operator ${it.text}" })
            }

            override fun toString() = "stop at $description"
        }

        val STOP_AT_EOF: StopCondition = object : StopCondition {
            override fun shouldStop(tokens: TransactionalSequence<Token>): Boolean {
                return !tokens.hasNext()
            }

            override val description = "EOF"

            override fun toString() = "stop at $description"
        }

        fun <T : Any> TransactionalSequence<T>.peek(): T? {
            if (!hasNext()) {
                return null
            }

            mark()
            val item = next()
            rollback()

            return item
        }
    }
}