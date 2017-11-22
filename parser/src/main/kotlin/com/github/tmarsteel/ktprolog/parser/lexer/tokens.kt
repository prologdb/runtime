package com.github.tmarsteel.ktprolog.parser.lexer

import com.github.tmarsteel.ktprolog.parser.parser.InternalParserError
import com.github.tmarsteel.ktprolog.parser.source.SourceLocationRange

enum class TokenType {
    IDENTIFIER,
    OPERATOR,
    NUMERIC_LITERAL
}

enum class Operator(val text: String) {
    PARENT_OPEN("("),
    PARENT_CLOSE(")"),
    BRACKET_OPEN("["),
    BRACKET_CLOSE("]"),
    FULL_STOP("."),
    SEMICOLON(";"),
    COMMA(","),
    HEAD_QUERY_SEPARATOR(":-"),
    HEAD_TAIL_SEPARATOR("|"),
    QUERY("?-"),
    PLUS("+"),
    MINUS("-"),
    DOUBLE_TIMES("**"),
    TIMES("*"),
    DIVIDE("/"),
    POWER("^"),
    TERM_EQUALS("=="),
    LESS_THAN("<"),
    LESS_THAN_OR_EQUAL("=<"),
    ARITHMETIC_NOT_EQUALS("=\\="),
    UNIFY("="),
    GREATER_THAN_OR_EQUAL(">="),
    GREATER_THAN(">"),
    TERM_NOT_EQUALS("\\=="),
    NEGATED_UNIFY("\\=")
}

/**
 * The precedence of the operator in expressions; less means higher precedence
 */
val OperatorToken.precedence: Int
    get() = when(operator) {
        Operator.COMMA     -> 50
        Operator.SEMICOLON -> 100
        else      -> throw InternalParserError("Precedence is not defined for operator $operator")
    }

val DECIMAL_SEPARATOR: Char = '.'

sealed class Token(val type: TokenType, val location: SourceLocationRange)

class IdentifierToken(val textContent: String, location: SourceLocationRange) : Token(TokenType.IDENTIFIER, location) {
    override fun equals(other: Any?): Boolean {
        if (this === other) return true
        if (other !is IdentifierToken) return false

        if (textContent != other.textContent) return false

        return true
    }

    override fun hashCode(): Int {
        return textContent.hashCode()
    }

    override fun toString() = "identifier $textContent"
}
class OperatorToken(val operator: Operator, location: SourceLocationRange): Token(TokenType.OPERATOR, location) {
    override fun equals(other: Any?): Boolean {
        if (this === other) return true
        if (other !is OperatorToken) return false

        if (operator != other.operator) return false

        return true
    }

    override fun hashCode(): Int {
        return operator.hashCode()
    }

    override fun toString() = "operator $operator"
}
class NumericLiteralToken(val number: Number, location: SourceLocationRange): Token(TokenType.NUMERIC_LITERAL, location) {
    override fun equals(other: Any?): Boolean {
        if (this === other) return true
        if (other !is NumericLiteralToken) return false

        if (number != other.number) return false

        return true
    }

    override fun hashCode(): Int {
        return number.hashCode()
    }

    override fun toString() = "number"
}

