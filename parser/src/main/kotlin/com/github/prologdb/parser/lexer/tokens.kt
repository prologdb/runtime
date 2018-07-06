package com.github.prologdb.parser.lexer

import com.github.prologdb.parser.source.SourceLocationRange

enum class TokenType {
    IDENTIFIER,
    OPERATOR,
    NUMERIC_LITERAL,
    STRING_LITERAL,
    ATOM_LITERAL
}

/**
 * The different operators
 * *Important:* comments (see [SINGLE_LINE_COMMENT_SINGALS], [MULTI_LINE_COMMENT_SIGNALS])
 * take precedence over operators.
 */
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
    NEGATED_UNIFY("\\="),
    DOUBLE_QUOTE("\""),
    SINGLE_QUOTE("'"),
    BACKTICK("`"),
    NON_PROVABLE("\\+"),

    TERM_GREATER_THAN("@>"),
    TERM_GREATER_THAN_OR_EQUAL("@>="),
    TERM_LESS_THAN("@<"),
    TERM_LESS_THAN_OR_EQUAL("@=<"),
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

class StringLiteralToken(val content: String, location: SourceLocationRange) : Token(TokenType.STRING_LITERAL, location) {
    override fun equals(other: Any?): Boolean {
        if (this === other) return true
        if (javaClass != other?.javaClass) return false

        other as StringLiteralToken

        if (content != other.content) return false

        return true
    }

    override fun hashCode(): Int {
        return content.hashCode()
    }

    override fun toString() = "string literal"
}

class AtomLiteralToken(val name: String, location: SourceLocationRange) : Token(TokenType.ATOM_LITERAL, location) {
    override fun equals(other: Any?): Boolean {
        if (this === other) return true
        if (javaClass != other?.javaClass) return false

        other as AtomLiteralToken

        if (name != other.name) return false

        return true
    }

    override fun hashCode(): Int {
        return name.hashCode()
    }

    override fun toString() = "atom literal"
}

/** The escape character in strings */
val ESCAPE_CHARACTER: Char = '\\'

/** Escape sequences in strings, e.g. \n to linefeed */
val ESCAPE_SEQUENCES: Map<Char, Char> = mapOf(
    'a' to '\u0007',
    'b' to '\b',
    'e' to '\u001B',
    'n' to '\n',
    'r' to '\r',
    't' to '\t',
    'v' to '\u000B'
)

/**
 * Any of these character sequences introduces a single line comment:
 * the rest of the line is entirely ignored by the lexer
 */
val SINGLE_LINE_COMMENT_SINGALS: List<CharSequence> = listOf(
    "%"
)

/**
 * Multiline comments can be started by each _key_ character sequence of
 * this map. When such a comment is found, the comment will end when the
 * _value_ of the key that started it appears. E.g.:
 * Entry `"/*" to "*/"` means: multiline comments can be started with `/*` and ended
 * by `*/`
 */
val MULTI_LINE_COMMENT_SIGNALS: Map<CharSequence, CharSequence> = mapOf(
    "/*" to "*/"
)