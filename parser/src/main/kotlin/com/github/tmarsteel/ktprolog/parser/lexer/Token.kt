package com.github.tmarsteel.ktprolog.parser.lexer

import com.github.tmarsteel.ktprolog.parser.source.SourceLocationRange

enum class TokenType {
    IDENTIFIER,
    OPERATOR,
    NUMERIC_LITERAL
}

enum class Operator(val text: String) {
    PARANT_OPEN("("),
    PARANT_CLOSE(")"),
    FULL_STOP("."),
    SEMICOLON(";"),
    COMMA(","),
    HEAD_QUERY_SEPARATOR(":-")
}

val DECIMAL_SEPARATOR: Char = '.'

sealed class Token(val type: TokenType, val location: SourceLocationRange)
class IdentifierToken(val textContent: String, location: SourceLocationRange) : Token(TokenType.IDENTIFIER, location)
class OperatorToken(val operator: Operator, location: SourceLocationRange): Token(TokenType.OPERATOR, location)
class NumericLiteralToken(val number: Number, location: SourceLocationRange): Token(TokenType.NUMERIC_LITERAL, location)

