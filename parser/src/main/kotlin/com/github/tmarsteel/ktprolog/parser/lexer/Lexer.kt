package com.github.tmarsteel.ktprolog.parser.lexer

import com.github.tmarsteel.ktprolog.parser.sequence.Index
import com.github.tmarsteel.ktprolog.parser.sequence.TransactionalSequence
import com.github.tmarsteel.ktprolog.parser.source.SourceLocation

interface Lexer {
    fun lex(input: TransactionalSequence<Char, SourceLocation>): TransactionalSequence<Token, Index>
}

