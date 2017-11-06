package com.github.tmarsteel.ktprolog.playground.jvm.editor;

import com.github.tmarsteel.ktprolog.parser.lexer.Lexer;
import com.github.tmarsteel.ktprolog.parser.lexer.OperatorToken;
import com.github.tmarsteel.ktprolog.parser.source.SourceLocation;
import com.github.tmarsteel.ktprolog.parser.source.SourceUnit;
import com.github.tmarsteel.ktprolog.playground.jvm.CharacterIterable;
import org.fife.ui.rsyntaxtextarea.AbstractTokenMaker;
import org.fife.ui.rsyntaxtextarea.Token;
import org.fife.ui.rsyntaxtextarea.TokenMap;

import javax.swing.text.Segment;
import java.util.ArrayList;
import java.util.List;

public class PrologLexerTokenMaker extends AbstractTokenMaker {
    @Override
    public TokenMap getWordsToHighlight() {
        return null;
    }

    @Override
    public Token getTokenList(Segment segment, int initialTokenType, int startOffset) {
        Lexer lexer = new Lexer(
            new CharacterIterable(segment.toString()).iterator(),
            new SourceLocation(
                new SourceUnit("mock source unit"),
                1,
                0,
                startOffset
            )
        );

        List<com.github.tmarsteel.ktprolog.parser.lexer.Token> prologTokens = new ArrayList<>();
        lexer.forEachRemaining(prologTokens::add);

        int segmentStartIndexInDocument = startOffset;
        int segmentEndIndexInDocument = segmentStartIndexInDocument + segment.count - 1;

        prologTokens.removeIf(t -> t.getLocation().getEnd().getSourceIndex() < segmentStartIndexInDocument || t.getLocation().getStart().getSourceIndex() > segmentEndIndexInDocument);

        com.github.tmarsteel.ktprolog.parser.lexer.Token previous = null;
        resetTokenList();

        for (com.github.tmarsteel.ktprolog.parser.lexer.Token prologToken : prologTokens) {
            if (previous == null) {
                if (prologToken.getLocation().getStart().getSourceIndex() > segmentStartIndexInDocument) {
                    // leading whitespace
                    addToken(
                        segment.array,
                        segmentStartIndexInDocument,
                            segmentStartIndexInDocument + prologToken.getLocation().getStart().getSourceIndex() - 1,
                        Token.WHITESPACE,
                        startOffset
                    );
                }
            } else {
                if (previous.getLocation().getEnd().getSourceIndex() + 1 != prologToken.getLocation().getStart().getSourceIndex()) {
                    // there is whitespace in between -> add it

                    addToken(
                        segment.array,
                        previous.getLocation().getEnd().getSourceIndex(),
                        prologToken.getLocation().getStart().getSourceIndex() - 1,
                        Token.WHITESPACE,
                        startOffset
                    );
                }
            }

            int tokenType = Token.IDENTIFIER;

            switch (prologToken.getType()) {
                case OPERATOR:
                    tokenType = Token.OPERATOR;

                    if (prologToken instanceof OperatorToken) {
                        switch (((OperatorToken) prologToken).getOperator()) {
                            case PARENT_OPEN: case PARENT_CLOSE: case BRACKET_OPEN: case BRACKET_CLOSE:
                            case HEAD_QUERY_SEPARATOR: case HEAD_TAIL_SEPARATOR:
                                tokenType = Token.SEPARATOR;
                        }
                    }
                    break;

                case IDENTIFIER:
                    tokenType = Token.IDENTIFIER;
                    break;

                case NUMERIC_LITERAL:
                    tokenType = Token.LITERAL_NUMBER_DECIMAL_INT;
                    break;
            }

            addToken(
                segment.array,
                prologToken.getLocation().getStart().getSourceIndex(),
                startOffset + prologToken.getLocation().getEnd().getSourceIndex(),
                tokenType,
                prologToken.getLocation().getStart().getSourceIndex()
            );
            previous = prologToken;
        }

        // handle trailing whitespace
        if (previous != null) {
            if (previous.getLocation().getEnd().getSourceIndex() < segmentEndIndexInDocument) {
                addToken(
                    segment.array,
                    previous.getLocation().getEnd().getSourceIndex(),
                    startOffset + segment.count - 1,
                    Token.WHITESPACE,
                    previous.getLocation().getEnd().getSourceIndex()
                );
            }
        }

        if (firstToken == null) {
            if (segment.count > 0) {
                // whitespace only
                addToken(
                    segment,
                    startOffset,
                    startOffset + segment.count - 1,
                    Token.WHITESPACE,
                    startOffset
                );
            } else {
                addNullToken();
            }
        }

        return firstToken;
    }
}
