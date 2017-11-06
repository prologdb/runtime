package com.github.tmarsteel.ktprolog.playground.jvm.editor;

import com.github.tmarsteel.ktprolog.parser.lexer.Lexer;
import com.github.tmarsteel.ktprolog.parser.lexer.OperatorToken;
import com.github.tmarsteel.ktprolog.parser.source.SourceUnit;
import com.github.tmarsteel.ktprolog.playground.jvm.CharacterIterable;
import org.fife.ui.rsyntaxtextarea.AbstractTokenMaker;
import org.fife.ui.rsyntaxtextarea.Token;
import org.fife.ui.rsyntaxtextarea.TokenMap;

import javax.swing.text.Segment;
import java.util.ArrayList;
import java.util.List;

public class PrologTokenMaker extends AbstractTokenMaker {
    @Override
    public TokenMap getWordsToHighlight() {
        return null;
    }

    @Override
    public Token getTokenList(Segment segment, int initialTokenType, int startOffset) {
        Lexer lexer = new Lexer(
            new SourceUnit("mock source unit"),
            new CharacterIterable(new String(segment.array)).iterator()
        );

        List<com.github.tmarsteel.ktprolog.parser.lexer.Token> prologTokens = new ArrayList<>();
        lexer.forEachRemaining(prologTokens::add);

        com.github.tmarsteel.ktprolog.parser.lexer.Token previous = null;
        resetTokenList();

        for (com.github.tmarsteel.ktprolog.parser.lexer.Token prologToken : prologTokens) {
            if (previous == null) {
                if (prologToken.getLocation().getStart().getColumn() != 1) {
                    // leading whitespace
                    addToken(
                        segment,
                        startOffset,
                        startOffset + prologToken.getLocation().getStart().getColumn() - 2,
                        Token.WHITESPACE,
                        startOffset
                    );
                }
            } else {
                if (previous.getLocation().getEnd().getColumn() + 1 != prologToken.getLocation().getStart().getColumn()) {
                    // there is whitespace in between -> add it

                    addToken(
                        segment,
                        startOffset + previous.getLocation().getEnd().getColumn(),
                        startOffset + prologToken.getLocation().getStart().getColumn() - 2,
                        Token.WHITESPACE,
                        startOffset + previous.getLocation().getEnd().getColumn()
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
                segment,
                startOffset + prologToken.getLocation().getStart().getColumn() - 1,
                startOffset + prologToken.getLocation().getEnd().getColumn() - 1,
                tokenType,
                startOffset + prologToken.getLocation().getStart().getColumn() - 1
            );
            previous = prologToken;
        }

        // handle trailing whitespace
        if (previous != null) {
            if (previous.getLocation().getEnd().getColumn() < segment.count) {
                addToken(
                    segment,
                    startOffset + previous.getLocation().getEnd().getColumn(),
                    startOffset + segment.count - 1,
                    Token.WHITESPACE,
                    startOffset + previous.getLocation().getEnd().getColumn()
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
