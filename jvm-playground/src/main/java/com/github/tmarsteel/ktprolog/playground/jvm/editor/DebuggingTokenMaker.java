package com.github.tmarsteel.ktprolog.playground.jvm.editor;

import org.fife.ui.rsyntaxtextarea.*;

import javax.swing.*;
import javax.swing.text.Segment;
import java.util.ArrayList;
import java.util.List;

public class DebuggingTokenMaker extends AbstractTokenMaker {
    private TokenMaker debuggedTokenMaker = new PrologTokenMaker();

    @Override
    public TokenMap getWordsToHighlight() {
        return null;
    }

    @Override
    public void addNullToken() {
        debuggedTokenMaker.addNullToken();
    }

    @Override
    public void addToken(char[] array, int start, int end, int tokenType, int startOffset) {
        debuggedTokenMaker.addToken(array, start, end, tokenType, startOffset);
    }

    @Override
    public int getClosestStandardTokenTypeForInternalType(int type) {
        return debuggedTokenMaker.getClosestStandardTokenTypeForInternalType(type);
    }

    @Override
    public boolean getCurlyBracesDenoteCodeBlocks(int languageIndex) {
        return debuggedTokenMaker.getCurlyBracesDenoteCodeBlocks(languageIndex);
    }

    @Override
    public int getLastTokenTypeOnLine(Segment text, int initialTokenType) {
        return debuggedTokenMaker.getLastTokenTypeOnLine(text, initialTokenType);
    }

    @Override
    public String[] getLineCommentStartAndEnd(int languageIndex) {
        return debuggedTokenMaker.getLineCommentStartAndEnd(languageIndex);
    }

    @Override
    public Action getInsertBreakAction() {
        return debuggedTokenMaker.getInsertBreakAction();
    }

    @Override
    public boolean getMarkOccurrencesOfTokenType(int type) {
        return debuggedTokenMaker.getMarkOccurrencesOfTokenType(type);
    }

    @Override
    public OccurrenceMarker getOccurrenceMarker() {
        return debuggedTokenMaker.getOccurrenceMarker();
    }

    @Override
    public boolean getShouldIndentNextLineAfter(Token token) {
        return debuggedTokenMaker.getShouldIndentNextLineAfter(token);
    }

    @Override
    public Token getTokenList(Segment text, int initialTokenType, int startOffset) {
        Token firstToken = debuggedTokenMaker.getTokenList(text, initialTokenType, startOffset);

        List<Token> tokens = new ArrayList<>();
        TokenImpl current = (TokenImpl) firstToken;
        while (current != null) {
            String lexeme;
            try {
                lexeme = current.getLexeme();
            }
            catch (Exception ex) {
                lexeme = ex.getClass().getSimpleName();
            }

            System.out.println(
                "Token [ offset = " + current.getOffset() + ", textOffset = " + current.textOffset +
                ", textCount = " + current.textCount + ", hasNext = " + (current.getNextToken() != null) +
                ", lexeme = " + lexeme + " ]"
            );
            current = (TokenImpl) current.getNextToken();
        }

        System.out.println();

        return firstToken;
    }

    @Override
    public boolean isIdentifierChar(int languageIndex, char ch) {
        return debuggedTokenMaker.isIdentifierChar(languageIndex, ch);
    }

    @Override
    public boolean isMarkupLanguage() {
        return debuggedTokenMaker.isMarkupLanguage();
    }
}
