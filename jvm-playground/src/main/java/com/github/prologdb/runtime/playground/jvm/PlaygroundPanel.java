package com.github.prologdb.runtime.playground.jvm;

import com.github.prologdb.parser.ParsedQuery;
import com.github.prologdb.parser.Reporting;
import com.github.prologdb.parser.lexer.Lexer;
import com.github.prologdb.parser.lexer.LineEndingNormalizer;
import com.github.prologdb.parser.parser.ParseResult;
import com.github.prologdb.parser.parser.PrologParser;
import com.github.prologdb.parser.source.SourceUnit;
import com.github.prologdb.runtime.RandomVariableScope;
import com.github.prologdb.runtime.knowledge.DefaultKnowledgeBase;
import com.github.prologdb.runtime.knowledge.library.Library;
import com.github.prologdb.runtime.playground.jvm.editor.PrologEditorPanel;
import com.github.prologdb.runtime.unification.VariableBucket;

import javax.swing.*;
import java.awt.*;
import java.awt.event.KeyEvent;
import java.awt.event.KeyListener;
import java.text.ParseException;

import static java.util.stream.Collectors.joining;

;

public class PlaygroundPanel {

    private JPanel panel = new JPanel(new BorderLayout());
    private PrologEditorPanel knowledgeBaseEditorPanel;
    private QueryPanel queryPanel;
    private SolutionExplorerPanel solutionExplorerPanel;

    private PrologParser parser = new PrologParser();

    /**
     * Is set to true whenever the code in the knowledge base changes. Is set back to false in {@link #assureKnowledgeBaseIsUpToDate()}.
     */
    private boolean knowledgeBaseChangeIndicator = false;
    private DefaultKnowledgeBase knowledgeBase = null;

    public PlaygroundPanel() {
        initComponents();
    }

    public PrologParser getParser() {
        return parser;
    }

    public PlaygroundPanel setParser(PrologParser parser) {
        this.parser = parser;
        this.queryPanel.setParser(parser);
        return this;
    }

    public JPanel asJPanel() {
        return panel;
    }

    private void initComponents() {
        knowledgeBaseEditorPanel = new PrologEditorPanel();
        queryPanel = new QueryPanel();
        solutionExplorerPanel = new SolutionExplorerPanel();

        JSplitPane queryAndResultsSplitter = new JSplitPane(JSplitPane.VERTICAL_SPLIT);
        queryAndResultsSplitter.add(queryPanel.asJPanel());
        queryAndResultsSplitter.add(solutionExplorerPanel.asJPanel());
        queryAndResultsSplitter.setBorder(null);

        JSplitPane baseAndQuerySplitter = new JSplitPane(JSplitPane.HORIZONTAL_SPLIT);
        baseAndQuerySplitter.add(knowledgeBaseEditorPanel.asJPanel());
        baseAndQuerySplitter.add(queryAndResultsSplitter);
        baseAndQuerySplitter.setBorder(null);

        panel.add(baseAndQuerySplitter, BorderLayout.CENTER);

        // -- listeners
        knowledgeBaseEditorPanel.addKeyListener(new KeyListener() {
            @Override
            public void keyTyped(KeyEvent e) {}

            @Override
            public void keyPressed(KeyEvent e) { }

            @Override
            public void keyReleased(KeyEvent e) {
                knowledgeBaseChangeIndicator = true;
            }
        });

        queryPanel.addQueryFiredListener((panel, query) -> onQueryFired(query));
    }

    private void assureKnowledgeBaseIsUpToDate() throws ParseException {
        if (knowledgeBaseChangeIndicator || knowledgeBase == null) {
            DefaultKnowledgeBase newKnowledgeBase = new DefaultKnowledgeBase();
            Lexer lexer = new Lexer(
                new SourceUnit("knowledge base"),
                new LineEndingNormalizer(
                    new CharacterIterable(
                        knowledgeBaseEditorPanel.getCodeAsString()
                    ).iterator()
                )
            );
            ParseResult<? extends Library> result = parser.parseLibrary(lexer, newKnowledgeBase::getLibrary);

            if (result.getReportings().isEmpty()) {
                knowledgeBase = newKnowledgeBase;
                knowledgeBaseChangeIndicator = false;
            } else {
                StringBuilder message = new StringBuilder("Failed to parse knowledge base:");
                result.getReportings().forEach(r -> { message.append("\n"); message.append(r.getMessage() + " in " + r.getLocation()); });
                throw new ParseException(message.toString(), 0);
            }
        }
    }

    private void onQueryFired(String queryCode) {
        try {
            assureKnowledgeBaseIsUpToDate();
        }
        catch (ParseException ex) {
            JOptionPane.showMessageDialog(null, ex.getMessage(), "Error in knowledge base", JOptionPane.ERROR_MESSAGE);
            return;
        }

        Lexer queryLexer = new Lexer(
            new SourceUnit("query"),
            new LineEndingNormalizer(
                new CharacterIterable(queryCode).iterator()
            )
        );
        ParseResult<ParsedQuery> queryParseResult = parser.parseQuery(queryLexer, knowledgeBase.getOperatorRegistry());
        if (!queryParseResult.getReportings().isEmpty() || queryParseResult.getItem() == null) {
            JOptionPane.showMessageDialog(
                null,
                "Errors in query:\n" + queryParseResult.getReportings().stream().map(Reporting::toString).collect(joining(" \n")),
                "Error in query",
                JOptionPane.ERROR_MESSAGE
            );
            return;
        }

        solutionExplorerPanel.setSolutions(queryParseResult.getItem().findProofWithin(knowledgeBase, new VariableBucket(), new RandomVariableScope()));
        solutionExplorerPanel.showNextSolution();
        this.panel.revalidate();
        this.panel.repaint();
    }
}
