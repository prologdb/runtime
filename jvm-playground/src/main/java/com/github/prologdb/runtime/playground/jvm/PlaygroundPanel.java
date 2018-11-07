package com.github.prologdb.runtime.playground.jvm;

import java.awt.BorderLayout;
import java.awt.event.KeyEvent;
import java.awt.event.KeyListener;
import java.text.ParseException;

import javax.swing.JOptionPane;
import javax.swing.JPanel;
import javax.swing.JSplitPane;

import com.github.prologdb.async.LazySequence;
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
import com.github.prologdb.runtime.playground.jvm.persistence.PlaygroundState;
import com.github.prologdb.runtime.query.Query;
import com.github.prologdb.runtime.unification.Unification;

import static java.util.Objects.requireNonNull;
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

    /** @return the current state of the playground */
    public PlaygroundState getCurrentState() {
        PlaygroundState state = new PlaygroundState();
        state.setKnowledgeBaseText(knowledgeBaseEditorPanel.getCodeAsString());
        state.setQuery(queryPanel.getCodeAsString());

        return state;
    }

    public void setCurrentState(PlaygroundState state) {
        knowledgeBaseEditorPanel.setCodeAsString(state.getKnowledgeBaseText());
        queryPanel.setCodeAsString(state.getQuery());
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
            DefaultKnowledgeBase newKnowledgeBase = DefaultKnowledgeBase.Companion.createWithDefaults();
            Lexer lexer = new Lexer(
                new SourceUnit("knowledge base"),
                new LineEndingNormalizer(
                    new CharacterIterable(
                        knowledgeBaseEditorPanel.getCodeAsString()
                    ).iterator()
                )
            );
            ParseResult<Library> result = parser.parseLibrary("user", lexer, newKnowledgeBase.getOperators());

            if (result.getReportings().isEmpty()) {
                newKnowledgeBase.load(requireNonNull(result.getItem()));
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
        ParseResult<Query> queryParseResult = parser.parseQuery(queryLexer, knowledgeBase.getOperators());
        if (!queryParseResult.getReportings().isEmpty() || queryParseResult.getItem() == null) {
            JOptionPane.showMessageDialog(
                null,
                "Errors in query:\n" + queryParseResult.getReportings().stream().map(Reporting::toString).collect(joining(" \n")),
                "Error in query",
                JOptionPane.ERROR_MESSAGE
            );
            return;
        }

        LazySequence<Unification> solutions = knowledgeBase.fulfill(queryParseResult.getItem(), new RandomVariableScope());

        solutionExplorerPanel.setSolutions(solutions);
        solutionExplorerPanel.showNextSolution();
        this.panel.revalidate();
        this.panel.repaint();
    }
}
