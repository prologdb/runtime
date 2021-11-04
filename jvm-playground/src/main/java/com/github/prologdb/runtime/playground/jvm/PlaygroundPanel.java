package com.github.prologdb.runtime.playground.jvm;

import com.github.prologdb.async.LazySequence;
import com.github.prologdb.parser.ModuleDeclaration;
import com.github.prologdb.parser.Reporting;
import com.github.prologdb.parser.lexer.Lexer;
import com.github.prologdb.parser.lexer.LineEndingNormalizer;
import com.github.prologdb.parser.parser.DefaultModuleSourceFileVisitor;
import com.github.prologdb.parser.parser.ParseResult;
import com.github.prologdb.parser.parser.PrologParser;
import com.github.prologdb.parser.source.SourceUnit;
import com.github.prologdb.runtime.PrologRuntimeEnvironment;
import com.github.prologdb.runtime.PrologRuntimeException;
import com.github.prologdb.runtime.module.Module;
import com.github.prologdb.runtime.playground.jvm.editor.PrologEditorPanel;
import com.github.prologdb.runtime.playground.jvm.persistence.PlaygroundState;
import com.github.prologdb.runtime.proofsearch.ReadWriteAuthorization;
import com.github.prologdb.runtime.query.Query;
import com.github.prologdb.runtime.stdlib.StandardLibraryModuleLoader;
import com.github.prologdb.runtime.unification.Unification;

import javax.swing.*;
import java.awt.*;
import java.awt.event.KeyEvent;
import java.awt.event.KeyListener;
import java.text.ParseException;

import static java.util.Objects.requireNonNull;
import static java.util.stream.Collectors.joining;

public class PlaygroundPanel {

    private JPanel panel = new JPanel(new BorderLayout());
    private PrologEditorPanel knowledgeBaseEditorPanel;
    private QueryPanel queryPanel;
    private SolutionExplorerPanel solutionExplorerPanel;

    private PrologParser parser = new PrologParser();

    /**
     * Is set back to false in {@link #assureKnowledgeBaseIsUpToDate()}.
     */
    private boolean knowledgeBaseChangeIndicator = false;

    private PrologRuntimeEnvironment runtimeEnvironment = null;

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

    private void assureKnowledgeBaseIsUpToDate() throws ParseException, PrologRuntimeException
    {
        if (runtimeEnvironment != null && !knowledgeBaseChangeIndicator) {
            return;
        }
        Lexer lexer = new Lexer(
            new SourceUnit("knowledge_base"),
            new LineEndingNormalizer(
                new CharacterIterable(
                    knowledgeBaseEditorPanel.getCodeAsString()
                ).iterator()
            )
        );

        long parseStart = System.currentTimeMillis();
        ParseResult<? extends Module> result = parser.parseSourceFile(
            lexer,
            new DefaultModuleSourceFileVisitor(new ModuleDeclaration("user", null))
        );
        solutionExplorerPanel.setParseTime(System.currentTimeMillis() - parseStart);

        if (result.getReportings().isEmpty()) {
            runtimeEnvironment = new PrologRuntimeEnvironment(requireNonNull(result.getItem()), StandardLibraryModuleLoader.INSTANCE);
            knowledgeBaseChangeIndicator = false;
        } else {
            StringBuilder message = new StringBuilder("Failed to parse knowledge base:");
            result.getReportings().forEach(r -> {
                message.append("\n");
                message.append(r.getMessage());
                message.append(" in ");
                message.append(r.getLocation());
            });
            throw new ParseException(message.toString(), 0);
        }
    }

    private void onQueryFired(String queryCode) {
        try {
            assureKnowledgeBaseIsUpToDate();
        }
        catch (ParseException ex) {
            JOptionPane.showMessageDialog(null, ex.getMessage(), "Error in knowledge base", JOptionPane.ERROR_MESSAGE);
            return;
        } catch (PrologRuntimeException ex) {
            JOptionPane.showMessageDialog(null, ex.getMessage(), "Failed to load user knowledge base", JOptionPane.ERROR_MESSAGE);
            return;
        }

        Lexer queryLexer = new Lexer(
            new SourceUnit("query"),
            new LineEndingNormalizer(
                new CharacterIterable(queryCode).iterator()
            )
        );
        ParseResult<Query> queryParseResult = parser.parseQuery(queryLexer, runtimeEnvironment.getRootModule().getLocalOperators());
        if (!queryParseResult.getReportings().isEmpty() || queryParseResult.getItem() == null) {
            JOptionPane.showMessageDialog(
                null,
                "Errors in query:\n" + queryParseResult.getReportings().stream().map(Reporting::toString).collect(joining(" \n")),
                "Error in query",
                JOptionPane.ERROR_MESSAGE
            );
            return;
        }

        LazySequence<Unification> solutions = runtimeEnvironment.fulfill(queryParseResult.getItem(), ReadWriteAuthorization.INSTANCE);

        solutionExplorerPanel.setSolutions(solutions, runtimeEnvironment.getRootModule().getLocalOperators());
        solutionExplorerPanel.showNextSolution();
        this.panel.revalidate();
        this.panel.repaint();
    }
}
