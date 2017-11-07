package com.github.tmarsteel.ktprolog.playground.jvm;

import com.github.tmarsteel.ktprolog.RandomVariableScope;
import com.github.tmarsteel.ktprolog.knowledge.DefaultKnowledgeBase;
import com.github.tmarsteel.ktprolog.knowledge.MutableKnowledgeBase;
import com.github.tmarsteel.ktprolog.knowledge.library.Library;
import com.github.tmarsteel.ktprolog.parser.ParseResult;
import com.github.tmarsteel.ktprolog.parser.lexer.Lexer;
import com.github.tmarsteel.ktprolog.parser.lexer.LineEndingNormalizer;
import com.github.tmarsteel.ktprolog.parser.parser.PrologParser;
import com.github.tmarsteel.ktprolog.parser.source.SourceUnit;
import com.github.tmarsteel.ktprolog.playground.jvm.editor.PrologEditorPanel;
import com.github.tmarsteel.ktprolog.query.Query;
import com.github.tmarsteel.ktprolog.unification.VariableBucket;
import kotlin.Unit;

import javax.swing.*;
import java.awt.*;
import java.awt.event.KeyEvent;
import java.awt.event.KeyListener;
import java.text.ParseException;

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
    private MutableKnowledgeBase knowledgeBase = null;

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
            MutableKnowledgeBase newKnowledgeBase = new DefaultKnowledgeBase();
            Lexer lexer = new Lexer(
                new SourceUnit("knowledge base"),
                new LineEndingNormalizer(
                    new CharacterIterable(
                        knowledgeBaseEditorPanel.getCodeAsString()
                    ).iterator()
                )
            );
            ParseResult<Library> result = parser.parseLibrary(lexer);

            if (result.getReportings().isEmpty()) {
                newKnowledgeBase.load(result.getItem());
                knowledgeBase = newKnowledgeBase;
                knowledgeBaseChangeIndicator = false;
            } else {
                StringBuilder message = new StringBuilder("Failed to parse knowledge base:");
                result.getReportings().forEach(r -> { message.append("\n"); message.append(r.getMessage() + " in " + r.getLocation().getStart()); });
                throw new ParseException(message.toString(), 0);
            }
        }
    }

    private void onQueryFired(Query query) {
        try {
            assureKnowledgeBaseIsUpToDate();
        }
        catch (ParseException ex) {
            JOptionPane.showMessageDialog(null, ex.getMessage(), "Error in knowledge base", JOptionPane.ERROR_MESSAGE);
            return;
        }

        solutionExplorerPanel.setSolutions(query.findProofWithin(knowledgeBase, new VariableBucket(), new RandomVariableScope()));
        solutionExplorerPanel.showNextSolution();
        this.panel.revalidate();
        this.panel.repaint();
    }
}
