package com.github.tmarsteel.ktprolog.playground.jvm;

import com.github.tmarsteel.ktprolog.parser.ParseResult;
import com.github.tmarsteel.ktprolog.parser.ParsedQuery;
import com.github.tmarsteel.ktprolog.parser.lexer.Lexer;
import com.github.tmarsteel.ktprolog.parser.lexer.LineEndingNormalizer;
import com.github.tmarsteel.ktprolog.parser.lexer.Operator;
import com.github.tmarsteel.ktprolog.parser.lexer.OperatorToken;
import com.github.tmarsteel.ktprolog.parser.parser.PrologParser;
import com.github.tmarsteel.ktprolog.parser.source.SourceUnit;
import com.github.tmarsteel.ktprolog.playground.jvm.editor.PrologEditorPanel;

import javax.swing.*;
import javax.swing.event.EventListenerList;
import java.awt.*;
import java.awt.event.ActionEvent;
import java.awt.event.KeyEvent;
import java.awt.event.KeyListener;
import java.util.EventListener;

public class QueryPanel {

    private JPanel panel;
    private PrologEditorPanel queryEditorPanel;

    private EventListenerList queryFiredListeners = new EventListenerList();

    private PrologParser parser = new PrologParser();
    private SourceUnit parsingSourceUnit = new SourceUnit("query");

    public QueryPanel() {
        initComponents();
    }

    public PrologParser getParser() {
        return parser;
    }

    public QueryPanel setParser(PrologParser parser) {
        this.parser = parser;
        return this;
    }

    public SourceUnit getParsingSourceUnit() {
        return parsingSourceUnit;
    }

    public QueryPanel setParsingSourceUnit(SourceUnit parsingSourceUnit) {
        this.parsingSourceUnit = parsingSourceUnit;
        return this;
    }

    public JPanel asJPanel() {
        return panel;
    }

    public void addQueryFiredListener(QueryFiredListener l) {
        queryFiredListeners.add(QueryFiredListener.class, l);
    }

    public void removeQueryFiredListener(QueryFiredListener l) {
        queryFiredListeners.remove(QueryFiredListener.class, l);
    }

    private void initComponents() {
        queryEditorPanel = new PrologEditorPanel();
        queryEditorPanel.setShowLineNumbers(false);
        queryEditorPanel.addKeyListener(new QueryTextAreaKeyListener());

        JLabel promptLabel = new JLabel("?- ");
        promptLabel.setFont(new Font(Font.MONOSPACED, Font.PLAIN, 12));
        JPanel promptPanel = new JPanel(new BorderLayout());
        promptPanel.add(promptLabel, BorderLayout.NORTH);

        JLabel fireHintLabel = new JLabel("Strg + Enter to fire the query ");
        fireHintLabel.setFont(fireHintLabel.getFont().deriveFont(Font.ITALIC, 10.5f));
        fireHintLabel.setAlignmentX(Component.RIGHT_ALIGNMENT);
        JPanel fireHintPanel = new JPanel(new BorderLayout());
        fireHintPanel.add(fireHintLabel, BorderLayout.EAST);

        panel = new JPanel(new BorderLayout());
        panel.add(promptPanel, BorderLayout.WEST);
        panel.add(queryEditorPanel.asJPanel(), BorderLayout.CENTER);
        panel.add(fireHintPanel, BorderLayout.SOUTH);
    }

    private ParseResult<ParsedQuery> parseQuery() {
        Lexer lexer = new Lexer(
            parsingSourceUnit,
            new LineEndingNormalizer(
                new CharacterIterable(
                    queryEditorPanel.getCodeAsString()
                ).iterator()
            )
        );

        return parser.parseQuery(lexer, t -> t instanceof OperatorToken && ((OperatorToken) t).getOperator() == Operator.FULL_STOP);
    }

    public interface QueryFiredListener extends EventListener {
        public void onQueryFired(QueryPanel originPanel, ParsedQuery query);
    }

    private class QueryTextAreaKeyListener implements KeyListener {
        @Override
        public void keyTyped(KeyEvent e) {

        }

        @Override
        public void keyPressed(KeyEvent e) {
            if (e.getKeyCode() == KeyEvent.VK_ENTER && (e.getModifiers() & ActionEvent.CTRL_MASK) != 0) {
                // CTRL + ENTER pressed
                ParseResult<ParsedQuery> queryParseResult = parseQuery();
                SwingUtilities.invokeLater(() -> {
                    if (queryParseResult.getReportings().isEmpty()) {
                        for (QueryFiredListener l : queryFiredListeners.getListeners(QueryFiredListener.class)) {
                            l.onQueryFired(QueryPanel.this, queryParseResult.getItem());
                        }
                    } else {
                        StringBuilder message = new StringBuilder("Failed to parse query:\n");
                        queryParseResult.getReportings().forEach(r -> message.append(r.getMessage() + "\n"));
                        JOptionPane.showMessageDialog(null, message.toString(), "Parse Error", JOptionPane.ERROR_MESSAGE);
                    }
                });
                e.consume();
            }
        }

        @Override
        public void keyReleased(KeyEvent e) {

        }
    }
}
