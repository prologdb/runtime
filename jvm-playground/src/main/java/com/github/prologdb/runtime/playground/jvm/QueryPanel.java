package com.github.prologdb.runtime.playground.jvm;

import java.awt.BorderLayout;
import java.awt.Component;
import java.awt.Font;
import java.awt.event.ActionEvent;
import java.awt.event.KeyEvent;
import java.awt.event.KeyListener;
import java.util.EventListener;

import javax.swing.JLabel;
import javax.swing.JPanel;
import javax.swing.SwingUtilities;
import javax.swing.event.EventListenerList;

import com.github.prologdb.parser.parser.PrologParser;
import com.github.prologdb.parser.source.SourceUnit;
import com.github.prologdb.runtime.playground.jvm.editor.PrologEditorPanel;

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

    public String getCodeAsString() {
        return queryEditorPanel.getCodeAsString();
    }

    public void setCodeAsString(String code) {
        queryEditorPanel.setCodeAsString(code);
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

    public interface QueryFiredListener extends EventListener {
        void onQueryFired(QueryPanel originPanel, String queryCode);
    }

    private class QueryTextAreaKeyListener implements KeyListener {
        @Override
        public void keyTyped(KeyEvent e) {

        }

        @Override
        public void keyPressed(KeyEvent e) {
            if (e.getKeyCode() == KeyEvent.VK_ENTER && (e.getModifiers() & ActionEvent.CTRL_MASK) != 0) {
                SwingUtilities.invokeLater(() -> {
                    String queryCode = queryEditorPanel.getCodeAsString();
                    for (QueryFiredListener l : queryFiredListeners.getListeners(QueryFiredListener.class)) {
                        l.onQueryFired(QueryPanel.this, queryCode);
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