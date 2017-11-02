package com.github.tmarsteel.ktprolog.playground.jvm.editor;

import org.fife.ui.rsyntaxtextarea.RSyntaxTextArea;
import org.fife.ui.rtextarea.RTextScrollPane;

import javax.swing.*;
import java.awt.*;
import java.awt.event.KeyListener;

public class PrologEditorPanel {

    private JPanel panel = new JPanel(new BorderLayout());
    private RSyntaxTextArea textArea;
    private RTextScrollPane textScrollPane;

    public PrologEditorPanel() {
        initComponents();
    }

    public void setShowLineNumbers(boolean is) {
        textScrollPane.setLineNumbersEnabled(is);
    }

    public JPanel asJPanel() {
        return panel;
    }

    public String getCodeAsString() {
        return textArea.getText();
    }

    public void addKeyListener(KeyListener l) {
        textArea.addKeyListener(l);
    }

    public void removeKeyListener(KeyListener l) {
        textArea.removeKeyListener(l);
    }

    private void initComponents() {
        textArea = new RSyntaxTextArea(7, 60);
        textScrollPane = new RTextScrollPane(textArea);
        panel.add(textScrollPane, BorderLayout.CENTER);

        // textArea.setSyntaxEditingStyle("text/prolog");
    }
}
