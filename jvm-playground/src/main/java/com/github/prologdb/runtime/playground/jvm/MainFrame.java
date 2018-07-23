package com.github.prologdb.runtime.playground.jvm;

import java.awt.BorderLayout;

import javax.swing.JFrame;
import javax.swing.WindowConstants;

import com.github.prologdb.runtime.playground.jvm.persistence.PlaygroundStatePersistenceService;

public class MainFrame extends JFrame {

    private final PlaygroundStatePersistenceService statePersistenceService;

    public MainFrame(PlaygroundStatePersistenceService statePersistenceService) {
        super("PrologDB Playground");

        this.statePersistenceService = statePersistenceService;

        initComponents();
    }

    private void initComponents() {
        setLayout(new BorderLayout());

        getContentPane().add(new PlaygroundPanel().asJPanel(), BorderLayout.CENTER);

        setDefaultCloseOperation(WindowConstants.EXIT_ON_CLOSE);
        pack();
        setLocationRelativeTo(null);
    }
}
