package com.github.prologdb.runtime.playground.jvm;

import java.awt.BorderLayout;
import java.nio.file.Path;
import java.util.Optional;

import javax.swing.JFrame;
import javax.swing.WindowConstants;

public class MainFrame extends JFrame {

    private final Optional<Path> statePersistenceFile;

    public MainFrame(Optional<Path> statePersistenceFile) {
        super("PrologDB Playground");

        this.statePersistenceFile = statePersistenceFile;

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
