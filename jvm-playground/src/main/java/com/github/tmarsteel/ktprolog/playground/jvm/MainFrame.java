package com.github.tmarsteel.ktprolog.playground.jvm;

import javax.swing.*;
import java.awt.*;

public class MainFrame extends JFrame {
    public MainFrame() {
        super("Kt-Prolog Playground");

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
