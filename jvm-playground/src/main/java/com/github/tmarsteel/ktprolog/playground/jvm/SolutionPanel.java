package com.github.tmarsteel.ktprolog.playground.jvm;

import com.github.tmarsteel.ktprolog.unification.Unification;

import javax.swing.*;
import java.awt.*;

public class SolutionPanel {

    private static Font font = new Font(Font.MONOSPACED, Font.PLAIN,12);

    private JPanel panel = new JPanel();
    private final Unification solution;

    public SolutionPanel(Unification solution) {
        this.solution = solution;

        initComponents();
    }

    public JPanel asJPanel() {
        return panel;
    }

    private void initComponents() {
        panel.setLayout(new BoxLayout(panel, BoxLayout.LINE_AXIS));
        panel.setAlignmentX(Component.LEFT_ALIGNMENT);

        solution.getVariableValues().getValues().forEach(varAndValue -> {
            JLabel label = new JLabel(varAndValue.getFirst().getName() + " = " + varAndValue.getSecond().toString());
            label.setFont(font);
            panel.add(label);
        });
    }
}
