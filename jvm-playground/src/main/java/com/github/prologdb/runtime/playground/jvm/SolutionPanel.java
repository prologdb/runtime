package com.github.prologdb.runtime.playground.jvm;

import java.awt.Color;
import java.awt.Component;
import java.awt.Font;

import javax.swing.BoxLayout;
import javax.swing.JLabel;
import javax.swing.JPanel;
import javax.swing.border.EmptyBorder;

import com.github.prologdb.runtime.knowledge.library.OperatorRegistry;
import com.github.prologdb.runtime.unification.Unification;

public class SolutionPanel {

    private static final Font FONT = new Font(Font.MONOSPACED, Font.PLAIN,12);
    private static final Color BG_EVEN = new Color(0xB0, 0xB0, 0x00, 0x50);
    private static final Color BG_ODD  = new Color(0xB0, 0x30, 0x00, 0x50);

    private JPanel panel = new JPanel();
    private final Unification solution;
    private final OperatorRegistry displayOperators;

    public SolutionPanel(Unification solution, OperatorRegistry displayOperators) {
        this.solution = solution;
        this.displayOperators = displayOperators;

        initComponents();
    }

    public JPanel asJPanel() {
        return panel;
    }

    /**
     * Sets the index of this solution in the set it was obtained from. Enables alternating BG colors.
     * @param index
     */
    public void setIndex(int index) {
        if (index % 2 == 0) {
            panel.setBackground(BG_EVEN);
        } else {
            panel.setBackground(BG_ODD);
        }

        panel.repaint();
    }

    private void initComponents() {
        panel.setLayout(new BoxLayout(panel, BoxLayout.PAGE_AXIS));
        panel.setAlignmentX(Component.LEFT_ALIGNMENT);
        panel.setBorder(new EmptyBorder(5, 5, 5, 5));

        solution.getVariableValues().getValues().forEach(varAndValue -> {
            JLabel label = new JLabel(varAndValue.getFirst().getName() + " = " + varAndValue.getSecond().toStringUsingOperatorNotations(displayOperators));
            label.setFont(FONT);
            panel.add(label);
        });
    }
}
