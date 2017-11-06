package com.github.tmarsteel.ktprolog.playground.jvm;

import com.github.tmarsteel.ktprolog.unification.Unification;

import javax.swing.*;
import javax.swing.border.EmptyBorder;
import java.awt.*;

public class SolutionPanel {

    private static final Font FONT = new Font(Font.MONOSPACED, Font.PLAIN,12);
    private static final Color BG_EVEN = new Color(0xB0, 0xB0, 0x00, 0x50);
    private static final Color BG_ODD  = new Color(0xB0, 0x30, 0x00, 0x50);

    private JPanel panel = new JPanel();
    private final Unification solution;

    public SolutionPanel(Unification solution) {
        this.solution = solution;

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
            JLabel label = new JLabel(varAndValue.getFirst().getName() + " = " + varAndValue.getSecond().toString());
            label.setFont(FONT);
            panel.add(label);
        });
    }
}
