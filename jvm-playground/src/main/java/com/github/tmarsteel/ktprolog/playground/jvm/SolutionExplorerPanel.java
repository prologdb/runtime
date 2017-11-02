package com.github.tmarsteel.ktprolog.playground.jvm;

import com.github.tmarsteel.ktprolog.unification.Unification;
import kotlin.sequences.Sequence;

import javax.swing.*;
import javax.swing.border.EmptyBorder;
import java.awt.*;
import java.util.Iterator;
import java.util.NoSuchElementException;

public class SolutionExplorerPanel {

    private JPanel panel;
    private JPanel solutionsPanel;
    private JButton showNextBT;
    private JButton showAllRemainingBT;

    private Iterator<Unification> currentSolutions = null;
    private boolean currentSolutionsDepleated = true;

    public SolutionExplorerPanel() {
        initComponents();
    }

    private void initComponents() {
        showNextBT = new JButton("Next");
        showNextBT.addActionListener(evt -> showNextSolution());
        showNextBT.setEnabled(false);

        showAllRemainingBT = new JButton("All");
        showAllRemainingBT.addActionListener(evt -> showAllRemainingSolutions());
        showAllRemainingBT.setEnabled(false);

        JPanel actionsToolbar = new JPanel();
        actionsToolbar.setLayout(new BoxLayout(actionsToolbar, BoxLayout.X_AXIS));
        actionsToolbar.add(Box.createHorizontalGlue());
        actionsToolbar.add(showAllRemainingBT);
        actionsToolbar.add(Box.createRigidArea(new Dimension(10, 0)));
        actionsToolbar.add(showNextBT);

        solutionsPanel = new JPanel();
        solutionsPanel.setLayout(new BoxLayout(solutionsPanel, BoxLayout.Y_AXIS));
        solutionsPanel.setBorder(new EmptyBorder(5, 5, 5, 5));

        JScrollPane solutionScrollPane = new JScrollPane(solutionsPanel);
        solutionScrollPane.setHorizontalScrollBarPolicy(JScrollPane.HORIZONTAL_SCROLLBAR_AS_NEEDED);
        solutionScrollPane.setVerticalScrollBarPolicy(JScrollPane.VERTICAL_SCROLLBAR_AS_NEEDED);
        solutionScrollPane.setBorder(null);

        panel = new JPanel(new BorderLayout());
        panel.add(actionsToolbar, BorderLayout.NORTH);
        panel.add(solutionScrollPane, BorderLayout.CENTER);
    }

    private JComponent createTrueComponent() {
        JLabel label = new JLabel("true");
        label.setForeground(new Color(0x30, 0xB4, 0x40));
        label.setFont(label.getFont().deriveFont(Font.BOLD));
        label.setAlignmentX(Component.LEFT_ALIGNMENT);

        return label;
    }

    private JComponent createFalseComponent() {
        JLabel label = new JLabel("false");
        label.setForeground(new Color(0xE2, 0x40, 0x00));
        label.setFont(label.getFont().deriveFont(Font.BOLD));
        label.setAlignmentX(Component.LEFT_ALIGNMENT);

        return label;
    }

    public JPanel asJPanel() {
        return panel;
    }

    public void showNextSolution() {
        if (currentSolutionsDepleated) return;

        try {
            Unification solution = currentSolutions.next();
            if (solution.getVariableValues().isEmpty()) {
                solutionsPanel.add(createTrueComponent());
            } else {
                solutionsPanel.add((new SolutionPanel(solution)).asJPanel());
            }
        }
        catch (NoSuchElementException ex) {
            solutionsPanel.add(createFalseComponent());
            currentSolutionsDepleated = true;
            showNextBT.setEnabled(false);
            showAllRemainingBT.setEnabled(false);
        }

        solutionsPanel.revalidate();
        solutionsPanel.repaint();
        panel.revalidate();
        panel.repaint();
    }

    public void showAllRemainingSolutions() {
        if (currentSolutionsDepleated) return;

        while (currentSolutions.hasNext()) {
            Unification solution = currentSolutions.next();
            if (solution.getVariableValues().isEmpty()) {
                solutionsPanel.add(createTrueComponent());
            } else {
                solutionsPanel.add((new SolutionPanel(solution)).asJPanel());
            }
        }

        solutionsPanel.add(createFalseComponent());
        currentSolutionsDepleated = true;
        showNextBT.setEnabled(false);
        showAllRemainingBT.setEnabled(false);

        solutionsPanel.revalidate();
        solutionsPanel.repaint();
        panel.revalidate();
        panel.repaint();
    }

    public void setSolutions(Sequence<Unification> solutions) {
        currentSolutions = solutions.iterator();
        currentSolutionsDepleated = false;

        solutionsPanel.removeAll();
        showNextBT.setEnabled(true);
        showAllRemainingBT.setEnabled(true);

        solutionsPanel.revalidate();
        solutionsPanel.repaint();
        panel.revalidate();
        panel.repaint();
    }
}
