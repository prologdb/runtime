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
    private GridBagLayout solutionsPaneLayout;
    private JButton showNextBT;
    private JButton showAllRemainingBT;

    private Iterator<Unification> currentSolutions = null;
    private boolean currentSolutionsDepleated = true;
    private int currentSolutionIndex = -1;

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

        solutionsPaneLayout = new GridBagLayout();
        solutionsPanel = new JPanel(solutionsPaneLayout);
        solutionsPanel.setBorder(new EmptyBorder(5, 5, 5, 5));

        JScrollPane solutionScrollPane = new JScrollPane(solutionsPanel);
        solutionScrollPane.setHorizontalScrollBarPolicy(JScrollPane.HORIZONTAL_SCROLLBAR_AS_NEEDED);
        solutionScrollPane.setVerticalScrollBarPolicy(JScrollPane.VERTICAL_SCROLLBAR_AS_NEEDED);
        solutionScrollPane.setBorder(null);
        solutionScrollPane.getVerticalScrollBar().setUnitIncrement(22);

        JPanel solutionsWrapperPanel = new JPanel(new BorderLayout());
        solutionsWrapperPanel.add(solutionsPanel, BorderLayout.NORTH);

        panel = new JPanel();
        panel.setLayout(new BorderLayout());
        panel.add(actionsToolbar, BorderLayout.NORTH);
        panel.add(solutionsWrapperPanel, BorderLayout.CENTER);
    }

    private JComponent createTrueComponent() {
        JLabel label = new JLabel("true");
        label.setForeground(new Color(0x30, 0xB4, 0x40));
        label.setFont(label.getFont().deriveFont(Font.BOLD));
        label.setAlignmentX(Component.LEFT_ALIGNMENT);
        label.setBorder(new EmptyBorder(5, 5, 5, 5));

        return label;
    }

    private JComponent createFalseComponent() {
        JLabel label = new JLabel("false");
        label.setForeground(new Color(0xE2, 0x40, 0x00));
        label.setFont(label.getFont().deriveFont(Font.BOLD));
        label.setAlignmentX(Component.LEFT_ALIGNMENT);
        label.setBorder(new EmptyBorder(5, 5, 5, 5));

        return label;
    }

    private JComponent createErrorComponent(String errorMessage) {
        JLabel label = new JLabel(errorMessage);
        label.setForeground(new Color(0xE2, 0x40, 0x00));
        label.setBackground(new Color(242, 140, 137));
        label.setFont(label.getFont().deriveFont(Font.BOLD));
        label.setAlignmentX(Component.LEFT_ALIGNMENT);
        label.setBorder(new EmptyBorder(5, 5, 5, 5));

        return label;
    }

    public JPanel asJPanel() {
        return panel;
    }

    private void addSolutionComponent(Component c) {
        GridBagConstraints constraints = new GridBagConstraints();
        constraints.gridy = currentSolutionIndex;
        constraints.gridwidth = 1;
        constraints.weightx = 1.0;
        constraints.fill = GridBagConstraints.BOTH;
        constraints.anchor = GridBagConstraints.NORTH;
        solutionsPaneLayout.setConstraints(c, constraints);
        solutionsPanel.add(c);
    }

    private void addSolution(Unification solution) {
        if (solution.getVariableValues().isEmpty()) {
            addSolutionComponent(createTrueComponent());
        } else {
            SolutionPanel solutionPanel = new SolutionPanel(solution);
            solutionPanel.setIndex(currentSolutionIndex);
            addSolutionComponent(solutionPanel.asJPanel());
        }

        currentSolutionIndex++;
    }

    public void showNextSolution() {
        if (currentSolutionsDepleated) return;

        try {
            addSolution(currentSolutions.next());
        }
        catch (NoSuchElementException ex) {
            addSolutionComponent(createFalseComponent());
            setDepleated();
        }
        catch (StackOverflowError e) {
            addSolutionComponent(createErrorComponent("Out of local stack."));
            setDepleated();
        }

        solutionsPanel.revalidate();
        solutionsPanel.repaint();
        panel.revalidate();
        panel.repaint();
    }

    public void showAllRemainingSolutions() {
        if (currentSolutionsDepleated) return;

        while (currentSolutions.hasNext()) {
            addSolution(currentSolutions.next());
        }

        addSolutionComponent(createFalseComponent());
        setDepleated();

        solutionsPanel.revalidate();
        solutionsPanel.repaint();
        panel.revalidate();
        panel.repaint();
    }

    public void setSolutions(Sequence<Unification> solutions) {
        currentSolutions = solutions.iterator();
        currentSolutionsDepleated = false;
        currentSolutionIndex = 0;

        solutionsPanel.removeAll();
        showNextBT.setEnabled(true);
        showAllRemainingBT.setEnabled(true);

        solutionsPanel.revalidate();
        solutionsPanel.repaint();
        panel.revalidate();
        panel.repaint();
    }

    public void setDepleated() {
        currentSolutionsDepleated = true;
        showNextBT.setEnabled(false);
        showAllRemainingBT.setEnabled(false);
        currentSolutionIndex = -1;
    }
}
