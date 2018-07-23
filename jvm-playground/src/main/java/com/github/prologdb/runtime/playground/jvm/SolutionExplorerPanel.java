package com.github.prologdb.runtime.playground.jvm;

import java.awt.BorderLayout;
import java.awt.Color;
import java.awt.Component;
import java.awt.Dimension;
import java.awt.Font;
import java.awt.GridBagConstraints;
import java.awt.GridBagLayout;
import java.util.NoSuchElementException;

import javax.swing.Box;
import javax.swing.BoxLayout;
import javax.swing.JButton;
import javax.swing.JComponent;
import javax.swing.JLabel;
import javax.swing.JPanel;
import javax.swing.JScrollPane;
import javax.swing.border.EmptyBorder;

import com.github.prologdb.runtime.PrologException;
import com.github.prologdb.runtime.PrologRuntimeException;
import com.github.prologdb.runtime.lazysequence.LazySequence;
import com.github.prologdb.runtime.unification.Unification;

public class SolutionExplorerPanel {

    private JPanel panel;
    private JPanel solutionsPanel;
    private GridBagLayout solutionsPaneLayout;
    private JButton showNextBT;
    private JButton showAllRemainingBT;

    private LazySequence<Unification> currentSolutions = null;
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
        String errorMessageAsHtml = "<html><pre>" + errorMessage + "</pre></html>";

        JLabel label = new JLabel(errorMessageAsHtml);
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
            Unification solution = currentSolutions.tryAdvance();
            if (solution != null) {
                addSolution(solution);
            } else {
                addSolutionComponent(createFalseComponent());
                setDepleated();
            }
        }
        catch (NoSuchElementException ex) {
            addSolutionComponent(createFalseComponent());
            setDepleated();
        }
        catch (PrologRuntimeException e) {
            addSolutionComponent(createErrorComponent("Error: " + formatPrologException(e)));
            e.printStackTrace(System.err);
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

        try {
            while (true) {
                Unification solution = currentSolutions.tryAdvance();
                if (solution != null) {
                    addSolution(solution);
                } else break;
            }
        }
        catch (PrologRuntimeException e) {
            addSolutionComponent(createErrorComponent("Error: " + formatPrologException(e)));
            e.printStackTrace(System.err);
            setDepleated();
        }
        catch (StackOverflowError e) {
            addSolutionComponent(createErrorComponent("Out of local stack."));
            setDepleated();
        }

        addSolutionComponent(createFalseComponent());
        setDepleated();

        solutionsPanel.revalidate();
        solutionsPanel.repaint();
        panel.revalidate();
        panel.repaint();
    }

    public void setSolutions(LazySequence<Unification> solutions) {
        currentSolutions = solutions;
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

    private String formatPrologException(PrologException ex) {
        StringBuilder sb = new StringBuilder();

        Throwable pivot = ex;
        while (pivot != null) {
            sb.append(pivot.getMessage());

            if (pivot instanceof PrologException) {
                ((PrologException) pivot).getPrologStackTrace().forEach(pste -> {
                    sb.append('\n');
                    sb.append(pste.toString());
                });
            } else {
                for (StackTraceElement jste : pivot.getStackTrace()) {
                    sb.append('\n');
                    sb.append("\tat ");
                    sb.append(jste.getClassName());
                    sb.append('.');
                    sb.append(jste.getLineNumber());
                    sb.append('(');
                    sb.append(jste.getFileName());
                    sb.append(':');
                    sb.append(Integer.toString(jste.getLineNumber(), 10));
                    sb.append(')');
                }
            }

            pivot = pivot.getCause();
            if (pivot != null) {
                sb.append('\n');
                sb.append("caused by ");
            }
        }

        return sb.toString();
    }
}
