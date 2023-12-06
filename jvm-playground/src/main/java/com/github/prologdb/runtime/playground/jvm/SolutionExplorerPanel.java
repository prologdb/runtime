package com.github.prologdb.runtime.playground.jvm;

import com.github.prologdb.async.LazySequence;
import com.github.prologdb.runtime.PrologException;
import com.github.prologdb.runtime.builtin.ISOOpsOperatorRegistry;
import com.github.prologdb.runtime.term.CompoundTerm;
import com.github.prologdb.runtime.term.Term;
import com.github.prologdb.runtime.unification.Unification;
import com.github.prologdb.runtime.util.OperatorRegistry;

import javax.swing.*;
import javax.swing.border.EmptyBorder;
import java.awt.*;
import java.math.BigDecimal;
import java.math.RoundingMode;
import java.time.Duration;
import java.util.NoSuchElementException;
import java.util.concurrent.ForkJoinPool;

public class SolutionExplorerPanel {

    private JPanel panel;
    private JPanel solutionsPanel;
    private GridBagLayout solutionsPaneLayout;
    private JButton showNextBT;
    private JButton showAllRemainingBT;

    private JLabel loadTimeOutput = new JLabel("-");
    private JLabel solutionTimeOutput = new JLabel("-");

    private LazySequence<Unification> currentSolutions = null;
    private OperatorRegistry currentSolutionDisplayOperators = null;
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

        JPanel actionsToolbarLeft = new JPanel();
        actionsToolbarLeft.setLayout(new BoxLayout(actionsToolbarLeft, BoxLayout.X_AXIS));
        actionsToolbarLeft.add(new JLabel("  load time: "));
        actionsToolbarLeft.add(loadTimeOutput);
        actionsToolbarLeft.add(new JLabel("   solution time: "));
        actionsToolbarLeft.add(solutionTimeOutput);

        JPanel actionsToolbarRight = new JPanel();
        actionsToolbarRight.setLayout(new BoxLayout(actionsToolbarRight, BoxLayout.X_AXIS));
        actionsToolbarRight.add(Box.createHorizontalGlue());
        actionsToolbarRight.add(showAllRemainingBT);
        actionsToolbarRight.add(Box.createRigidArea(new Dimension(10, 0)));
        actionsToolbarRight.add(showNextBT);

        JPanel actionsToolbar = new JPanel(new BorderLayout());
        actionsToolbar.add(actionsToolbarLeft, BorderLayout.WEST);
        actionsToolbar.add(actionsToolbarRight, BorderLayout.EAST);

        solutionsPaneLayout = new GridBagLayout();
        solutionsPanel = new JPanel(solutionsPaneLayout);
        solutionsPanel.setBorder(new EmptyBorder(5, 5, 5, 5));

        JPanel solutionsWrapperPanel = new JPanel(new BorderLayout());
        solutionsWrapperPanel.add(solutionsPanel, BorderLayout.NORTH);

        JScrollPane solutionsScrollPane = new JScrollPane(solutionsWrapperPanel);
        solutionsScrollPane.setHorizontalScrollBarPolicy(JScrollPane.HORIZONTAL_SCROLLBAR_AS_NEEDED);
        solutionsScrollPane.setVerticalScrollBarPolicy(JScrollPane.VERTICAL_SCROLLBAR_AS_NEEDED);
        solutionsScrollPane.setBorder(null);
        solutionsScrollPane.getVerticalScrollBar().setUnitIncrement(22);
        solutionsScrollPane.getHorizontalScrollBar().setUnitIncrement(22);

        panel = new JPanel();
        panel.setLayout(new BorderLayout());
        panel.add(actionsToolbar, BorderLayout.NORTH);
        panel.add(solutionsScrollPane, BorderLayout.CENTER);
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
        if (solution.isEmpty()) {
            addSolutionComponent(createTrueComponent());
        } else {
            SolutionPanel solutionPanel = new SolutionPanel(solution, currentSolutionDisplayOperators);
            solutionPanel.setIndex(currentSolutionIndex);
            addSolutionComponent(solutionPanel.asJPanel());
        }

        currentSolutionIndex++;

        solution.getEntries().forEach(e -> {
            var entryTerm = new CompoundTerm("=", new Term[] { e.getFirst(), e.getSecond() });
            System.out.println(entryTerm.toStringUsingOperatorNotations(currentSolutionDisplayOperators));
        });
        System.out.println("-------");
    }

    private void refreshUI() {
        SwingUtilities.invokeLater(() -> {
            solutionsPanel.revalidate();
            solutionsPanel.repaint();
            panel.revalidate();
            panel.repaint();
        });
    }

    public void showNextSolution()
    {
        if (currentSolutionsDepleated) {
            return;
        }

        showNextBT.setEnabled(false);
        showAllRemainingBT.setEnabled(false);
        ForkJoinPool.commonPool().execute(() -> {
            try {
                long solutionStart = System.currentTimeMillis();
                Unification solution = currentSolutions.tryAdvance();
                long solutionDuration = System.currentTimeMillis() - solutionStart;

                if (solution != null) {
                    solutionTimeOutput.setText(formatMillis(solutionDuration));
                    addSolution(solution);
                }
                if (solution == null || currentSolutions.getState() == LazySequence.State.DEPLETED) {
                    addSolutionComponent(createFalseComponent());
                    setDepleted();
                } else {
                    showNextBT.setEnabled(true);
                    showAllRemainingBT.setEnabled(true);
                }
            } catch (NoSuchElementException ex) {
                addSolutionComponent(createFalseComponent());
                setDepleted();
            } catch (PrologException e) {
                addSolutionComponent(createErrorComponent("Error: " + e.getFormattedPrologStackTrace()));
                e.printStackTrace(System.err);
                setDepleted();
            } catch (StackOverflowError e) {
                addSolutionComponent(createErrorComponent("Out of local stack."));
                setDepleted();
            }

            refreshUI();
        });
    }

    public void showAllRemainingSolutions()
    {
        if (currentSolutionsDepleated) {
            return;
        }

        showNextBT.setEnabled(false);
        showAllRemainingBT.setEnabled(false);
        ForkJoinPool.commonPool().execute(() -> {
            long duration = 0;
            long nSolutions = 0;
            try {
                while (true) {
                    long solutionStart = System.currentTimeMillis();
                    Unification solution = currentSolutions.tryAdvance();
                    duration += System.currentTimeMillis() - solutionStart;
                    nSolutions++;

                    if (solution != null) {
                        addSolution(solution);
                        solutionTimeOutput.setText(formatMillis(duration) + " (" + nSolutions + " solutions)");
                        refreshUI();
                    } else {
                        break;
                    }
                }
            } catch (PrologException e) {
                addSolutionComponent(createErrorComponent("Error: " + e.getFormattedPrologStackTrace()));
                e.printStackTrace(System.err);
                setDepleted();
            } catch (StackOverflowError e) {
                addSolutionComponent(createErrorComponent("Out of local stack."));
                setDepleted();
            }

            addSolutionComponent(createFalseComponent());
            setDepleted();
            solutionTimeOutput.setText(formatMillis(duration) + " (all)");

            refreshUI();
        });
    }

    public void setSolutions(LazySequence<Unification> solutions, OperatorRegistry displayOperators) {
        currentSolutions = solutions;
        currentSolutionsDepleated = false;
        currentSolutionIndex = 0;
        currentSolutionDisplayOperators = displayOperators;

        solutionsPanel.removeAll();
        showNextBT.setEnabled(true);
        showAllRemainingBT.setEnabled(true);

        solutionsPanel.revalidate();
        solutionsPanel.repaint();
        panel.revalidate();
        panel.repaint();
    }

    public void setDepleted() {
        currentSolutionsDepleated = true;
        showNextBT.setEnabled(false);
        showAllRemainingBT.setEnabled(false);
        currentSolutionIndex = -1;
    }

    public void setLoadTime(Duration parseTime) {
        loadTimeOutput.setText(formatMillis(parseTime.toMillis()));
    }

    private static final BigDecimal THOUSAND = new BigDecimal("1000");
    private static final BigDecimal SIXTY = new BigDecimal("60");
    private static String formatMillis(long millis) {
        if (millis < 1000) {
            return millis + "ms";
        }

        // seconds
        BigDecimal time = new BigDecimal(Long.toString(millis, 10)).divide(THOUSAND, 3, RoundingMode.CEILING);
        if (time.compareTo(SIXTY) < 0) {
            return time.setScale(1, RoundingMode.CEILING).toString() + "s";
        }

        // minutes
        BigDecimal minutes = time.divide(SIXTY, 0, RoundingMode.FLOOR);
        BigDecimal seconds = time.subtract(minutes.multiply(SIXTY)).setScale(0, RoundingMode.HALF_UP);
        if (minutes.compareTo(SIXTY) < 0) {
            if (seconds.compareTo(BigDecimal.ZERO) > 0) {
                return minutes + "m " + seconds + "s";
            } else {
                return minutes + "m";
            }
        }

        // hours
        BigDecimal hours = minutes.divide(SIXTY, 0, RoundingMode.FLOOR);
        minutes = minutes.subtract(hours.multiply(SIXTY));
        if (minutes.compareTo(BigDecimal.ZERO) > 0) {
            return hours + "h " + minutes + "m";
        } else {
            return hours + "h";
        }
    }
}
