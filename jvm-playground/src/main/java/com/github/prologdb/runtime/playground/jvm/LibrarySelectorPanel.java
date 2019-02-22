package com.github.prologdb.runtime.playground.jvm;

import com.github.prologdb.runtime.knowledge.library.ClauseIndicator;
import com.github.prologdb.runtime.knowledge.library.Library;

import javax.swing.*;
import javax.swing.event.ChangeEvent;
import java.awt.*;
import java.awt.event.ComponentEvent;
import java.awt.event.ComponentListener;
import java.util.ArrayList;
import java.util.Collections;
import java.util.Comparator;
import java.util.EnumMap;
import java.util.HashSet;
import java.util.List;
import java.util.Map;
import java.util.Set;
import java.util.function.Consumer;

import static java.util.Arrays.asList;

public class LibrarySelectorPanel
{
    public static final Set<LoadableLibrary> DEFAULT_SELECTION;
    static {
        Set<LoadableLibrary> _defaults = new HashSet<>(asList(LoadableLibrary.values()));
        DEFAULT_SELECTION = Collections.unmodifiableSet(_defaults);
    }
    private Set<LoadableLibrary> selection;

    private final JPanel panel = new JPanel();

    private final FlowLayout flowLayout = new FlowLayout(FlowLayout.LEADING, 20, 0);

    private final Map<LoadableLibrary, JCheckBox> libraryCheckboxes = new EnumMap<>(LoadableLibrary.class);

    private final Set<Consumer<Set<LoadableLibrary>>> selectionChangeListeners = new HashSet<>();

    public LibrarySelectorPanel()
    {
        initComponents();
        setSelection(DEFAULT_SELECTION);
    }

    public void setSelection(Set<LoadableLibrary> selection)
    {
        this.selection = Collections.newSetFromMap(new EnumMap<>(LoadableLibrary.class));
        this.selection.addAll(selection);
        updateCheckboxes();
        notifyListeners();
    }

    public void addSelectionChangedListener(Consumer<Set<LoadableLibrary>> listener)
    {
        selectionChangeListeners.add(listener);
        listener.accept(Collections.unmodifiableSet(selection));
    }

    public JPanel asJPanel()
    {
        return panel;
    }

    private void initComponents()
    {
        panel.setLayout(flowLayout);

        panel.add(new JLabel("Load Libraries:  "));

        for (LoadableLibrary loadableLibrary : LoadableLibrary.values()) {
            JCheckBox libraryCheckbox = new JCheckBox(loadableLibrary.getLibrary().getName());
            libraryCheckbox.setCursor(Cursor.getPredefinedCursor(Cursor.HAND_CURSOR));
            panel.add(libraryCheckbox);
            libraryCheckboxes.put(loadableLibrary, libraryCheckbox);
            libraryCheckbox.setToolTipText(getQuickListing(loadableLibrary.getLibrary()));
            libraryCheckbox.addChangeListener(this::onCheckBoxChange);
        }

        panel.addComponentListener(new ComponentListener()
        {
            @Override
            public void componentResized(ComponentEvent e)
            {
                int minWidth = 0;
                int minHeight = 0;

                for (Component c : panel.getComponents()) {
                    int componentRightEdge = c.getX() + c.getWidth();
                    int componentBottomEdge = c.getY() + c.getHeight();
                    if (minWidth < componentRightEdge) {
                        minWidth = componentRightEdge;
                    }
                    if (minHeight < componentBottomEdge) {
                        minHeight = componentBottomEdge;
                    }
                }

                minWidth += panel.getInsets().left + panel.getInsets().right + flowLayout.getHgap() * 2;
                minHeight += panel.getInsets().top + panel.getInsets().bottom + flowLayout.getVgap() * 2;

                panel.setPreferredSize(new Dimension(minWidth, minHeight));
            }

            @Override
            public void componentMoved(ComponentEvent e)
            {
            }

            @Override
            public void componentShown(ComponentEvent e)
            {
            }

            @Override
            public void componentHidden(ComponentEvent e)
            {
            }
        });
    }

    private String getQuickListing(Library library)
    {
        StringBuilder sb = new StringBuilder();
        List<ClauseIndicator> exports = new ArrayList<>(library.getExports());
        exports.sort(Comparator.comparing(ClauseIndicator::getName).thenComparing(ClauseIndicator::getArity));

        for (int i = 0; i < exports.size(); i++) {
            if (sb.length() > 50) {
                sb.append(", ... (");
                sb.append(exports.size() - i);
                sb.append(" more)");
                break;
            }

            ClauseIndicator export = exports.get(i);
            sb.append(export.getName());
            sb.append('/');
            sb.append(export.getArity());

            if (i < exports.size() - 1) {
                sb.append(", ");
            }
        }

        return sb.toString();
    }

    private void updateCheckboxes()
    {
        for (Map.Entry<LoadableLibrary, JCheckBox> libWithCB : libraryCheckboxes.entrySet()) {
            libWithCB.getValue().setSelected(selection.contains(libWithCB.getKey()));
        }
    }

    private void notifyListeners()
    {
        Set<LoadableLibrary> publishableCopy = Collections.unmodifiableSet(selection);
        for (Consumer<Set<LoadableLibrary>> listener : selectionChangeListeners) {
            listener.accept(publishableCopy);
        }
    }

    private void onCheckBoxChange(ChangeEvent event)
    {
        for (Map.Entry<LoadableLibrary, JCheckBox> libWithCB : libraryCheckboxes.entrySet()) {
            LoadableLibrary library = libWithCB.getKey();
            JCheckBox checkBox = libWithCB.getValue();
            if (checkBox == event.getSource()) {
                if (checkBox.isSelected()) {
                    selection.add(library);
                } else {
                    selection.remove(library);
                }
            }
        }

        notifyListeners();
    }
}
