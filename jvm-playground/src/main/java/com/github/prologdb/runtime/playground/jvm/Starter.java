package com.github.prologdb.runtime.playground.jvm;

import com.github.prologdb.runtime.playground.jvm.editor.JFlexPrologTokenMaker;
import org.fife.ui.rsyntaxtextarea.AbstractTokenMakerFactory;
import org.fife.ui.rsyntaxtextarea.TokenMakerFactory;

import javax.swing.*;

public class Starter {
    public static void main(final String... args) {
        ((AbstractTokenMakerFactory) TokenMakerFactory.getDefaultInstance())
            .putMapping("text/prolog", JFlexPrologTokenMaker.class.getName());

        SwingUtilities.invokeLater(() -> {
            try {
                UIManager.setLookAndFeel(UIManager.getSystemLookAndFeelClassName());
            }
            catch (Exception ex) {
                System.err.println("Failed to set system LAF:");
                ex.printStackTrace(System.err);
            }

            new MainFrame().setVisible(true);
        });
    }
}
