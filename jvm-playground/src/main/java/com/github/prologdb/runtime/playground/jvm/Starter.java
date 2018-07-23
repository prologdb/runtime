package com.github.prologdb.runtime.playground.jvm;

import java.io.IOException;

import javax.swing.JOptionPane;
import javax.swing.SwingUtilities;
import javax.swing.UIManager;

import org.fife.ui.rsyntaxtextarea.AbstractTokenMakerFactory;
import org.fife.ui.rsyntaxtextarea.TokenMakerFactory;

import com.github.prologdb.runtime.playground.jvm.editor.JFlexPrologTokenMaker;
import com.github.prologdb.runtime.playground.jvm.persistence.AppDataPlagroundStatePersistenceService;
import com.github.prologdb.runtime.playground.jvm.persistence.NullPlaygroundStatePersistenceService;
import com.github.prologdb.runtime.playground.jvm.persistence.PlaygroundStatePersistenceService;

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

            PlaygroundStatePersistenceService statePersistenceService;
            try {
                statePersistenceService = new AppDataPlagroundStatePersistenceService();
            }
            catch (IOException ex) {
                statePersistenceService = new NullPlaygroundStatePersistenceService();
                ex.printStackTrace(System.err);
                JOptionPane.showMessageDialog(
                        null,
                        "Cannot persist playground state:\nIOException: " + ex.getMessage(),
                        "I/O Error",
                        JOptionPane.ERROR_MESSAGE
                );
            }

            new MainFrame(statePersistenceService).setVisible(true);
        });
    }


}
