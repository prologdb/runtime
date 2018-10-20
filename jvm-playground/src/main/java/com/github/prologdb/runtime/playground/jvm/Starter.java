package com.github.prologdb.runtime.playground.jvm;

import com.github.prologdb.runtime.playground.jvm.editor.JFlexPrologTokenMaker;
import com.github.prologdb.runtime.playground.jvm.persistence.AppDataPlagroundStatePersistenceService;
import com.github.prologdb.runtime.playground.jvm.persistence.NullPlaygroundStatePersistenceService;
import com.github.prologdb.runtime.playground.jvm.persistence.PlaygroundStatePersistenceService;
import org.fife.ui.rsyntaxtextarea.AbstractTokenMakerFactory;
import org.fife.ui.rsyntaxtextarea.TokenMakerFactory;

import javax.swing.*;
import java.awt.event.WindowEvent;
import java.awt.event.WindowListener;
import java.io.IOException;

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

            MainFrame mf = new MainFrame(statePersistenceService);
            mf.addWindowListener(new WindowListener() {
                @Override
                public void windowOpened(WindowEvent e) { }

                @Override
                public void windowClosing(WindowEvent e) { }

                @Override
                public void windowClosed(WindowEvent e) {
                    System.exit(-1);
                }

                @Override
                public void windowIconified(WindowEvent e) { }

                @Override
                public void windowDeiconified(WindowEvent e) { }

                @Override
                public void windowActivated(WindowEvent e) { }

                @Override
                public void windowDeactivated(WindowEvent e) { }
            });
            mf.setVisible(true);
        });
    }


}
