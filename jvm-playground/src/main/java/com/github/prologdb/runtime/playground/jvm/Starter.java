package com.github.prologdb.runtime.playground.jvm;

import java.io.File;
import java.io.IOException;
import java.nio.file.Files;
import java.nio.file.Path;
import java.util.Optional;

import javax.swing.JOptionPane;
import javax.swing.SwingUtilities;
import javax.swing.UIManager;

import org.fife.ui.rsyntaxtextarea.AbstractTokenMakerFactory;
import org.fife.ui.rsyntaxtextarea.TokenMakerFactory;

import com.github.prologdb.runtime.playground.jvm.editor.JFlexPrologTokenMaker;

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

            Optional<Path> statePersistenceFile;
            try {
                statePersistenceFile = Optional.of(getStatePersistenceFile());
            }
            catch (IOException ex) {
                statePersistenceFile = Optional.empty();
                ex.printStackTrace(System.err);
                JOptionPane.showMessageDialog(
                        null,
                        "Cannot persist playground state:\nIOException: " + ex.getMessage(),
                        "I/O Error",
                        JOptionPane.ERROR_MESSAGE
                );
            }

            new MainFrame(statePersistenceFile).setVisible(true);
        });
    }

    /**
     * @return the path to the persistence file
     */
    private static Path getStatePersistenceFile() throws IOException {
        return getPersistenceDirectory().resolve("playground-state.json");
    }

    /**
     * @return a path to a directory where state can be persisted. The path is assured to exist.
     */
    private static Path getPersistenceDirectory() throws IOException {
        String appdata = System.getenv("APPDATA");
        Path persistenceDir;
        if (appdata != null) {
            // Windows
            persistenceDir = (new File(appdata).toPath()).resolve("prologdb-playground");
        } else {
            // unix-like
            persistenceDir = (new File(System.getProperty("user.home")).toPath()).resolve(".prologdb-playground");
        }

        if (!Files.notExists(persistenceDir)) {
            Files.createDirectory(persistenceDir);
        }

        return persistenceDir;
    }
}
