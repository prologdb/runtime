package com.github.prologdb.runtime.playground.jvm;

import com.github.prologdb.runtime.playground.jvm.persistence.PlaygroundState;
import com.github.prologdb.runtime.playground.jvm.persistence.PlaygroundStatePersistenceService;
import com.github.prologdb.runtime.playground.jvm.persistence.WindowState;

import javax.swing.*;
import java.awt.*;
import java.awt.event.WindowEvent;
import java.awt.event.WindowListener;
import java.io.IOException;
import java.util.Timer;
import java.util.TimerTask;

public class MainFrame extends JFrame {

    private final PlaygroundStatePersistenceService statePersistenceService;

    public MainFrame(PlaygroundStatePersistenceService statePersistenceService) {
        super("PrologDB Playground");

        this.statePersistenceService = statePersistenceService;

        initComponents();
        setDefaultCloseOperation(JFrame.DO_NOTHING_ON_CLOSE);

        SwingUtilities.invokeLater(() -> {
            try {
                statePersistenceService.read().ifPresent(playgroundPanel::setCurrentState);
                statePersistenceService.read().map(PlaygroundState::getMainWindowState).ifPresent(this::setWindowState);
                (new Timer()).schedule(periodicPersistenceTimerTask, 10000, 15000);
            }
            catch (IOException ex) {
                errorDuringInitialStateRead = true;
                JOptionPane.showMessageDialog(
                        null,
                        "Failed to read state:\n" + ex.getMessage(),
                        "I/O Error",
                        JOptionPane.ERROR_MESSAGE
                );
            }
        });
    }

    private WindowState getWindowState()
    {
        WindowState state = new WindowState();

        state.setLocationX(getLocation().x);
        state.setLocationY(getLocation().y);
        state.setWidth(getWidth());
        state.setHeight(getHeight());

        for (GraphicsDevice device : GraphicsEnvironment.getLocalGraphicsEnvironment().getScreenDevices()) {
            if (device.getDefaultConfiguration().getBounds().contains(getLocation())) {
                state.setGraphicsDeviceID(device.getIDstring());
                break;
            }
        }

        return state;
    }

    private void setWindowState(WindowState state)
    {

        if (state.getWidth() != null && state.getHeight() != null) {
            setSize(state.getWidth(), state.getHeight());
        }

        if (state.getLocationX() != null && state.getLocationY() != null) {
            setLocation(state.getLocationX(), state.getLocationY());
        }

        GraphicsEnvironment ge = GraphicsEnvironment.getLocalGraphicsEnvironment();
        for (GraphicsDevice device : ge.getScreenDevices()) {
            if (device.getIDstring().equals(state.getGraphicsDeviceID())) {
                moveTo(device);
                return;
            }
        }
    }

    private void moveTo(GraphicsDevice targetDevice) {
        GraphicsConfiguration gc = getGraphicsConfiguration();
        if (gc.getDevice() == targetDevice) return;
        if (gc.getDevice().getIDstring().equals(targetDevice.getIDstring()));

        // following lines shamelessly stolen from java.awt.Window.init(GraphicsConfiguration)
        Rectangle targetBounds = targetDevice.getDefaultConfiguration().getBounds();
        Insets screenInsets = getToolkit().getScreenInsets(gc);
        setLocation(
            getX() + targetBounds.x + screenInsets.left,
            getY() + targetBounds.y + screenInsets.top
        );
    }

    private TimerTask periodicPersistenceTimerTask = new TimerTask() {
        @Override
        public void run() {
            try {
                persistCurrentState();
            }
            catch (IOException ex) {
                ex.printStackTrace(System.err);
            }
        }
    };

    private final PlaygroundPanel playgroundPanel = new PlaygroundPanel();

    /** The state last persisted; null if not persisted yet. */
    private PlaygroundState lastPersistedState;

    /**
     * Is set to true when an error occurs while initially reading the
     * state from the service. If so, state will not be persisted
     * because that might overwrite valuable state.
     */
    private boolean errorDuringInitialStateRead = false;

    private void persistCurrentState() throws IOException {
        PlaygroundState state = playgroundPanel.getCurrentState();
        state.setMainWindowState(getWindowState());
        statePersistenceService.write(state);

        lastPersistedState = state;
    }

    private void initComponents() {
        setLayout(new BorderLayout());

        getContentPane().add(playgroundPanel.asJPanel(), BorderLayout.CENTER);

        setDefaultCloseOperation(WindowConstants.EXIT_ON_CLOSE);
        pack();
        setLocationRelativeTo(null);

        addWindowListener(new WindowListener() {
            @Override
            public void windowOpened(WindowEvent e) { }

            @Override
            public void windowClosing(WindowEvent e) {
                e.getWindow().setEnabled(false);

                try {
                    PlaygroundState currentState = playgroundPanel.getCurrentState();

                    if (lastPersistedState == null || !lastPersistedState.equals(currentState)) {
                        try {
                            persistCurrentState();
                        } catch (IOException ex) {
                            int choice = JOptionPane.showConfirmDialog(
                                    null,
                                    "Failed to persist current state. Close anyway?\n" + ex.getMessage(),
                                    "I/O Error",
                                    JOptionPane.YES_NO_OPTION,
                                    JOptionPane.ERROR_MESSAGE
                            );

                            if (choice != 0) {
                                return;
                            }
                        }
                    }

                    e.getWindow().dispose();
                }
                finally {
                    e.getWindow().setEnabled(true);
                }
            }

            @Override
            public void windowClosed(WindowEvent e) { }

            @Override
            public void windowIconified(WindowEvent e) { }

            @Override
            public void windowDeiconified(WindowEvent e) { }

            @Override
            public void windowActivated(WindowEvent e) { }

            @Override
            public void windowDeactivated(WindowEvent e) { }
        });
    }
}
