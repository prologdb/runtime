package com.github.prologdb.runtime.playground.jvm.persistence;

import java.io.File;
import java.io.IOException;
import java.nio.file.Files;
import java.nio.file.Path;

/**
 * Implements {@link PlaygroundStatePersistenceService} by writing the state
 * as json into the typical location of volatile application data of the current user;
 * specifically:
 * <ul>
 *     <li>
 *         If the environment variable <code>%APPDATA%</code> is set (mostly Windows),
 *         writes to <code>%APPDATA%/prologdb-playground/playground-state.json</code>
 *     </li>
 *     <li>
 *         Otherwise obtains the users home directory from the java system property
 *         <code>user.home</code> and writes to
 *         <code>${user.home}/.prologdb-playground/playground-state.json</code>
 *     </li>
 * </ul>
 */
public class AppDataPlagroundStatePersistenceService extends JsonFilePlaygroundStatePersistenceService {

    public AppDataPlagroundStatePersistenceService() throws IOException {
        super(getStatePersistenceFile());
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

        if (Files.notExists(persistenceDir)) {
            Files.createDirectory(persistenceDir);
        }

        return persistenceDir;
    }
}
