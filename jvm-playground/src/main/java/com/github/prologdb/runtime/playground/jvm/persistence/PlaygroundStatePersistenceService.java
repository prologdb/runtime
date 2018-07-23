package com.github.prologdb.runtime.playground.jvm.persistence;

import java.io.IOException;
import java.util.Optional;

/**
 * Persists instances of {@link PlaygroundState}. Each instance of
 * {@link PlaygroundStatePersistenceService} persists to the same
 * physical location (overwrites earlier saves).
 */
public interface PlaygroundStatePersistenceService {
    /**
     * Reads the persisted state and returns it as a {@link PlaygroundState} object.
     * @return Present if there was persisted state and it was read successfully; empty
     *         if there was no state to read.
     * @throws IOException If state is present but cannot be read.
     */
    Optional<PlaygroundState> read() throws IOException;

    /**
     * Writes the given {@link PlaygroundState} to the persistent storage
     * associated with this service.
     */
    void write(PlaygroundState state) throws IOException;
}
