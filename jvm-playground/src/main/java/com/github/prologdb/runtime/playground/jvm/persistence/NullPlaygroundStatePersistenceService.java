package com.github.prologdb.runtime.playground.jvm.persistence;

import java.util.Optional;

/**
 * Implements {@link PlaygroundStatePersistenceService} by never reading anything
 * and discarding all writes.
 */
public class NullPlaygroundStatePersistenceService implements PlaygroundStatePersistenceService {
    @Override
    public Optional<PlaygroundState> read() {
        return Optional.empty();
    }

    @Override
    public void write(PlaygroundState state) {

    }
}
