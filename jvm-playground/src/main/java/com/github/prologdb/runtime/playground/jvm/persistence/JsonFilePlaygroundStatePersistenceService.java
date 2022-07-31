package com.github.prologdb.runtime.playground.jvm.persistence;

import com.fasterxml.jackson.databind.ObjectMapper;

import java.io.IOException;
import java.nio.file.Files;
import java.nio.file.Path;
import java.util.Objects;
import java.util.Optional;

/**
 * Implements {@link PlaygroundStatePersistenceService} by writing the state as
 * JSON to a file specified when the service is constructed
 */
public class JsonFilePlaygroundStatePersistenceService implements PlaygroundStatePersistenceService {
    /**
     * The file to persist to
     */
    private Path filePath;

    private Path backupPath;

    private ObjectMapper objectMapper;

    public JsonFilePlaygroundStatePersistenceService(Path filePath, ObjectMapper om)
    {
        this.filePath = Objects.requireNonNull(filePath);
        this.backupPath = filePath.resolveSibling(filePath.getFileName().toString() + ".bak");
        this.objectMapper = Objects.requireNonNull(om);
    }

    public JsonFilePlaygroundStatePersistenceService(Path filePath) {
        this(filePath, new ObjectMapper());
    }

    @Override
    public Optional<PlaygroundState> read() throws IOException
    {
        if (Files.exists(backupPath)) {
            return Optional.of(objectMapper.readValue(backupPath.toFile(), PlaygroundState.class));
        }

        if (Files.exists(filePath)) {
            if (Files.isRegularFile(filePath)) {
                return Optional.of(objectMapper.readValue(filePath.toFile(), PlaygroundState.class));
            } else {
                throw new IOException(filePath.toString() + " is a directory, expected file");
            }
        } else {
            return Optional.empty();
        }
    }

    @Override
    public void write(PlaygroundState state) throws IOException
    {
        if (Files.exists(filePath) && !Files.isRegularFile(filePath)) {
            throw new IOException(filePath.toString() + " is a directory, expected file");
        }

        if (Files.exists(backupPath) && !Files.isRegularFile(backupPath)) {
            throw new IOException(backupPath.toString() + " is a directory, expected file");
        }

        Files.deleteIfExists(backupPath);
        if (Files.exists(filePath)) {
            Files.move(filePath, backupPath);
        }
        objectMapper.writeValue(filePath.toFile(), state);
        Files.deleteIfExists(backupPath);
    }
}
