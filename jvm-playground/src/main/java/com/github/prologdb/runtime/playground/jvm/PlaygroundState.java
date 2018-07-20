package com.github.prologdb.runtime.playground.jvm;

import java.nio.file.Path;

import com.fasterxml.jackson.annotation.JsonProperty;

/**
 * Models the state of the playground, supporting persistence
 */
public class PlaygroundState {
    /**
     * Path to the current file. If null, there is no related file.
     */
    private Path openKnowledgeBaseFile;

    /**
     * The actual text in the knowledge base window. Might differ from
     * the contents of {@link #openKnowledgeBaseFile} in case changes were
     * made and the playground was closed without saving the file.
     */
    private String knowledgeBaseText;

    /**
     * The contents of the query field.
     */
    private String query;

    @JsonProperty
    public Path getOpenKnowledgeBaseFile() {
        return openKnowledgeBaseFile;
    }

    @JsonProperty
    public void setOpenKnowledgeBaseFile(Path openKnowledgeBaseFile) {
        this.openKnowledgeBaseFile = openKnowledgeBaseFile;
    }

    @JsonProperty
    public String getKnowledgeBaseText() {
        return knowledgeBaseText;
    }

    @JsonProperty
    public void setKnowledgeBaseText(String knowledgeBaseText) {
        this.knowledgeBaseText = knowledgeBaseText;
    }

    @JsonProperty
    public String getQuery() {
        if (query == null) return "";
        return query;
    }

    @JsonProperty
    public void setQuery(String query) {
        this.query = query;
    }
}
