package com.github.prologdb.runtime.playground.jvm.persistence;

import com.fasterxml.jackson.annotation.JsonProperty;

import java.util.Objects;

/**
 * Models the state of the playground, supporting persistence
 */
public class PlaygroundState {
    /**
     * The actual text in the knowledge base window.
     */
    private String knowledgeBaseText;

    /**
     * The contents of the query field.
     */
    private String query;

    /**
     * The graphics device last shown on (multi-monitor support)
     */
    private String graphicsDeviceID = null;

    @JsonProperty
    public String getGraphicsDeviceID() {
        return graphicsDeviceID;
    }

    @JsonProperty
    public void setGraphicsDeviceID(String graphicsDeviceID) {
        this.graphicsDeviceID = graphicsDeviceID;
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

    @Override
    public boolean equals(Object o) {
        if (this == o) return true;
        if (!(o instanceof PlaygroundState)) return false;
        PlaygroundState that = (PlaygroundState) o;
        return Objects.equals(getKnowledgeBaseText(), that.getKnowledgeBaseText()) &&
                Objects.equals(getQuery(), that.getQuery());
    }

    @Override
    public int hashCode() {

        return Objects.hash(getKnowledgeBaseText(), getQuery());
    }
}
