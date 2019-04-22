package com.github.prologdb.runtime.playground.jvm.persistence;

import com.fasterxml.jackson.annotation.JsonAnyGetter;
import com.fasterxml.jackson.annotation.JsonAnySetter;
import com.fasterxml.jackson.annotation.JsonProperty;

import java.util.HashMap;
import java.util.Map;

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

    private WindowState mainWindowState = new WindowState();

    private Map<String, Object> unknownProperties = null;

    @JsonProperty
    @Deprecated
    public String getGraphicsDeviceID() {
        return mainWindowState.getGraphicsDeviceID();
    }

    @JsonProperty
    @Deprecated
    public void setGraphicsDeviceID(String graphicsDeviceID) {
        mainWindowState.setGraphicsDeviceID(graphicsDeviceID);
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
    public WindowState getMainWindowState()
    {
        return mainWindowState;
    }

    @JsonProperty
    public void setMainWindowState(WindowState mainWindowState)
    {
        this.mainWindowState = mainWindowState;
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

    @JsonAnyGetter
    public Map<String, Object> getUnknownProperties() {
        return unknownProperties;
    }

    @JsonAnySetter
    public void setUnknownProperties(Map<String, Object> unknownProperties) {
        this.unknownProperties = new HashMap<>(unknownProperties);

        // deprecated fields that are to be removed
        this.unknownProperties.remove("selectedLibraries");
    }
}
