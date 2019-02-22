package com.github.prologdb.runtime.playground.jvm.persistence;

import com.fasterxml.jackson.annotation.JsonProperty;

public class WindowState
{
    private Integer locationX;

    private Integer locationY;

    private Integer width;

    private Integer height;

    /**
     * The graphics device last shown on (multi-monitor support)
     */
    private String graphicsDeviceID;

    @JsonProperty
    public Integer getLocationX()
    {
        return locationX;
    }

    @JsonProperty
    public void setLocationX(Integer locationX)
    {
        this.locationX = locationX;
    }

    @JsonProperty
    public Integer getLocationY()
    {
        return locationY;
    }

    @JsonProperty
    public void setLocationY(Integer locationY)
    {
        this.locationY = locationY;
    }

    @JsonProperty
    public Integer getWidth()
    {
        return width;
    }

    @JsonProperty
    public void setWidth(Integer width)
    {
        this.width = width;
    }

    @JsonProperty
    public Integer getHeight()
    {
        return height;
    }

    @JsonProperty
    public void setHeight(Integer height)
    {
        this.height = height;
    }

    @JsonProperty
    public String getGraphicsDeviceID()
    {
        return graphicsDeviceID;
    }

    @JsonProperty
    public void setGraphicsDeviceID(String graphicsDeviceID)
    {
        this.graphicsDeviceID = graphicsDeviceID;
    }
}
