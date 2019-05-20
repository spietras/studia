package com.spietras.picgallery.search.models.picdata.pixabayData;

import java.io.IOException;

public class PixabayRateLimitException extends IOException
{
    private String query;
    private int limit;
    private int reset;

    public PixabayRateLimitException(String query, int limit, int reset)
    {
        super();
        this.query = query;
        this.limit = limit;
        this.reset = reset;
    }

    public PixabayRateLimitException(String query, int limit, int reset, String message)
    {
        super(message);
        this.query = query;
        this.limit = limit;
        this.reset = reset;
    }

    public PixabayRateLimitException(String query, int limit, int reset, String message, Throwable cause)
    {
        super(message, cause);
        this.query = query;
        this.limit = limit;
        this.reset = reset;
    }

    public PixabayRateLimitException(String query, int limit, int reset, Throwable cause)
    {
        super(cause);
        this.query = query;
        this.limit = limit;
        this.reset = reset;
    }

    public String getQuery() { return query; }

    public int getLimit() { return limit; }

    public int getReset() { return reset; }
}
