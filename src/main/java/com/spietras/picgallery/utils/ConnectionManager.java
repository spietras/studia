package com.spietras.picgallery.utils;

import java.io.IOException;
import java.net.HttpURLConnection;
import java.net.URL;

public class ConnectionManager
{
    private final int timeout;

    public ConnectionManager(int timeout)
    {
        this.timeout = timeout;
    }

    public HttpURLConnection getConnection(String url) throws IOException
    {
        HttpURLConnection connection = (HttpURLConnection) new URL(url).openConnection();
        connection.setConnectTimeout(timeout);
        connection.setReadTimeout(timeout);
        return connection;
    }
}
