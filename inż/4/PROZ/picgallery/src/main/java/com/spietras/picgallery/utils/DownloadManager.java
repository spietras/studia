package com.spietras.picgallery.utils;

import javafx.scene.image.Image;

import java.io.IOException;
import java.io.InputStream;
import java.net.HttpURLConnection;

public class DownloadManager
{
    private final ConnectionManager connectionManager;

    public DownloadManager(ConnectionManager connectionManager)
    {
        this.connectionManager = connectionManager;
    }

    public Image downloadImage(String url) throws IOException
    {
        HttpURLConnection connection = connectionManager.getConnection(url);
        try(InputStream is = connection.getInputStream())
        {
            return new Image(is);
        }
        finally
        {
            connection.disconnect();
        }
    }
}
