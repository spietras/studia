package com.spietras.picgallery.utils;

import javafx.scene.image.Image;

import java.io.IOException;
import java.io.InputStream;
import java.net.URL;

public class DownloadHelper
{
    public static Image downloadImage(String url) throws IOException
    {
        try(InputStream in = new URL(url).openStream())
        {
            return new Image(in);
        }
    }
}
