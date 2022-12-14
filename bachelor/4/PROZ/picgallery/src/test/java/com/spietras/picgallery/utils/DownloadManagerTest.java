package com.spietras.picgallery.utils;

import javafx.embed.swing.JFXPanel;
import javafx.scene.image.Image;
import org.junit.jupiter.api.Test;

import java.io.FileInputStream;
import java.io.IOException;
import java.io.InputStream;
import java.net.HttpURLConnection;

import static org.junit.jupiter.api.Assertions.assertEquals;
import static org.junit.jupiter.api.Assertions.assertThrows;
import static org.mockito.Mockito.*;

@SuppressWarnings("FieldCanBeLocal")
class DownloadManagerTest
{
    private final String dummyUrl = "http://example.com";
    private final String dummyImage = "/dummyImage.png";
    private JFXPanel panel = new JFXPanel(); //necessary to access JavaFX components

    @Test
    void downloadImage() throws IOException
    {
        //is it returning proper image

        Image expected = new Image(dummyImage);

        HttpURLConnection dummyConnection = mock(HttpURLConnection.class);
        InputStream is = getClass().getResourceAsStream(dummyImage);
        doReturn(is).when(dummyConnection).getInputStream();
        ConnectionManager dummyConnectionManager = mock(ConnectionManager.class);
        doReturn(dummyConnection).when(dummyConnectionManager).getConnection(anyString());

        DownloadManager dm = new DownloadManager(dummyConnectionManager);

        Image im = dm.downloadImage(dummyUrl);

        assertEquals(expected.getWidth(), im.getWidth());
        assertEquals(expected.getHeight(), im.getHeight());
    }

    @Test
    void downloadImageNoConnection() throws IOException
    {
        //does it throw when there is no connection

        ConnectionManager dummyConnectionManager = mock(ConnectionManager.class);
        doThrow(IOException.class).when(dummyConnectionManager).getConnection(anyString());

        DownloadManager dm = new DownloadManager(dummyConnectionManager);

        assertThrows(IOException.class, () -> dm.downloadImage(dummyUrl));
    }

    @SuppressWarnings("ResultOfMethodCallIgnored")
    @Test
    void downloadImageNoRead() throws IOException
    {
        //does it throw when we can't read input

        InputStream dummyStream = mock(FileInputStream.class);
        doThrow(IOException.class).when(dummyStream).read();
        doThrow(IOException.class).when(dummyStream).read(any());
        doThrow(IOException.class).when(dummyStream).read(any(), anyInt(), anyInt());
        doThrow(IOException.class).when(dummyStream).close();
        HttpURLConnection dummyConnection = mock(HttpURLConnection.class);
        doReturn(dummyStream).when(dummyConnection).getInputStream();
        ConnectionManager dummyConnectionManager = mock(ConnectionManager.class);
        doReturn(dummyConnection).when(dummyConnectionManager).getConnection(anyString());

        DownloadManager dm = new DownloadManager(dummyConnectionManager);

        assertThrows(IOException.class, () -> dm.downloadImage(dummyUrl));
    }
}