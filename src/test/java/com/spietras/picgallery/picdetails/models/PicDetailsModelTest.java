package com.spietras.picgallery.picdetails.models;

import com.spietras.picgallery.models.picdata.PictureData;
import com.spietras.picgallery.utils.DownloadManager;
import javafx.scene.image.Image;
import org.junit.jupiter.api.Test;

import java.io.IOException;

import static org.junit.jupiter.api.Assertions.assertEquals;
import static org.junit.jupiter.api.Assertions.assertThrows;
import static org.mockito.Mockito.*;

class PicDetailsModelTest
{
    private final String dummyUrl = "http://example.com/";

    @Test
    void getFullImage() throws IOException
    {
        //is it returning proper image

        Image expected = mock(Image.class);

        PictureData dummyData = mock(PictureData.class);
        doReturn(dummyUrl).when(dummyData).getLargeImageURL();
        DownloadManager dummyDownloadManager = mock(DownloadManager.class);
        doReturn(expected).when(dummyDownloadManager).downloadImage(anyString());

        PicDetailsModel model = new PicDetailsModel(dummyData, dummyDownloadManager);

        assertEquals(expected, model.getFullImage());
    }

    @Test
    void getFullImageTwice() throws IOException
    {
        //is it returning the same image the second time

        Image expected = mock(Image.class);

        PictureData dummyData = mock(PictureData.class);
        doReturn(dummyUrl).when(dummyData).getLargeImageURL();
        DownloadManager dummyDownloadManager = mock(DownloadManager.class);
        doReturn(expected).when(dummyDownloadManager).downloadImage(anyString());

        PicDetailsModel model = new PicDetailsModel(dummyData, dummyDownloadManager);

        assertEquals(expected, model.getFullImage());
        assertEquals(expected, model.getFullImage());
    }

    @Test
    void getFullImageNoDownload() throws IOException
    {
        //does it throw when there is no connection

        PictureData dummyData = mock(PictureData.class);
        doReturn(dummyUrl).when(dummyData).getLargeImageURL();
        DownloadManager dummyDownloadManager = mock(DownloadManager.class);
        doThrow(IOException.class).when(dummyDownloadManager).downloadImage(anyString());

        PicDetailsModel model = new PicDetailsModel(dummyData, dummyDownloadManager);

        assertThrows(IOException.class, model::getFullImage);
    }
}