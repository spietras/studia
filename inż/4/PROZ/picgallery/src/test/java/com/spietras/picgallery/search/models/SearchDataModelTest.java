package com.spietras.picgallery.search.models;

import com.spietras.picgallery.models.picdata.PictureData;
import com.spietras.picgallery.models.picdata.PictureDataProvider;
import com.spietras.picgallery.utils.DownloadManager;
import com.spietras.picgallery.utils.ExecutorHelper;
import javafx.embed.swing.JFXPanel;
import org.junit.jupiter.api.Test;

import java.io.IOException;
import java.util.Collections;
import java.util.List;

import static org.junit.jupiter.api.Assertions.assertEquals;
import static org.junit.jupiter.api.Assertions.assertTrue;
import static org.mockito.Mockito.*;

class SearchDataModelTest
{
    private final String dummyQuery = "flower";
    private final String dummyPreviewUrl = "/dummyImage.png";
    private JFXPanel panel = new JFXPanel(); //necessary to access JavaFX components

    @Test
    void testLoadPicturesChunk() throws IOException, InterruptedException
    {
        //is it returning the data i provided

        int chunkSize = 10;
        List<PictureData> dummyDataList = mockDataList(chunkSize);
        PictureDataProvider provider = mockProvider(dummyDataList);


        SearchDataModel model = new SearchDataModel(provider, mock(DownloadManager.class));
        model.loadPicturesChunk(dummyQuery, chunkSize);
        ExecutorHelper.waitForRunLater();


        assertEquals(chunkSize, model.getPictures().size());
        assertTrue(model.getPictures().stream().allMatch(x -> x.getData().getPreviewURL().equals(dummyPreviewUrl)));
    }

    @Test
    void testPicturesEnded() throws IOException, InterruptedException
    {
        //does it set flag that pictures ended

        int dataSize = 1;
        List<PictureData> dummyDataList = mockDataList(dataSize);
        PictureDataProvider provider = mockProvider(dummyDataList);
        doReturn(true).when(provider).dataEnded();


        SearchDataModel model = new SearchDataModel(provider, mock(DownloadManager.class));
        model.loadPicturesChunk(dummyQuery, dataSize + 1);
        ExecutorHelper.waitForRunLater();


        assertTrue(model.endOfPictures().getValue());
    }

    @Test
    void testLoadPicturesChunkMultiple() throws IOException, InterruptedException
    {
        //does it loads proper data across chunks

        int dataSize = 10, firstSize = 5;
        List<PictureData> dummyDataList = mockDataList(dataSize);
        PictureDataProvider provider = mockProvider(dummyDataList.subList(0, firstSize));
        doReturn(false).when(provider).dataEnded();


        SearchDataModel model = new SearchDataModel(provider, mock(DownloadManager.class));
        model.loadPicturesChunk(dummyQuery, firstSize);


        doReturn(dummyDataList.subList(firstSize, dataSize)).when(provider)
                                                               .loadPicturesDataChunk(anyString(), anyInt());
        doReturn(true).when(provider).dataEnded();


        model.loadPicturesChunk(dummyQuery, dataSize - (firstSize));
        ExecutorHelper.waitForRunLater();


        assertEquals(dataSize, model.getPictures().size());
        assertTrue(model.getPictures().stream().allMatch(x -> x.getData().getPreviewURL().equals(dummyPreviewUrl)));
    }

    @Test
    void testClearPictures() throws IOException, InterruptedException
    {
        //does it properly clears data

        int dataSize = 10;
        List<PictureData> dummyDataList = mockDataList(dataSize);
        PictureDataProvider provider = mockProvider(dummyDataList);
        doReturn(true).when(provider).dataEnded();


        SearchDataModel model = new SearchDataModel(provider, mock(DownloadManager.class));
        model.loadPicturesChunk(dummyQuery, dataSize);
        ExecutorHelper.waitForRunLater();


        assertEquals(dataSize, model.getPictures().size());
        assertTrue(model.getPictures().stream().allMatch(x -> x.getData().getPreviewURL().equals(dummyPreviewUrl)));


        model.clearPictures();
        ExecutorHelper.waitForRunLater();


        assertTrue(model.getPictures().isEmpty());
    }

    private List<PictureData> mockDataList(int dataSize)
    {
        PictureData dummyData = mock(PictureData.class);
        doReturn(dummyPreviewUrl).when(dummyData).getPreviewURL();
        return Collections.nCopies(dataSize, dummyData);
    }

    private PictureDataProvider mockProvider(List<PictureData> dummyDataList) throws IOException
    {
        PictureDataProvider provider = mock(PictureDataProvider.class);
        doReturn(dummyDataList).when(provider).loadPicturesDataChunk(anyString(), anyInt());
        return provider;
    }
}