package com.spietras.picgallery.search.models;

import com.spietras.picgallery.search.models.picdata.PictureData;
import com.spietras.picgallery.search.models.picdata.PictureDataProvider;
import com.spietras.picgallery.search.utils.ExecutorManager;
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
    JFXPanel panel = new JFXPanel(); //necessary to access JavaFX components

    @Test
    void testLoadPicturesChunk() throws IOException, InterruptedException
    {
        //is it returning the data i provided

        String query = "flower", DUMMY_PREVIEW_URL = "/dummyImage.png";
        int chunkSize = 10;

        PictureData dummyData = mock(PictureData.class);
        doReturn(DUMMY_PREVIEW_URL).when(dummyData).getPreviewURL();
        List<PictureData> dummyDataList = Collections.nCopies(chunkSize, dummyData);

        PictureDataProvider provider = mock(PictureDataProvider.class);
        doReturn(dummyDataList).when(provider).loadPicturesDataChunk(anyString(), anyInt());

        SearchDataModel model = new SearchDataModel(provider);
        model.loadPicturesChunk(query, chunkSize);

        ExecutorManager.waitForRunLater();

        assertEquals(chunkSize, model.getPictures().size());
        assertTrue(model.getPictures().stream().allMatch(x -> x.getData().getPreviewURL().equals(DUMMY_PREVIEW_URL)));
    }

    @Test
    void testPicturesEnded() throws IOException, InterruptedException
    {
        //does it set flag that pictures ended

        String query = "flower", DUMMY_PREVIEW_URL = "/dummyImage.png";
        int dataSize = 1;

        PictureData dummyData = mock(PictureData.class);
        doReturn(DUMMY_PREVIEW_URL).when(dummyData).getPreviewURL();
        List<PictureData> dummyDataList = Collections.nCopies(dataSize, dummyData);

        PictureDataProvider provider = mock(PictureDataProvider.class);
        doReturn(dummyDataList).when(provider).loadPicturesDataChunk(anyString(), anyInt());
        doReturn(true).when(provider).dataEnded();

        SearchDataModel model = new SearchDataModel(provider);
        model.loadPicturesChunk(query, dataSize + 1);

        ExecutorManager.waitForRunLater();

        assertTrue(model.endOfPictures().getValue());
    }

    @Test
    void testLoadPicturesChunkMultiple() throws IOException, InterruptedException
    {
        //does it loads proper data across chunks

        String query = "flower", DUMMY_PREVIEW_URL = "/dummyImage.png";
        int dataSize = 10;

        PictureData dummyData = mock(PictureData.class);
        doReturn(DUMMY_PREVIEW_URL).when(dummyData).getPreviewURL();
        List<PictureData> dummyDataList = Collections.nCopies(dataSize, dummyData);

        PictureDataProvider provider = mock(PictureDataProvider.class);
        doReturn(dummyDataList.subList(0, dataSize / 2)).when(provider).loadPicturesDataChunk(anyString(), anyInt());
        doReturn(query).when(provider).getCurrentQuery();
        doReturn(false).when(provider).dataEnded();

        SearchDataModel model = new SearchDataModel(provider);
        model.loadPicturesChunk(query, dataSize / 2);

        doReturn(dummyDataList.subList(dataSize / 2, dataSize)).when(provider)
                                                               .loadPicturesDataChunk(anyString(), anyInt());
        doReturn(true).when(provider).dataEnded();

        model.loadPicturesChunk(query, dataSize - (dataSize / 2));

        ExecutorManager.waitForRunLater();

        assertEquals(dataSize, model.getPictures().size());
        assertTrue(model.getPictures().stream().allMatch(x -> x.getData().getPreviewURL().equals(DUMMY_PREVIEW_URL)));
    }

    @Test
    void testClearPictures() throws IOException, InterruptedException
    {
        //does it properly clears data

        String query = "flower", DUMMY_PREVIEW_URL = "/dummyImage.png";
        int dataSize = 10;

        PictureData dummyData = mock(PictureData.class);
        doReturn(DUMMY_PREVIEW_URL).when(dummyData).getPreviewURL();
        List<PictureData> dummyDataList = Collections.nCopies(dataSize, dummyData);

        PictureDataProvider provider = mock(PictureDataProvider.class);
        doReturn(dummyDataList).when(provider).loadPicturesDataChunk(anyString(), anyInt());
        doReturn(true).when(provider).dataEnded();

        SearchDataModel model = new SearchDataModel(provider);
        model.loadPicturesChunk(query, dataSize);

        ExecutorManager.waitForRunLater();

        assertEquals(dataSize, model.getPictures().size());
        assertTrue(model.getPictures().stream().allMatch(x -> x.getData().getPreviewURL().equals(DUMMY_PREVIEW_URL)));

        model.clearPictures();

        ExecutorManager.waitForRunLater();

        assertTrue(model.getPictures().isEmpty());
    }
}