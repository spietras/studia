package com.spietras.picgallery.search.models;

import com.spietras.picgallery.search.models.picdata.PictureData;
import com.spietras.picgallery.search.models.picdata.PictureProvider;
import org.junit.jupiter.api.Test;

import java.io.IOException;
import java.util.Collections;
import java.util.List;

import static org.junit.jupiter.api.Assertions.assertEquals;
import static org.junit.jupiter.api.Assertions.assertTrue;
import static org.mockito.Mockito.*;

class SearchDataModelTest
{
    @Test
    void testLoadPicturesChunk() throws IOException
    {
        //is it returning the data i provided

        String query = "flower";
        List<PictureData> expected = Collections.nCopies(10, mock(PictureData.class));

        PictureProvider provider = mock(PictureProvider.class);
        doReturn(expected).when(provider).loadPicturesDataChunk(anyString(), anyInt());

        SearchDataModel model = new SearchDataModel(provider);
        model.loadPicturesChunk(query);

        assertEquals(expected, model.getPictures());
    }

    @Test
    void testPicturesEnded() throws IOException
    {
        //does it set flag that pictures ended

        String query = "flower";
        List<PictureData> expected = Collections.nCopies(10, mock(PictureData.class));

        PictureProvider provider = mock(PictureProvider.class);
        doReturn(expected).when(provider).loadPicturesDataChunk(anyString(), anyInt());
        doReturn(true).when(provider).dataEnded();

        SearchDataModel model = new SearchDataModel(provider);
        model.loadPicturesChunk(query);

        assertEquals(expected, model.getPictures());
        assertTrue(model.endOfPictures().getValue());
    }

    @Test
    void testLoadPicturesChunkMultiple() throws IOException
    {
        //does it loads proper data across chunks

        String query = "flower";
        List<PictureData> expected = Collections.nCopies(10, mock(PictureData.class));

        PictureProvider provider = mock(PictureProvider.class);
        doReturn(expected.subList(0, 5)).when(provider).loadPicturesDataChunk(anyString(), anyInt());
        doReturn(query).when(provider).getCurrentQuery();
        doReturn(false).when(provider).dataEnded();

        SearchDataModel model = new SearchDataModel(provider);
        model.loadPicturesChunk(query);

        doReturn(expected.subList(5, 10)).when(provider).loadPicturesDataChunk(anyString(), anyInt());
        doReturn(true).when(provider).dataEnded();

        model.loadPicturesChunk(query);

        assertEquals(expected, model.getPictures());
    }

    @Test
    void testClearPictures() throws IOException
    {
        //does it properly clears data

        String query = "flower";
        List<PictureData> expected = Collections.nCopies(10, mock(PictureData.class));

        PictureProvider provider = mock(PictureProvider.class);
        doReturn(expected).when(provider).loadPicturesDataChunk(anyString(), anyInt());
        doReturn(true).when(provider).dataEnded();

        SearchDataModel model = new SearchDataModel(provider);
        model.loadPicturesChunk(query);

        assertEquals(expected, model.getPictures());

        model.clearPictures();

        assertTrue(model.getPictures().isEmpty());
    }
}