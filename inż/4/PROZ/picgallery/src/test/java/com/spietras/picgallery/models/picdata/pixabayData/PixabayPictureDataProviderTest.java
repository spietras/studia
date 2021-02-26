package com.spietras.picgallery.models.picdata.pixabayData;

import okhttp3.MediaType;
import okhttp3.ResponseBody;
import org.junit.jupiter.api.Test;
import retrofit2.Call;
import retrofit2.Response;

import java.io.IOException;
import java.util.Collections;
import java.util.List;

import static org.junit.jupiter.api.Assertions.*;
import static org.mockito.Mockito.*;

class PixabayPictureDataProviderTest
{
    private final String dummyQuery = "flowers";

    @Test
    void testLoadPicturesDataSample() throws IOException
    {
        //is it returning the data i provided

        int dataSize = 10, chunkSize = 10;
        List<PixabayPictureData> expected = Collections.nCopies(dataSize, new PixabayPictureData());
        PixabayEndpointAPI service = mockPixabayService(expected, dataSize);


        PixabayPictureDataProvider provider = new PixabayPictureDataProvider(service);
        List<PixabayPictureData> data = provider.loadPicturesDataChunk(dummyQuery, chunkSize);


        assertEquals(expected, data);
        assertTrue(provider.dataEnded());
    }

    @Test
    void testLoadPicturesDataAll() throws IOException
    {
        //does it load all data and sets ended flag

        int dataSize = 1, chunkSize = 10;
        List<PixabayPictureData> expected = Collections.nCopies(dataSize, new PixabayPictureData());
        PixabayEndpointAPI service = mockPixabayService(expected, dataSize);


        PixabayPictureDataProvider provider = new PixabayPictureDataProvider(service);

        assertFalse(provider.dataEnded());


        provider.loadPicturesDataChunk(dummyQuery, chunkSize);


        assertTrue(provider.dataEnded());
    }

    @Test
    void testLoadPicturesDataEmpty() throws IOException
    {
        //does it properly return no matches

        int dataSize = 0, chunkSize = 10;
        String query = "pictures of me being happy";
        List<PixabayPictureData> expected = Collections.emptyList();
        PixabayEndpointAPI service = mockPixabayService(expected, dataSize);


        PixabayPictureDataProvider provider = new PixabayPictureDataProvider(service);
        List<PixabayPictureData> data = provider.loadPicturesDataChunk(query, chunkSize);


        assertEquals(expected, data);
        assertTrue(provider.dataEnded());
    }

    @Test
    void testLoadPicturesDataError() throws IOException
    {
        //does it throw when error occurs

        int chunkSize = 10;
        String query = "drop table users";
        PixabayEndpointAPI service = mockPixabayServiceThatReturnsError();


        PixabayPictureDataProvider provider = new PixabayPictureDataProvider(service);


        assertThrows(IOException.class, () -> provider.loadPicturesDataChunk(query, chunkSize));
    }

    @Test
    void testLoadPicturesDataMultiple() throws IOException
    {
        //does it return proper data across chunks

        int dataSize = 10, chunkSize = 5;
        List<PixabayPictureData> expected = Collections.nCopies(dataSize, new PixabayPictureData());
        PixabayEndpointAPI service = mockPixabayService(expected, dataSize);


        PixabayPictureDataProvider provider = new PixabayPictureDataProvider(service);
        List<PixabayPictureData> data = provider.loadPicturesDataChunk(dummyQuery, chunkSize);


        assertNotEquals(expected, data);


        data.addAll(provider.loadPicturesDataChunk(dummyQuery, chunkSize));


        assertEquals(expected, data);
    }

    @Test
    void testLoadPicturesDataMultipleNotAligned() throws IOException
    {
        //does it return proper data when data quantity is less than sum of chunk sizes

        int dataSize = 17, chunkSize = 10;
        List<PixabayPictureData> expected = Collections.nCopies(dataSize, new PixabayPictureData());
        PixabayEndpointAPI service = mockPixabayService(expected, dataSize);


        PixabayPictureDataProvider provider = new PixabayPictureDataProvider(service);
        List<PixabayPictureData> data = provider.loadPicturesDataChunk(dummyQuery, chunkSize);
        data.addAll(provider.loadPicturesDataChunk(dummyQuery, chunkSize));


        assertEquals(expected, data);
        assertTrue(provider.dataEnded());
    }

    @Test
    void testLoadPicturesDataAnotherQuery() throws IOException
    {
        //does it return new data after new query

        int dataSize = 5, chunkSize = 10;
        String query1 = "cats", query2 = "dogs";
        PixabayPictureData dummyData1 = mock(PixabayPictureData.class), dummyData2 = mock(PixabayPictureData.class);
        List<PixabayPictureData> dummyDataList1 = Collections.nCopies(dataSize, dummyData1);
        List<PixabayPictureData> dummyDataList2 = Collections.nCopies(dataSize, dummyData2);
        PixabayEndpointAPI service = mockPixabayServiceForDifferentQueries(query1, dummyDataList1, query2,
                                                                           dummyDataList2);


        PixabayPictureDataProvider provider = new PixabayPictureDataProvider(service);
        List<PixabayPictureData> data = provider.loadPicturesDataChunk(query1, chunkSize);


        assertEquals(dummyDataList1, data);
        assertNotEquals(dummyDataList2, data);


        data = provider.loadPicturesDataChunk(query2, chunkSize);


        assertEquals(dummyDataList2, data);
        assertNotEquals(dummyDataList1, data);
    }

    @Test
    void testLoadPicturesDataAnotherQueryWithDroppedCache() throws IOException
    {
        //does it return new data after new query when old data is still cached

        int dataSize = 20, chunkSize = 10;
        String query1 = "cats", query2 = "dogs";
        PixabayPictureData dummyData1 = mock(PixabayPictureData.class), dummyData2 = mock(PixabayPictureData.class);
        List<PixabayPictureData> dummyDataList2 = Collections.nCopies(dataSize, dummyData2);
        List<PixabayPictureData> dummyDataList1 = Collections.nCopies(dataSize, dummyData1);
        PixabayEndpointAPI service = mockPixabayServiceForDifferentQueries(query1, dummyDataList1, query2,
                                                                           dummyDataList2);


        PixabayPictureDataProvider provider = new PixabayPictureDataProvider(service);
        List<PixabayPictureData> data = provider.loadPicturesDataChunk(query1, chunkSize);


        assertEquals(dummyDataList1.subList(0, chunkSize), data);
        assertNotEquals(dummyDataList2.subList(0, chunkSize), data);


        data = provider.loadPicturesDataChunk(query2, chunkSize);


        assertEquals(dummyDataList2.subList(0, chunkSize), data);
        assertNotEquals(dummyDataList1.subList(0, chunkSize), data);
    }

    @Test
    void testLoadPicturesDataClearCache() throws IOException
    {
        //is it returning the data i provided after clearing cache

        int dataSize = 10, chunkSize = 5;
        List<PixabayPictureData> expected = Collections.nCopies(dataSize, new PixabayPictureData());
        PixabayEndpointAPI service = mockPixabayService(expected, dataSize);


        PixabayPictureDataProvider provider = new PixabayPictureDataProvider(service);
        List<PixabayPictureData> data = provider.loadPicturesDataChunk(dummyQuery, chunkSize);


        assertEquals(expected.subList(0, chunkSize), data);
        assertFalse(provider.dataEnded());


        provider.clearCache();
        data = provider.loadPicturesDataChunk(dummyQuery, chunkSize);


        assertEquals(expected.subList(0, chunkSize), data);
        assertFalse(provider.dataEnded());
    }

    @SuppressWarnings("ResultOfMethodCallIgnored")
    private PixabayEndpointAPI mockPixabayService(List<PixabayPictureData> data, int dataCount) throws IOException
    {
        PixabayEndpointAPI service = mock(PixabayEndpointAPI.class);
        @SuppressWarnings("unchecked") Call<PixabaySearchResult> dummyCall = (Call<PixabaySearchResult>) mock(
                Call.class);
        PixabaySearchResult dummyResults = mock(PixabaySearchResult.class);
        doReturn(data).when(dummyResults).getHits();
        doReturn(dataCount).when(dummyResults).getTotalHits();
        Response<PixabaySearchResult> dummyResponse = Response.success(dummyResults);
        doReturn(dummyResponse).when(dummyCall).execute();
        doReturn(dummyResponse).when(dummyCall).execute();
        doReturn(dummyCall).when(service).searchPictures(anyString(), anyInt(), anyInt());
        return service;
    }

    @SuppressWarnings("ResultOfMethodCallIgnored")
    private PixabayEndpointAPI mockPixabayServiceForDifferentQueries(String query1, List<PixabayPictureData> data1,
                                                                     String query2,
                                                                     List<PixabayPictureData> data2) throws
                                                                                                     IOException
    {
        PixabayEndpointAPI service = mock(PixabayEndpointAPI.class);

        @SuppressWarnings("unchecked") Call<PixabaySearchResult> dummyCall1 = (Call<PixabaySearchResult>) mock(
                Call.class);
        PixabaySearchResult dummyResults1 = mock(PixabaySearchResult.class);
        doReturn(data1).when(dummyResults1).getHits();
        doReturn(data1.size()).when(dummyResults1).getTotalHits();
        Response<PixabaySearchResult> dummyResponse1 = Response.success(dummyResults1);
        doReturn(dummyResponse1).when(dummyCall1).execute();

        @SuppressWarnings("unchecked") Call<PixabaySearchResult> dummyCall2 = (Call<PixabaySearchResult>) mock(
                Call.class);
        PixabaySearchResult dummyResults2 = mock(PixabaySearchResult.class);
        doReturn(data2).when(dummyResults2).getHits();
        doReturn(data2.size()).when(dummyResults2).getTotalHits();
        Response<PixabaySearchResult> dummyResponse2 = Response.success(dummyResults2);
        doReturn(dummyResponse2).when(dummyCall2).execute();

        doReturn(dummyCall1).when(service).searchPictures(eq(query1), anyInt(), anyInt());
        doReturn(dummyCall2).when(service).searchPictures(eq(query2), anyInt(), anyInt());

        return service;
    }

    private PixabayEndpointAPI mockPixabayServiceThatReturnsError() throws IOException
    {
        PixabayEndpointAPI service = mock(PixabayEndpointAPI.class);
        @SuppressWarnings("unchecked") Call<PixabaySearchResult> dummyCall = (Call<PixabaySearchResult>) mock(
                Call.class);
        String mockedError = "{\"message\":\"mocked error\"}";
        Response<PixabaySearchResult> dummyResponse = Response.error(404, ResponseBody.create(
                MediaType.parse("application/json"), mockedError));
        doReturn(dummyResponse).when(dummyCall).execute();
        doReturn(dummyCall).when(service).searchPictures(anyString(), anyInt(), anyInt());
        return service;
    }
}