package com.spietras.picgallery.search.models.picdata.pixabayData;

import com.google.gson.Gson;
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

class PixabayPictureProviderTest
{
    @Test
    void testLoadPicturesDataSample() throws IOException
    {
        //is it returning the data i provided

        String query = "flowers";
        List<PixabayPictureData> expected = Collections.nCopies(10, new PixabayPictureData());

        PixabayEndpointAPI service = mock(PixabayEndpointAPI.class);
        @SuppressWarnings("unchecked") Call<PixabaySearchResult> dummyCall = (Call<PixabaySearchResult>) mock(
                Call.class);
        PixabaySearchResult dummyResults = mock(PixabaySearchResult.class);
        when(dummyResults.getHits()).thenReturn(expected);
        when(dummyResults.getTotalHits()).thenReturn(10);
        Response<PixabaySearchResult> dummyResponse = Response.success(dummyResults);
        when(dummyCall.execute()).thenReturn(dummyResponse);
        when(service.searchPictures(anyString(), anyInt(), anyInt())).thenReturn(dummyCall);

        PixabayPictureProvider provider = new PixabayPictureProvider(service);
        List<PixabayPictureData> data = provider.loadPicturesDataChunk(query, 10);

        assertEquals(expected, data);
        assertTrue(provider.dataEnded());
    }

    @Test
    void testLoadPicturesDataAll() throws IOException
    {
        //does it load all data and sets ended flag

        String query = "flowers";
        List<PixabayPictureData> expected = Collections.nCopies(1, new PixabayPictureData());

        PixabayEndpointAPI service = mock(PixabayEndpointAPI.class);
        @SuppressWarnings("unchecked") Call<PixabaySearchResult> dummyCall = (Call<PixabaySearchResult>) mock(
                Call.class);
        PixabaySearchResult dummyResults = mock(PixabaySearchResult.class);
        when(dummyResults.getHits()).thenReturn(expected);
        when(dummyResults.getTotal()).thenReturn(1);
        Response<PixabaySearchResult> dummyResponse = Response.success(dummyResults);
        when(dummyCall.execute()).thenReturn(dummyResponse);
        when(service.searchPictures(anyString(), anyInt(), anyInt())).thenReturn(dummyCall);

        PixabayPictureProvider provider = new PixabayPictureProvider(service);
        assertFalse(provider.dataEnded());
        provider.loadPicturesDataChunk(query, 10);
        assertTrue(provider.dataEnded());
    }

    @Test
    void testLoadPicturesDataEmpty() throws IOException
    {
        //does it properly return no matches

        String query = "pictures of me being happy";
        List<PixabayPictureData> expected = Collections.emptyList();

        PixabayEndpointAPI service = mock(PixabayEndpointAPI.class);
        @SuppressWarnings("unchecked") Call<PixabaySearchResult> dummyCall = (Call<PixabaySearchResult>) mock(
                Call.class);
        PixabaySearchResult dummyResults = mock(PixabaySearchResult.class);
        when(dummyResults.getHits()).thenReturn(expected);
        when(dummyResults.getTotal()).thenReturn(0);
        Response<PixabaySearchResult> dummyResponse = Response.success(dummyResults);
        when(dummyCall.execute()).thenReturn(dummyResponse);
        when(service.searchPictures(anyString(), anyInt(), anyInt())).thenReturn(dummyCall);

        PixabayPictureProvider provider = new PixabayPictureProvider(service);

        List<PixabayPictureData> data = provider.loadPicturesDataChunk(query, 10);
        assertEquals(expected, data);
        assertTrue(provider.dataEnded());
    }

    @Test
    void testLoadPicturesDataError() throws IOException
    {
        //does it throw when error occurs

        String query = "drop table users";

        PixabayEndpointAPI service = mock(PixabayEndpointAPI.class);
        @SuppressWarnings("unchecked") Call<PixabaySearchResult> dummyCall = (Call<PixabaySearchResult>) mock(
                Call.class);
        String mockedError = "{\"message\":\"mocked error\"}";
        Response<PixabaySearchResult> dummyResponse = Response.error(404, ResponseBody.create(
                MediaType.parse("application/json"), mockedError));
        when(dummyCall.execute()).thenReturn(dummyResponse);
        when(service.searchPictures(anyString(), anyInt(), anyInt())).thenReturn(dummyCall);

        PixabayPictureProvider provider = new PixabayPictureProvider(service);

        IOException e = assertThrows(IOException.class, () -> provider.loadPicturesDataChunk(query, 10));
    }

    @Test
    void testLoadPicturesDataMultiple() throws IOException
    {
        //does it return proper data across chunks

        String query = "flowers";
        List<PixabayPictureData> expected = Collections.nCopies(10, new PixabayPictureData());

        PixabayEndpointAPI service = mock(PixabayEndpointAPI.class);
        @SuppressWarnings("unchecked") Call<PixabaySearchResult> dummyCall = (Call<PixabaySearchResult>) mock(
                Call.class);
        PixabaySearchResult dummyResults = mock(PixabaySearchResult.class);
        when(dummyResults.getHits()).thenReturn(expected);
        when(dummyResults.getTotalHits()).thenReturn(10);
        Response<PixabaySearchResult> dummyResponse = Response.success(dummyResults);
        when(dummyCall.execute()).thenReturn(dummyResponse);
        when(service.searchPictures(anyString(), anyInt(), anyInt())).thenReturn(dummyCall);

        PixabayPictureProvider provider = new PixabayPictureProvider(service);
        List<PixabayPictureData> data = provider.loadPicturesDataChunk(query, 5);
        assertNotEquals(expected, data);
        data.addAll(provider.loadPicturesDataChunk(query, 5));
        assertEquals(expected, data);
    }

    @Test
    void testLoadPicturesDataMultipleNotAligned() throws IOException
    {
        //does it return proper data when data quantity is less than sum of chunk sizes

        String query = "flowers";
        List<PixabayPictureData> expected = Collections.nCopies(17, new PixabayPictureData());

        PixabayEndpointAPI service = mock(PixabayEndpointAPI.class);
        @SuppressWarnings("unchecked") Call<PixabaySearchResult> dummyCall = (Call<PixabaySearchResult>) mock(
                Call.class);
        PixabaySearchResult dummyResults = mock(PixabaySearchResult.class);
        when(dummyResults.getHits()).thenReturn(expected);
        when(dummyResults.getTotalHits()).thenReturn(17);
        Response<PixabaySearchResult> dummyResponse = Response.success(dummyResults);
        when(dummyCall.execute()).thenReturn(dummyResponse);
        when(service.searchPictures(anyString(), anyInt(), anyInt())).thenReturn(dummyCall);

        PixabayPictureProvider provider = new PixabayPictureProvider(service);
        List<PixabayPictureData> data = provider.loadPicturesDataChunk(query, 10);
        data.addAll(provider.loadPicturesDataChunk(query, 10));

        assertEquals(expected, data);
        assertTrue(provider.dataEnded());
    }

    @Test
    void testLoadPicturesDataAnotherQuery() throws IOException
    {
        //does it return new data after new query

        String query1 = "cats", query2 = "dogs";
        String json1 = "{\"tags\":\"cat\"}", json2 = "{\"tags\":\"dog\"}";
        Gson gson = new Gson();
        PixabayPictureData data1 = gson.fromJson(json1, PixabayPictureData.class), data2 = gson.fromJson(json2,
                                                                                                         PixabayPictureData.class);
        List<PixabayPictureData> dogs = Collections.nCopies(5, data2);
        List<PixabayPictureData> cats = Collections.nCopies(5, data1);

        PixabayEndpointAPI service = mock(PixabayEndpointAPI.class);
        @SuppressWarnings("unchecked") Call<PixabaySearchResult> dummyCall = (Call<PixabaySearchResult>) mock(
                Call.class);
        PixabaySearchResult dummyResults = mock(PixabaySearchResult.class);
        when(dummyResults.getHits()).thenReturn(cats);
        when(dummyResults.getTotalHits()).thenReturn(5);
        Response<PixabaySearchResult> dummyResponse = Response.success(dummyResults);
        when(dummyCall.execute()).thenReturn(dummyResponse);
        when(service.searchPictures(anyString(), anyInt(), anyInt())).thenReturn(dummyCall);

        PixabayPictureProvider provider = new PixabayPictureProvider(service);
        List<PixabayPictureData> data = provider.loadPicturesDataChunk(query1, 10);
        assertEquals(cats, data);
        assertNotEquals(dogs, data);

        when(dummyResults.getHits()).thenReturn(dogs);

        data = provider.loadPicturesDataChunk(query2, 10);
        assertEquals(dogs, data);
        assertNotEquals(cats, data);
    }

    @Test
    void testLoadPicturesDataAnotherQueryWithDroppedCache() throws IOException
    {
        //does it return new data after new query when old data is still cached

        String query1 = "cats", query2 = "dogs";
        String json1 = "{\"tags\":\"cat\"}", json2 = "{\"tags\":\"dog\"}";
        Gson gson = new Gson();
        PixabayPictureData data1 = gson.fromJson(json1, PixabayPictureData.class), data2 = gson.fromJson(json2,
                                                                                                         PixabayPictureData.class);
        List<PixabayPictureData> dogs = Collections.nCopies(20, data2);
        List<PixabayPictureData> cats = Collections.nCopies(20, data1);

        PixabayEndpointAPI service = mock(PixabayEndpointAPI.class);
        @SuppressWarnings("unchecked") Call<PixabaySearchResult> dummyCall = (Call<PixabaySearchResult>) mock(
                Call.class);
        PixabaySearchResult dummyResults = mock(PixabaySearchResult.class);
        when(dummyResults.getHits()).thenReturn(cats);
        when(dummyResults.getTotalHits()).thenReturn(20);
        Response<PixabaySearchResult> dummyResponse = Response.success(dummyResults);
        when(dummyCall.execute()).thenReturn(dummyResponse);
        when(service.searchPictures(anyString(), anyInt(), anyInt())).thenReturn(dummyCall);

        PixabayPictureProvider provider = new PixabayPictureProvider(service);
        List<PixabayPictureData> data = provider.loadPicturesDataChunk(query1, 10);
        assertEquals(cats.subList(0, 10), data);
        assertNotEquals(dogs.subList(0, 10), data);

        when(dummyResults.getHits()).thenReturn(dogs);

        data = provider.loadPicturesDataChunk(query2, 10);
        assertEquals(dogs.subList(0, 10), data);
        assertNotEquals(cats.subList(0, 10), data);
    }

    @Test
    void testLoadPicturesDataClearCache() throws IOException
    {
        //is it returning the data i provided

        String query = "flowers";
        List<PixabayPictureData> expected = Collections.nCopies(10, new PixabayPictureData());

        PixabayEndpointAPI service = mock(PixabayEndpointAPI.class);
        @SuppressWarnings("unchecked") Call<PixabaySearchResult> dummyCall = (Call<PixabaySearchResult>) mock(
                Call.class);
        PixabaySearchResult dummyResults = mock(PixabaySearchResult.class);
        when(dummyResults.getHits()).thenReturn(expected);
        when(dummyResults.getTotalHits()).thenReturn(10);
        Response<PixabaySearchResult> dummyResponse = Response.success(dummyResults);
        when(dummyCall.execute()).thenReturn(dummyResponse);
        when(service.searchPictures(anyString(), anyInt(), anyInt())).thenReturn(dummyCall);

        PixabayPictureProvider provider = new PixabayPictureProvider(service);
        List<PixabayPictureData> data = provider.loadPicturesDataChunk(query, 5);

        assertEquals(expected.subList(0, 5), data);
        assertFalse(provider.dataEnded());

        provider.clearCache();

        data = provider.loadPicturesDataChunk(query, 5);

        assertEquals(expected.subList(0, 5), data);
        assertFalse(provider.dataEnded());
    }
}