package com.spietras.picgallery.search.models.picdata.pixabayData;

import com.spietras.picgallery.search.models.picdata.PictureDataProvider;
import retrofit2.Call;
import retrofit2.Response;

import java.io.IOException;
import java.util.*;

public class PixabayPictureDataProvider implements PictureDataProvider
{
    private Queue<PixabayPictureData> cachedData = new LinkedList<>();
    private int currentPage = 0;
    private int totalHits;
    private int currentHits = 0;
    private boolean dataEnded = false;
    private boolean firstCall = true;
    private String currentQuery = null;
    private PixabayEndpointAPI pixabayService;

    public PixabayPictureDataProvider(PixabayEndpointAPI service)
    {
        pixabayService = service;
    }

    @Override
    public List<PixabayPictureData> loadPicturesDataChunk(String query, int chunkSize) throws IOException
    {
        if(!query.equalsIgnoreCase(currentQuery)) //another query, clean everything
            clearCache();

        if(dataEnded())
            return Collections.emptyList();

        List<PixabayPictureData> data = getChunk(query, chunkSize);

        if(cachedData.size() == 0 && currentHits >= totalHits) //if cache is empty and we used all hits then data ended
            dataEnded = true;

        return data;
    }

    @Override
    public boolean dataEnded()
    {
        return dataEnded;
    }

    @Override
    public void clearCache()
    {
        cachedData.clear();
        currentPage = 0;
        currentHits = 0;
        dataEnded = false;
        firstCall = true;
    }

    @Override
    public String getCurrentQuery()
    {
        return currentQuery;
    }

    private List<PixabayPictureData> getChunk(String query, int chunkSize) throws IOException
    {
        List<PixabayPictureData> data = new ArrayList<>();

        for(int i = 0; i < chunkSize; i++) //we need to return chunkSize of elements
        {
            if(cachedData.size() == 0 && handleEmptyQueue(query)) //cache ended
                return data;

            PixabayPictureData polled = cachedData.poll();
            if(polled != null) //is queue is empty, don't add nulls (useful when there is less data than chunk size)
                data.add(polled);
        }

        return data;
    }

    /**
     * @return true when data ended
     */
    private boolean handleEmptyQueue(String query) throws IOException
    {
        if(!firstCall && currentHits >= totalHits) //if data ended, return what we have
        {
            dataEnded = true;
            return true;
        }

        cachedData.addAll(loadNewData(query)); //load new data
        firstCall = false;
        currentQuery = query;
        return false;
    }

    private List<PixabayPictureData> loadNewData(String query) throws IOException
    {
        int perPage = 50;

        Call<PixabaySearchResult> call = pixabayService.searchPictures(query, ++currentPage, perPage);
        Response<PixabaySearchResult> response = call.execute();

        checkResponse(response, query);

        if(firstCall)
            totalHits = response.body().getTotalHits();

        currentHits += response.body().getHits().size();

        return response.body().getHits();
    }

    private void checkResponse(Response<PixabaySearchResult> response, String query) throws IOException
    {
        if(response.code() == 429)
            throw new PixabayRateLimitException(query, Integer.parseInt(response.headers().get("X-RateLimit-Limit")),
                                                Integer.parseInt(response.headers().get("X-RateLimit-Reset")));
        else if(!response.isSuccessful() && response.errorBody() != null)
            throw new IOException("Unsuccessful response from Pixabay server: " + response.errorBody().string());
        else if(response.body() == null)
            throw new IOException("Response is successful but the body is null for some reason idk ;(");
    }
}
