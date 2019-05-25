package com.spietras.picgallery.models.picdata.pixabayData;

import com.spietras.picgallery.models.picdata.PictureDataProvider;
import retrofit2.Call;
import retrofit2.Response;

import java.io.IOException;
import java.io.InterruptedIOException;
import java.util.*;

public class PixabayPictureDataProvider implements PictureDataProvider
{
    private final Queue<PixabayPictureData> cachedData = new LinkedList<>();
    private final PixabayEndpointAPI pixabayService;
    private int currentPage = 0;
    private int totalHits;
    private int currentHits = 0;
    private boolean dataEnded = false;
    private boolean firstCall = true;
    private String currentQuery = null;

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
        boolean wasFirstCall = firstCall;
        String previousQuery = currentQuery;

        try
        {
            if(pollData(data, query, chunkSize))
                return data;
        }
        catch(InterruptedIOException e) //if loading was interrupted, try to restore state
        {
            handleInterrupt(data, wasFirstCall, previousQuery);
        }

        return data;
    }

    private boolean pollData(List<PixabayPictureData> results, String query, int chunkSize) throws IOException
    {
        for(int i = 0; i < chunkSize; i++) //we need to return chunkSize of elements
        {
            if(cachedData.size() == 0 && handleEmptyQueue(query)) //cache ended
                return true;

            PixabayPictureData polled = cachedData.poll();
            if(polled != null) //is queue is empty, don't add nulls (useful when there is less data than chunk size)
                results.add(polled);
        }

        return false;
    }

    private void handleInterrupt(List<PixabayPictureData> gatheredData, boolean wasFirstCall, String previousQuery)
    {
        firstCall = wasFirstCall;
        currentQuery = previousQuery;
        cachedData.addAll(gatheredData);
        gatheredData.clear();
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

    @SuppressWarnings("ConstantConditions")
    private List<PixabayPictureData> loadNewData(String query) throws IOException
    {
        int perPage = 50;

        Call<PixabaySearchResult> call = pixabayService.searchPictures(query, currentPage + 1, perPage);
        Response<PixabaySearchResult> response = call.execute();

        checkResponse(response, query);

        updateStats(response);

        return response.body().getHits();
    }

    @SuppressWarnings("ConstantConditions")
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

    @SuppressWarnings("ConstantConditions")
    private void updateStats(Response<PixabaySearchResult> response)
    {
        currentPage++;

        if(firstCall)
            totalHits = response.body().getTotalHits();

        currentHits += response.body().getHits().size();
    }
}
