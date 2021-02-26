package com.spietras.picgallery.models.picdata.pixabayData;

import com.google.gson.annotations.Expose;
import com.google.gson.annotations.SerializedName;

import java.util.List;

public class PixabaySearchResult
{

    @SerializedName("total")
    @Expose
    private int total;
    @SerializedName("totalHits")
    @Expose
    private int totalHits;
    @SerializedName("hits")
    @Expose
    private List<PixabayPictureData> hits;

    public int getTotal()
    {
        return total;
    }

    public int getTotalHits()
    {
        return totalHits;
    }

    public List<PixabayPictureData> getHits()
    {
        return hits;
    }
}