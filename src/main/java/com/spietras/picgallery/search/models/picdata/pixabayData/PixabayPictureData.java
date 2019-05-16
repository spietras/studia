package com.spietras.picgallery.search.models.picdata.pixabayData;

import com.google.gson.annotations.Expose;
import com.google.gson.annotations.SerializedName;
import com.spietras.picgallery.search.models.picdata.PictureData;

public class PixabayPictureData implements PictureData
{
    @SerializedName("tags")
    @Expose
    private String tags;
    @SerializedName("previewURL")
    @Expose
    private String previewURL;
    @SerializedName("largeImageURL")
    @Expose
    private String largeImageURL;
    @SerializedName("views")
    @Expose
    private int views;
    @SerializedName("downloads")
    @Expose
    private int downloads;
    @SerializedName("likes")
    @Expose
    private int likes;
    @SerializedName("user")
    @Expose
    private String user;

    @Override
    public String getTags()
    {
        return tags;
    }

    @Override
    public String getPreviewURL()
    {
        return previewURL;
    }

    @Override
    public String getLargeImageURL()
    {
        return largeImageURL;
    }

    @Override
    public int getViews()
    {
        return views;
    }

    @Override
    public int getDownloads()
    {
        return downloads;
    }

    @Override
    public int getLikes()
    {
        return likes;
    }

    @Override
    public String getUser()
    {
        return user;
    }
}