package com.spietras.picgallery.search.models.picdata;

public interface PictureData
{
    String getTags();

    String getPreviewURL();

    String getLargeImageURL();

    int getViews();

    int getDownloads();

    int getLikes();

    String getUser();
}
