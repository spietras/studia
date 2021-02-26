package com.spietras.picgallery.models.picdata;

public interface PictureData
{
    String getTags();

    String getPreviewURL();

    String getLargeImageURL();

    int getViews();

    int getDownloads();

    int getLikes();

    String getUser();

    String getSourceURL();
}
