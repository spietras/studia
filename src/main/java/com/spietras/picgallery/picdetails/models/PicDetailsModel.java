package com.spietras.picgallery.picdetails.models;

import com.spietras.picgallery.models.picdata.PictureData;
import com.spietras.picgallery.utils.DownloadManager;
import javafx.scene.image.Image;

import java.io.IOException;

public class PicDetailsModel
{
    private final PictureData data;
    private Image fullImage;

    private final DownloadManager downloadManager;

    public PicDetailsModel(PictureData data, DownloadManager downloadManager)
    {
        this.data = data;
        this.downloadManager = downloadManager;
    }

    public PictureData getData() { return data; }

    public Image getFullImage() throws IOException
    {
        if(fullImage == null) //if image not set, download it
            fullImage = downloadManager.downloadImage(data.getLargeImageURL());

        return fullImage;
    }
}
