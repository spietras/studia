package com.spietras.picgallery.picdetails.models;

import com.spietras.picgallery.models.picdata.PictureData;
import com.spietras.picgallery.utils.DownloadHelper;
import javafx.scene.image.Image;

import java.io.IOException;

public class PicDetailsModel
{
    private final PictureData data;
    private Image fullImage;

    public PicDetailsModel(PictureData data)
    {
        this.data = data;
    }

    public PictureData getData() { return data; }

    public Image getFullImage() throws IOException
    {
        if(fullImage == null) //if image not set, download it
            fullImage = DownloadHelper.downloadImage(data.getLargeImageURL());

        return fullImage;
    }
}
