package com.spietras.picgallery.picdetails.models;

import com.spietras.picgallery.models.picdata.PictureData;
import javafx.scene.image.Image;

public class PicDetailsModel
{
    private final PictureData data;
    private Image fullImage;

    public PicDetailsModel(PictureData data)
    {
        this.data = data;
    }

    public PictureData getData() { return data; }

    public Image getFullImage()
    {
        if(fullImage == null) //if image not set, download it
            fullImage = new Image(data.getLargeImageURL());

        return fullImage;
    }
}
