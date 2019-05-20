package com.spietras.picgallery.search.models;

import com.spietras.picgallery.search.models.picdata.PictureData;
import javafx.scene.image.Image;

public class PictureTile
{
    private PictureData data;
    private Image previewImage;

    public PictureTile(PictureData data)
    {
        this.data = data;
        previewImage = new Image(data.getPreviewURL());
    }

    public PictureData getData() { return data; }

    public Image getPreviewImage() { return previewImage; }
}
