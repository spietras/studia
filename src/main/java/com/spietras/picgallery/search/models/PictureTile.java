package com.spietras.picgallery.search.models;

import com.spietras.picgallery.search.models.picdata.PictureData;
import javafx.scene.image.Image;

public class PictureTile
{
    private final PictureData data;
    private final Image previewImage;

    public PictureTile(PictureData data)
    {
        this.data = data;
        previewImage = new Image(data.getPreviewURL()); //download preview from url
    }

    public PictureData getData() { return data; }

    public Image getPreviewImage() { return previewImage; }
}
