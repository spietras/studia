package com.spietras.picgallery.search.models;

import com.spietras.picgallery.models.picdata.PictureData;
import javafx.scene.image.Image;

public class PictureTile
{
    private final PictureData data;
    private final Image previewImage;

    public PictureTile(PictureData data, Image previewImage)
    {
        this.data = data;
        this.previewImage = previewImage;
    }

    public PictureData getData() { return data; }

    public Image getPreviewImage() { return previewImage; }
}
