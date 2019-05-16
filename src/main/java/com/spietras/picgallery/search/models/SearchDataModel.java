package com.spietras.picgallery.search.models;

import com.spietras.picgallery.search.models.picdata.PictureData;
import com.spietras.picgallery.search.models.picdata.PictureProvider;
import javafx.beans.property.BooleanProperty;
import javafx.beans.property.SimpleBooleanProperty;
import javafx.collections.FXCollections;
import javafx.collections.ObservableList;

import java.io.IOException;

public class SearchDataModel
{
    private ObservableList<PictureData> pictures = FXCollections.observableArrayList();
    private BooleanProperty picturesEnded = new SimpleBooleanProperty(false);
    private PictureProvider provider;

    public SearchDataModel(PictureProvider prov) { provider = prov; }

    public ObservableList<PictureData> getPictures() { return pictures; }

    public BooleanProperty endOfPictures() { return picturesEnded; }

    public void loadPicturesChunk(String query) throws IOException
    {
        int perChunk = 10;

        if(!query.equalsIgnoreCase(provider.getCurrentQuery()))
            clearPictures();

        pictures.addAll(provider.loadPicturesDataChunk(query, perChunk));

        if(provider.dataEnded())
            picturesEnded.setValue(true);
    }

    public void clearPictures()
    {
        provider.clearCache();
        pictures.clear();
    }
}
