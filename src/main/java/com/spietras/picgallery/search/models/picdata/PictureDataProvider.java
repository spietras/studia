package com.spietras.picgallery.search.models.picdata;

import java.io.IOException;
import java.util.List;

public interface PictureDataProvider
{
    List<? extends PictureData> loadPicturesDataChunk(String query, int chunkSize) throws IOException;

    boolean dataEnded();

    void clearCache();

    String getCurrentQuery();
}
