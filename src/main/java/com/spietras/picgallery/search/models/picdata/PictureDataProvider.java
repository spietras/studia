package com.spietras.picgallery.search.models.picdata;

import java.io.IOException;
import java.util.List;

public interface PictureDataProvider
{
    /**
     * Loads next chunk of data of given size
     *
     * @param query     what to query for
     * @param chunkSize size of chunk
     * @throws IOException when can't load
     */
    List<? extends PictureData> loadPicturesDataChunk(String query, int chunkSize) throws IOException;

    /**
     * @return true if there is no more data for current query
     */
    boolean dataEnded();

    /**
     * Resets provider to its clean stage
     */
    void clearCache();

    String getCurrentQuery();
}
