package com.spietras.picgallery.search.models.picdata.pixabayData;

import retrofit2.Call;
import retrofit2.http.GET;
import retrofit2.http.Query;

public interface PixabayEndpointAPI
{
    /**
     * Queries Pixabay for pictures
     *
     * @param query   what to query for
     * @param pageNum which page
     * @param perPage how many results per page
     */
    @GET("api/?image_type=photo")
    Call<PixabaySearchResult> searchPictures(@Query("q") String query, @Query("page") int pageNum,
                                             @Query("per_page") int perPage);
}
