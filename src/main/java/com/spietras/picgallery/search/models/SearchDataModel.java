package com.spietras.picgallery.search.models;

import com.spietras.picgallery.search.models.picdata.PictureData;
import com.spietras.picgallery.search.models.picdata.PictureDataProvider;
import com.spietras.picgallery.search.utils.ExecutorManager;
import javafx.application.Platform;
import javafx.beans.property.BooleanProperty;
import javafx.beans.property.SimpleBooleanProperty;
import javafx.collections.FXCollections;
import javafx.collections.ObservableList;

import java.io.IOException;
import java.util.concurrent.ExecutorService;
import java.util.concurrent.Executors;

public class SearchDataModel
{
    private ObservableList<PictureTile> pictures = FXCollections.observableArrayList();
    private BooleanProperty picturesEnded = new SimpleBooleanProperty(false);
    private PictureDataProvider provider;
    private ExecutorService downloadPreviewsMultiThreadExecutor = Executors.newCachedThreadPool();

    public SearchDataModel(PictureDataProvider provider) { this.provider = provider; }

    public ObservableList<PictureTile> getPictures() { return pictures; }

    public BooleanProperty endOfPictures() { return picturesEnded; }

    public void loadPicturesChunk(String query, int chunkSize) throws IOException
    {
        for(PictureData data : provider.loadPicturesDataChunk(query, chunkSize))
        {
            downloadPreviewsMultiThreadExecutor.submit(() -> {
                PictureTile tile = new PictureTile(data);
                Platform.runLater(() -> pictures.add(tile));

                if(Thread.currentThread().isInterrupted())
                    Platform.runLater(() -> pictures.remove(tile));
            });
        }

        ExecutorManager.shutdownExecutor(downloadPreviewsMultiThreadExecutor, 60);
        downloadPreviewsMultiThreadExecutor = Executors.newCachedThreadPool();

        if(provider.dataEnded())
            picturesEnded.setValue(true);
    }

    public void clearPictures()
    {
        provider.clearCache();
        Platform.runLater(() -> pictures.clear());
        ExecutorManager.shutdownExecutor(downloadPreviewsMultiThreadExecutor, 60);
        downloadPreviewsMultiThreadExecutor = Executors.newCachedThreadPool();
    }

    public String getCurrentQuery() { return provider.getCurrentQuery(); }
}
