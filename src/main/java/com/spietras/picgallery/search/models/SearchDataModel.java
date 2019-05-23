package com.spietras.picgallery.search.models;

import com.spietras.picgallery.models.picdata.PictureData;
import com.spietras.picgallery.models.picdata.PictureDataProvider;
import com.spietras.picgallery.utils.DownloadHelper;
import com.spietras.picgallery.utils.ExecutorManager;
import javafx.application.Platform;
import javafx.beans.property.BooleanProperty;
import javafx.beans.property.SimpleBooleanProperty;
import javafx.collections.FXCollections;
import javafx.collections.ObservableList;
import javafx.scene.image.Image;

import java.io.IOException;
import java.util.concurrent.ExecutorService;
import java.util.concurrent.Executors;

public class SearchDataModel
{
    private final ObservableList<PictureTile> pictures = FXCollections.observableArrayList();
    private final BooleanProperty picturesEnded = new SimpleBooleanProperty(false);
    private final PictureDataProvider provider;
    private ExecutorService downloadPreviewsMultiThreadExecutor = Executors.newCachedThreadPool();

    public SearchDataModel(PictureDataProvider provider) { this.provider = provider; }

    public ObservableList<PictureTile> getPictures() { return pictures; }

    public BooleanProperty endOfPictures() { return picturesEnded; }

    /**
     * Loads chunk of pictures with given query and size
     *
     * @param query     what to query for
     * @param chunkSize chunk size
     * @throws IOException when can't load
     */
    public void loadPicturesChunk(String query, int chunkSize) throws IOException
    {
        for(PictureData data : provider.loadPicturesDataChunk(query,
                                                              chunkSize)) //creating new tiles takes some time, so run it in separate threads
        {
            downloadPreviewsMultiThreadExecutor.submit(() -> {
                Image previewImage; //download preview from url
                try
                {
                    previewImage = DownloadHelper.downloadImage(data.getPreviewURL());
                }
                catch(IOException e)
                {
                    e.printStackTrace();
                    return;
                }
                PictureTile tile = new PictureTile(data, previewImage);
                Platform.runLater(() -> pictures.add(tile)); //update list on UI thread

                if(Thread.currentThread().isInterrupted())
                    Platform.runLater(() -> pictures.remove(tile)); //update list on UI thread
            });
        }

        ExecutorManager.shutdownExecutor(downloadPreviewsMultiThreadExecutor, 60); //wait for threads to finish
        downloadPreviewsMultiThreadExecutor = Executors.newCachedThreadPool();

        if(provider.dataEnded())
            Platform.runLater(() -> picturesEnded.setValue(true));
    }

    /**
     * Clears all previously loaded pictures
     */
    public void clearPictures()
    {
        provider.clearCache();
        Platform.runLater(() -> {
            pictures.clear();
            picturesEnded.setValue(false);
        });
        ExecutorManager.shutdownExecutor(downloadPreviewsMultiThreadExecutor, 60);
        downloadPreviewsMultiThreadExecutor = Executors.newCachedThreadPool();
    }

    public String getCurrentQuery() { return provider.getCurrentQuery(); }

    public void shutdown(long timeout) throws IllegalThreadStateException
    {
        ExecutorManager.abortExecutor(downloadPreviewsMultiThreadExecutor, timeout);
    }
}
