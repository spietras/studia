package com.spietras.picgallery.search.models;

import com.spietras.picgallery.models.picdata.PictureData;
import com.spietras.picgallery.models.picdata.PictureDataProvider;
import com.spietras.picgallery.utils.DownloadManager;
import com.spietras.picgallery.utils.ExecutorHelper;
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
    private final DownloadManager downloadManager;

    public SearchDataModel(PictureDataProvider provider, DownloadManager downloadManager)
    {
        this.provider = provider;
        this.downloadManager = downloadManager;
    }

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
                    previewImage = downloadManager.downloadImage(data.getPreviewURL());
                }
                catch(IOException e)
                {
                    return;
                }
                PictureTile tile = new PictureTile(data, previewImage);
                Platform.runLater(() -> pictures.add(tile)); //update list on UI thread

                if(Thread.currentThread().isInterrupted())
                    Platform.runLater(() -> pictures.remove(tile)); //update list on UI thread
            });
        }

        ExecutorHelper.shutdownExecutor(downloadPreviewsMultiThreadExecutor, 60); //wait for threads to finish
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
        ExecutorHelper.shutdownExecutor(downloadPreviewsMultiThreadExecutor, 60);
        downloadPreviewsMultiThreadExecutor = Executors.newCachedThreadPool();
    }

    public String getCurrentQuery() { return provider.getCurrentQuery(); }

    public void shutdown(long timeout) throws IllegalThreadStateException
    {
        ExecutorHelper.abortExecutor(downloadPreviewsMultiThreadExecutor, timeout);
    }
}
