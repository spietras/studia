package com.spietras.picgallery.search;

import com.spietras.picgallery.search.models.SearchDataModel;
import com.spietras.picgallery.search.models.picdata.PictureData;
import javafx.application.Platform;
import javafx.collections.ListChangeListener;
import javafx.event.ActionEvent;
import javafx.fxml.FXML;
import javafx.scene.control.Button;
import javafx.scene.control.ScrollPane;
import javafx.scene.control.TextField;
import javafx.scene.image.ImageView;
import javafx.scene.layout.TilePane;

import java.io.IOException;
import java.util.concurrent.ExecutorService;
import java.util.concurrent.Executors;
import java.util.concurrent.TimeUnit;

public class SearchController
{
    @FXML
    private Button searchButton;

    @FXML
    private TextField inputTextField;

    @FXML
    private TilePane previewTilePane;

    @FXML
    private ScrollPane scroll;

    private SearchDataModel model;

    private ExecutorService loadChunkSingleThreadExecutor = Executors.newSingleThreadExecutor();
    private ExecutorService downloadPreviewMultiThreadExecutor = Executors.newCachedThreadPool();

    public SearchController(SearchDataModel m)
    {
        model = m;
    }

    @FXML
    public void initialize()
    {
        model.getPictures().addListener((ListChangeListener<PictureData>) change -> {
            while(change.next())
            {
                if(change.wasAdded())
                {
                    for(PictureData p : change.getAddedSubList())
                    {
                        downloadPreviewMultiThreadExecutor.submit(() -> {
                            ImageView preview = new ImageView(p.getPreviewURL());
                            preview.setFitHeight(80);
                            preview.setPreserveRatio(true);
                            Platform.runLater(() -> previewTilePane.getChildren().add(preview));
                        });
                    }

                    shutdownExecutor(downloadPreviewMultiThreadExecutor, 60);
                    downloadPreviewMultiThreadExecutor = Executors.newCachedThreadPool();
                }
                else if(change.wasRemoved())
                {
                    Platform.runLater(
                            () -> previewTilePane.getChildren().remove(change.getFrom(), change.getRemovedSize()));
                }
            }
        });

        model.endOfPictures().addListener((observable, oldValue, newValue) -> {
            if(newValue)
            {
                //TODO
            }
        });
    }

    public void shutdown()
    {
        shutdownExecutor(loadChunkSingleThreadExecutor, 1);
        shutdownExecutor(downloadPreviewMultiThreadExecutor, 1);
    }

    @FXML
    void onSearchButtonClicked(ActionEvent event)
    {
        loadChunkSingleThreadExecutor.execute(() -> {
            try
            {
                model.loadPicturesChunk(inputTextField.getText());
            }
            catch(IOException e)
            {
                e.printStackTrace();
            }
        });
    }

    private void shutdownExecutor(ExecutorService serivce, long timeout)
    {
        serivce.shutdown();
        try
        {
            if(!serivce.awaitTermination(timeout, TimeUnit.SECONDS))
            {
                serivce.shutdownNow();
            }
        }
        catch(InterruptedException e)
        {
            serivce.shutdownNow();
        }
    }
}
