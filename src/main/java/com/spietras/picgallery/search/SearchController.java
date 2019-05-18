package com.spietras.picgallery.search;

import com.spietras.picgallery.search.models.SearchDataModel;
import com.spietras.picgallery.search.models.picdata.PictureData;
import javafx.application.Platform;
import javafx.collections.ListChangeListener;
import javafx.event.ActionEvent;
import javafx.fxml.FXML;
import javafx.scene.control.Button;
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

    private SearchDataModel model;

    private ExecutorService singleThreadExecutor = Executors.newSingleThreadExecutor();
    private ExecutorService multiThreadExecutor = Executors.newCachedThreadPool();

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
                        multiThreadExecutor.submit(() -> {
                            ImageView preview = new ImageView(p.getPreviewURL());
                            preview.setFitHeight(80);
                            preview.setPreserveRatio(true);
                            Platform.runLater(() -> previewTilePane.getChildren().add(preview));
                        });
                    }
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
        singleThreadExecutor.shutdown();
        try
        {
            if(!singleThreadExecutor.awaitTermination(800, TimeUnit.MILLISECONDS))
            {
                singleThreadExecutor.shutdownNow();
            }
        }
        catch(InterruptedException e)
        {
            singleThreadExecutor.shutdownNow();
        }

        multiThreadExecutor.shutdown();
        try
        {
            if(!multiThreadExecutor.awaitTermination(800, TimeUnit.MILLISECONDS))
            {
                multiThreadExecutor.shutdownNow();
            }
        }
        catch(InterruptedException e)
        {
            multiThreadExecutor.shutdownNow();
        }
    }

    @FXML
    void onSearchButtonClicked(ActionEvent event)
    {
        singleThreadExecutor.execute(() -> {
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
}
