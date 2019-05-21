package com.spietras.picgallery.search;

import com.spietras.picgallery.search.models.PictureTile;
import com.spietras.picgallery.search.models.SearchDataModel;
import com.spietras.picgallery.search.models.picdata.pixabayData.PixabayRateLimitException;
import com.spietras.picgallery.search.utils.ExecutorManager;
import javafx.application.Platform;
import javafx.beans.value.ObservableValue;
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

    private final SearchDataModel model;

    private ExecutorService loadChunkSingleThreadExecutor = Executors.newSingleThreadExecutor();

    private volatile String currentQuery = null;

    public SearchController(SearchDataModel model)
    {
        this.model = model;
    }

    @FXML
    public void initialize()
    {
        model.getPictures().addListener(this::onPicturesChanged);
        model.endOfPictures().addListener(this::onPicturesEndedChanged);
        scroll.setVvalue(scroll.getVmax());
        scroll.vvalueProperty().addListener(this::scrollVerticalChanged);
    }

    public void shutdown()
    {
        ExecutorManager.shutdownExecutor(loadChunkSingleThreadExecutor, 1);
        model.clearPictures();
    }

    @FXML
    void onSearchButtonClicked(ActionEvent event) throws InterruptedException
    {
        String inputText = inputTextField.getText();
        searchAction(inputText);
    }

    private void searchAction(String query) throws InterruptedException
    {
        searchButton.setDisable(true); //to prevent spamming

        checkQueryChanged(query); //check if query changed

        loadChunkSingleThreadExecutor.submit(() -> loadPictures(query));
    }

    private synchronized void checkQueryChanged(String newQuery) throws InterruptedException
    {
        if(!newQuery.equalsIgnoreCase(currentQuery))
        {
            currentQuery = newQuery;
            ExecutorManager.abortExecutor(loadChunkSingleThreadExecutor); //abort everything related to previous query
            loadChunkSingleThreadExecutor = Executors.newSingleThreadExecutor();
            model.clearPictures(); //clear all previous pictures
        }
    }

    private void loadPictures(String query)
    {
        int CHUNK_SIZE = 10;

        try
        {
            model.loadPicturesChunk(query, CHUNK_SIZE);
            searchButton.setDisable(false); //unlock button after loading
            Platform.runLater(() -> {
                if(!model.endOfPictures().getValue() &&
                   !contentFilledViewport()) //load pictures until viewport is filled or they ended
                {
                    try
                    {
                        searchAction(query);
                    }
                    catch(InterruptedException e)
                    {
                        e.printStackTrace();
                    }
                }
            });
        }
        catch(PixabayRateLimitException e)
        {
            //TODO: handle exceeding rate limit
        }
        catch(IOException e)
        {
            e.printStackTrace();
        }
    }

    private boolean contentFilledViewport()
    {
        double contentHeight = scroll.getContent().getLayoutBounds().getHeight();
        double viewportHeight = scroll.getViewportBounds().getHeight();

        return contentHeight > viewportHeight;
    }

    private void onPicturesChanged(ListChangeListener.Change<? extends PictureTile> change)
    {
        try
        {
            while(change.next())
            {
                if(change.wasAdded())
                {
                    for(PictureTile p : change.getAddedSubList())
                    { addNewTileToView(p); }
                }
                else if(change.wasRemoved())
                    for(PictureTile p : change.getRemoved())
                    { removeTileFromView(p); }
            }
        }
        catch(Exception e)
        {
            e.printStackTrace();
        }
    }

    private void addNewTileToView(PictureTile p)
    {
        ImageView preview = new ImageView(p.getPreviewImage());
        preview.setFitHeight(80);
        preview.setPreserveRatio(true);
        previewTilePane.getChildren().add(preview);
    }

    private void removeTileFromView(PictureTile p)
    {
        previewTilePane.getChildren().removeIf(x -> ((ImageView) x).getImage() == p.getPreviewImage());
    }

    private void onPicturesEndedChanged(ObservableValue<? extends Boolean> observable, Boolean oldValue,
                                        Boolean newValue)
    {
        if(newValue)
        {
            //TODO
        }
    }

    private void scrollVerticalChanged(ObservableValue<? extends Number> observable, Number oldValue, Number newValue)
    {
        if(newValue.doubleValue() == scroll.getVmax()) //when scrolled to end, load new chunk
        {
            try
            {
                searchAction(currentQuery);
            }
            catch(InterruptedException e)
            {
                e.printStackTrace();
            }
        }
    }
}
