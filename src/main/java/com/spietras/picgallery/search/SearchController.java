package com.spietras.picgallery.search;

import com.spietras.picgallery.search.models.PictureTile;
import com.spietras.picgallery.search.models.SearchDataModel;
import com.spietras.picgallery.search.models.picdata.pixabayData.PixabayRateLimitException;
import com.spietras.picgallery.search.utils.ExecutorManager;
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

    private SearchDataModel model;

    private ExecutorService loadChunkSingleThreadExecutor = Executors.newSingleThreadExecutor();

    private String currentQuery = null;

    public SearchController(SearchDataModel model)
    {
        this.model = model;
    }

    @FXML
    public void initialize()
    {
        model.getPictures().addListener(this::onPicturesChanged);
        model.endOfPictures().addListener(this::onPicturesEndedChanged);
    }

    public void shutdown()
    {
        ExecutorManager.shutdownExecutor(loadChunkSingleThreadExecutor, 1);
        model.clearPictures();
    }

    @FXML
    synchronized void onSearchButtonClicked(ActionEvent event) throws InterruptedException
    {
        searchButton.setDisable(true);
        String inputText = inputTextField.getText();

        checkQueryChanged(inputText);

        loadChunkSingleThreadExecutor.submit(() -> loadPictures(inputText));
    }

    private void checkQueryChanged(String newQuery) throws InterruptedException
    {
        if(!newQuery.equalsIgnoreCase(currentQuery))
        {
            currentQuery = newQuery;
            ExecutorManager.abortExecutor(loadChunkSingleThreadExecutor);
            loadChunkSingleThreadExecutor = Executors.newSingleThreadExecutor();
            model.clearPictures();
        }
    }

    private void loadPictures(String query)
    {
        int CHUNK_SIZE = 10;

        try
        {
            model.loadPicturesChunk(query, CHUNK_SIZE);
            searchButton.setDisable(false);
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

    private void onPicturesChanged(ListChangeListener.Change<? extends PictureTile> change)
    {
        while(change.next())
        {
            if(change.wasAdded())
            {
                for(PictureTile p : change.getAddedSubList())
                { addNewTileToView(p); }
            }
            else if(change.wasRemoved())
                removeTilesFromView(change.getFrom(), change.getRemovedSize());
        }
    }

    private void addNewTileToView(PictureTile p)
    {
        ImageView preview = new ImageView(p.getPreviewImage());
        preview.setFitHeight(80);
        preview.setPreserveRatio(true);
        previewTilePane.getChildren().add(preview);
    }

    private void removeTilesFromView(int startIndex, int size)
    {
        previewTilePane.getChildren().remove(startIndex, size);
    }

    private void onPicturesEndedChanged(ObservableValue<? extends Boolean> observable, Boolean oldValue,
                                        Boolean newValue)
    {
        if(newValue)
        {
            //TODO
        }
    }
}
