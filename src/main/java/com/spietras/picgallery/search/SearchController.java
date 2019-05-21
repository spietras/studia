package com.spietras.picgallery.search;

import com.spietras.picgallery.search.models.PictureTile;
import com.spietras.picgallery.search.models.SearchDataModel;
import com.spietras.picgallery.search.models.picdata.pixabayData.PixabayRateLimitException;
import com.spietras.picgallery.search.utils.ExecutorManager;
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

public class SearchController
{
    private final SearchDataModel model;
    @FXML
    private Button searchButton;
    @FXML
    private TextField inputTextField;
    @FXML
    private TilePane previewTilePane;
    @FXML
    private ScrollPane scroll;
    private ExecutorService loadChunkSingleThreadExecutor = Executors.newSingleThreadExecutor();

    private volatile String currentQuery = null;

    public SearchController(SearchDataModel model)
    {
        this.model = model;
    }

    @FXML
    public void initialize()
    {
        previewTilePane.setPrefRows(1);
        model.getPictures().addListener(this::onPicturesChanged);
        model.endOfPictures().addListener(
                (observable1, oldValue1, newValue1) -> onPicturesEndedChanged(newValue1));
        scroll.setVvalue(scroll.getVmax());
        scroll.vvalueProperty().addListener(
                (observable, oldValue, newValue) -> scrollVerticalChanged(newValue));
        scroll.layoutBoundsProperty().addListener((observableValue, bounds, t1) -> areaResized());
    }

    public void shutdown()
    {
        ExecutorManager.shutdownExecutor(loadChunkSingleThreadExecutor, 1);
        model.clearPictures();
    }

    @FXML
    void onSearchButtonClicked(ActionEvent event)
    {
        String inputText = inputTextField.getText();
        searchAction(inputText, false);
    }

    private void searchAction(String query, boolean unfilledOnly)
    {
        if(query == null || query.isEmpty())
            return;

        searchButton.setDisable(true); //to prevent spamming

        checkQueryChanged(query); //check if query changed

        loadChunkSingleThreadExecutor.submit(() -> loadPictures(query, unfilledOnly));
    }

    private synchronized void checkQueryChanged(String newQuery)
    {
        if(!newQuery.equalsIgnoreCase(currentQuery))
        {
            currentQuery = newQuery;
            ExecutorManager.abortExecutor(loadChunkSingleThreadExecutor); //abort everything related to previous query
            loadChunkSingleThreadExecutor = Executors.newSingleThreadExecutor();
            model.clearPictures(); //clear all previous pictures
        }
    }

    private void loadPictures(String query, boolean unfilledOnly)
    {
        int CHUNK_SIZE = 10;

        if(unfilledOnly && contentFilledViewport())
            return;

        try
        {
            model.loadPicturesChunk(query, CHUNK_SIZE);
            searchButton.setDisable(false); //unlock button after loading
            Platform.runLater(() -> {
                if(!model.endOfPictures().getValue())
                    searchAction(query, true);
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
        double contentWidth = previewTilePane.getWidth();
        int tilesInRow = (int) (contentWidth / (previewTilePane.getTileWidth() + previewTilePane.getHgap()));
        int rows = (int) Math.ceil((double) previewTilePane.getChildren().size() / tilesInRow);
        double contentHeight = rows * (previewTilePane.getTileHeight() + previewTilePane.getVgap());

        return contentHeight > scroll.getHeight();
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

    private void onPicturesEndedChanged(Boolean newValue)
    {
        if(newValue)
        {
            //TODO
        }
    }

    private void scrollVerticalChanged(Number newValue)
    {
        if(newValue.doubleValue() == scroll.getVmax()) //when scrolled to end, load new chunk
            searchAction(currentQuery, false);
    }

    private void areaResized()
    {
        Platform.runLater(() -> searchAction(currentQuery, true));
    }
}
