package com.spietras.picgallery.search;

import com.spietras.picgallery.models.picdata.PictureData;
import com.spietras.picgallery.models.picdata.pixabayData.PixabayRateLimitException;
import com.spietras.picgallery.picdetails.PicDetailsController;
import com.spietras.picgallery.picdetails.models.PicDetailsModel;
import com.spietras.picgallery.search.models.PictureTile;
import com.spietras.picgallery.search.models.SearchDataModel;
import com.spietras.picgallery.utils.ConnectionManager;
import com.spietras.picgallery.utils.DownloadManager;
import com.spietras.picgallery.utils.ExecutorHelper;
import com.spietras.picgallery.utils.FXHelper;
import javafx.application.Platform;
import javafx.beans.value.ObservableValue;
import javafx.collections.ListChangeListener;
import javafx.event.ActionEvent;
import javafx.fxml.FXML;
import javafx.fxml.FXMLLoader;
import javafx.geometry.Rectangle2D;
import javafx.scene.Parent;
import javafx.scene.control.Button;
import javafx.scene.control.ScrollPane;
import javafx.scene.control.TextField;
import javafx.scene.image.Image;
import javafx.scene.image.ImageView;
import javafx.scene.layout.TilePane;
import javafx.stage.Stage;

import java.io.IOException;
import java.util.concurrent.ExecutionException;
import java.util.concurrent.ExecutorService;

public class SearchController
{
    private final Stage stage;
    private final SearchDataModel model;
    @FXML
    private Button searchButton;
    @FXML
    private TextField inputTextField;
    @FXML
    private TilePane previewTilePane;
    @FXML
    private ScrollPane scroll;
    private ExecutorService loadChunkSingleThreadExecutor = ExecutorHelper.newRejectingSingleThreadExecutor();
    private ExecutorService scrollSingleExecutor = ExecutorHelper.newRejectingSingleThreadExecutor();
    private ExecutorService resizeSingleExecutor = ExecutorHelper.newRejectingSingleThreadExecutor();

    private volatile String currentQuery = null;

    private PicDetailsController currentDetailsController = null;
    private Parent searchParent = null;

    public SearchController(Stage stage, SearchDataModel model)
    {
        this.stage = stage;
        this.model = model;
    }

    @FXML
    public void initialize()
    {
        model.getPictures().addListener(this::onPicturesChanged);
        model.endOfPictures().addListener(
                (observable1, oldValue1, newValue1) -> onPicturesEndedChanged(newValue1));
        scroll.vvalueProperty().addListener(
                (observable, oldValue, newValue) -> scrollVerticalChanged(newValue));
        scroll.layoutBoundsProperty().addListener((observableValue, bounds, t1) -> areaResized());
        inputTextField.textProperty().addListener(this::inputTextChanged);
        searchButton.setDisable(true);
    }

    /**
     * Fetches Parent from scene (because parent is set after scene is passed to constructor)
     */
    public void fetchParent()
    {
        searchParent = stage.getScene().getRoot();
    }

    /**
     * Shuts down and cleans everything (particularly background threads)
     */
    public void shutdown()
    {
        long timeout = 10;

        try
        {
            if(currentDetailsController != null)
                currentDetailsController.shutdown(timeout);
            model.shutdown(timeout);
        }
        catch(IllegalThreadStateException ignored) { }
        abortExecutors(timeout);
    }

    /**
     * Aborts all executors
     *
     * @param timeout how many seconds to wait on each executor to shut down
     */
    private void abortExecutors(long timeout)
    {
        try
        {
            ExecutorHelper.abortExecutor(loadChunkSingleThreadExecutor, timeout);
        }
        catch(IllegalThreadStateException ignored) { }
        try
        {
            ExecutorHelper.abortExecutor(scrollSingleExecutor, timeout);
        }
        catch(IllegalThreadStateException ignored) { }
        try
        {
            ExecutorHelper.abortExecutor(resizeSingleExecutor, timeout);
        }
        catch(IllegalThreadStateException ignored) { }
    }

    @FXML
    void onSearchButtonClicked(ActionEvent event)
    {
        String inputText = inputTextField.getText();
        searchAction(inputText);
    }

    private void searchAction(String query)
    {
        if(query == null || query.isEmpty())
            return;

        searchButton.setDisable(true); //to prevent spamming
        inputTextField.setEditable(false);

        checkQueryChanged(query); //check if query changed

        loadChunkSingleThreadExecutor.submit(() -> loadPictures(query));
    }

    /**
     * Checks if query is different from previous one and if it is cleans everything up, so we have fresh start for new query
     */
    private synchronized void checkQueryChanged(String newQuery)
    {
        long timeout = 1;

        if(!newQuery.equalsIgnoreCase(currentQuery))
        {
            currentQuery = newQuery;
            abortExecutors(timeout);
            loadChunkSingleThreadExecutor = ExecutorHelper.newRejectingSingleThreadExecutor();
            scrollSingleExecutor = ExecutorHelper.newRejectingSingleThreadExecutor();
            resizeSingleExecutor = ExecutorHelper.newRejectingSingleThreadExecutor();
            Platform.runLater(model::clearPictures); //clear all previous pictures
        }
    }

    /**
     * Loads pictures until they filled viewport and we don't scroll to the end
     */
    private void loadPictures(String query)
    {
        int CHUNK_SIZE = 10;

        try
        {
            model.loadPicturesChunk(query, CHUNK_SIZE);
            if(Thread.currentThread().isInterrupted())
                return;

            if(!FXHelper.runOnUIAndWait(() -> model.endOfPictures().getValue()))
            {
                if(isScrollOnEnd() || !contentFilledViewport())
                    loadPictures(query);
            }
        }
        catch(PixabayRateLimitException e)
        {
            handleRateLimitException(e);
        }
        catch(IOException e)
        {
            handleLoadException(e);
        }
        catch(InterruptedException | ExecutionException e)
        {
            Thread.currentThread().interrupt();
        }
        finally
        {
            try
            {
                if(FXHelper.runOnUIAndWait(() -> model.endOfPictures().getValue()) &&
                   inputTextField.getText().equals(currentQuery))
                    searchButton.setDisable(true);
                else
                    searchButton.setDisable(false);

                inputTextField.setEditable(true);
            }
            catch(ExecutionException | InterruptedException ignored) { }
        }
    }

    private void handleRateLimitException(PixabayRateLimitException e)
    {
        Platform.runLater(() -> FXHelper.showErrorDialog("Error",
                                                         "Can't load pictures.\n" +
                                                         "Rate limit exceeded for query: " + e.getQuery() +
                                                         ", limit: " + e.getLimit() + "\n" +
                                                         "Reset time: " + e.getReset() + " seconds"));
    }

    private void handleLoadException(IOException e)
    {
        Platform.runLater(
                () -> FXHelper.showErrorNotification("Error", "Can't load pictures.\n" + e.getMessage(), 5000));
    }

    /**
     * Checks if scroll position is near the end
     */
    private boolean isScrollOnEnd() throws ExecutionException, InterruptedException
    {
        double threshold = 0.8;

        return FXHelper.runOnUIAndWait(() -> scroll.getVvalue() >= threshold * scroll.getVmax());
    }

    /**
     * Checks if content filled viewport so there are no empty spaces
     */
    private boolean contentFilledViewport() throws ExecutionException, InterruptedException
    {
        return FXHelper.runOnUIAndWait(() -> {
            double contentWidth = previewTilePane.getWidth();
            int tilesInRow = (int) (contentWidth / (previewTilePane.getTileWidth() + previewTilePane.getHgap()));
            int rows = (int) Math.ceil((double) previewTilePane.getChildren().size() / tilesInRow);
            double singleRowSize = previewTilePane.getTileHeight() + previewTilePane.getVgap();
            double contentHeight = rows * singleRowSize;

            return contentHeight > scroll.getHeight() + singleRowSize;
        });
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
        double tileHeight = 80.0;
        previewTilePane.getChildren().add(createTilePreview(p, tileHeight));
    }

    private ImageView createTilePreview(PictureTile p, double tileHeight)
    {
        ImageView preview = new ImageView(p.getPreviewImage());
        preview.setFitHeight(tileHeight);
        preview.setPreserveRatio(true);
        preview.setViewport(createTileViewport(preview.getImage()));
        preview.setOnMouseClicked(e -> showDetails(p.getData()));
        return preview;
    }

    private Rectangle2D createTileViewport(Image previewImage)
    {
        double smallerDimension = Math.min(previewImage.getWidth(), previewImage.getHeight());
        double viewportStartX = (previewImage.getWidth() - smallerDimension) / 2;
        double viewportStartY = (previewImage.getHeight() - smallerDimension) / 2;
        return new Rectangle2D(viewportStartX, viewportStartY, smallerDimension, smallerDimension);
    }

    private void removeTileFromView(PictureTile p)
    {
        previewTilePane.getChildren().removeIf(x -> ((ImageView) x).getImage() == p.getPreviewImage());
    }

    private void onPicturesEndedChanged(Boolean newValue)
    {
        if(newValue)
            FXHelper.showInfoNotification("", "No more pictures", 2000);
    }

    private void scrollVerticalChanged(Number newValue)
    {
        scrollSingleExecutor.submit(() -> {
            try
            {
                if(!isScrollOnEnd())
                    return;
            }
            catch(ExecutionException | InterruptedException e) { return; }

            searchAction(currentQuery);
            try
            {
                Thread.sleep(500);
            }
            catch(InterruptedException ignored) { }
        });
    }

    private void areaResized()
    {
        resizeSingleExecutor.submit(() -> {
            try
            {
                if(currentQuery == null || contentFilledViewport())
                    return;

                if(Thread.currentThread().isInterrupted())
                    return;

                searchAction(currentQuery);
            }
            catch(InterruptedException | ExecutionException ignored) { }
            finally
            {
                try
                {
                    Thread.sleep(500);
                }
                catch(InterruptedException ignored) { }
            }
        });
    }

    private void inputTextChanged(ObservableValue<? extends String> observable, String oldValue, String newValue)
    {
        if(newValue == null || newValue.isEmpty())
            searchButton.setDisable(true);
        else if(model.endOfPictures().getValue() && newValue.equals(currentQuery))
            searchButton.setDisable(true);
        else
            searchButton.setDisable(false);
    }

    /**
     * Switches scene to show details of chosen picture
     */
    private void showDetails(PictureData data)
    {
        DownloadManager dm = new DownloadManager(new ConnectionManager(5000));
        PicDetailsModel model = new PicDetailsModel(data, dm);

        FXMLLoader loader = new FXMLLoader(getClass().getResource("/views/picdetails.fxml"));
        PicDetailsController controller = new PicDetailsController(this, model);
        loader.setController(controller);
        Parent root;
        try
        {
            root = loader.load(); //load details scene
        }
        catch(IOException e)
        {
            e.printStackTrace();
            return;
        }

        stage.getScene().setRoot(root); //show details scene
        currentDetailsController = controller;
    }

    /**
     * Switches back to search scene
     */
    public void resumeScene() throws IllegalStateException
    {
        long timeout = 1;

        if(searchParent == null)
            throw new IllegalStateException("Can't resume scene when parent is not set");

        stage.getScene().setRoot(searchParent);
        try
        {
            currentDetailsController.shutdown(timeout); //clean everything in previous scene
        }
        catch(IllegalThreadStateException ignored) { }
        currentDetailsController = null;
    }
}
