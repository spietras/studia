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
import java.util.concurrent.FutureTask;

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
        scroll.setVvalue(scroll.getVmax());
        scroll.vvalueProperty().addListener(
                (observable, oldValue, newValue) -> scrollVerticalChanged(newValue));
        scroll.layoutBoundsProperty().addListener((observableValue, bounds, t1) -> areaResized());
    }

    public void fetchParent()
    {
        searchParent = stage.getScene().getRoot();
    }

    public void shutdown()
    {
        long timeout = 60;

        if(currentDetailsController != null)
            currentDetailsController.shutdown(timeout);
        model.shutdown(timeout);
        ExecutorHelper.abortExecutor(loadChunkSingleThreadExecutor, timeout);
        ExecutorHelper.abortExecutor(scrollSingleExecutor, timeout);
        ExecutorHelper.abortExecutor(resizeSingleExecutor, timeout);
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

        checkQueryChanged(query); //check if query changed

        loadChunkSingleThreadExecutor.submit(() -> loadPictures(query));
    }

    private synchronized void checkQueryChanged(String newQuery)
    {
        long timeout = 1;

        if(!newQuery.equalsIgnoreCase(currentQuery))
        {
            currentQuery = newQuery;
            try
            {
                ExecutorHelper.abortExecutor(loadChunkSingleThreadExecutor,
                                             timeout); //abort everything related to previous query
            }
            catch(IllegalThreadStateException e) { e.printStackTrace(); }
            try
            {
                ExecutorHelper.abortExecutor(scrollSingleExecutor,
                                             timeout); //abort everything related to previous query
            }
            catch(IllegalThreadStateException e) { e.printStackTrace(); }
            try
            {
                ExecutorHelper.abortExecutor(resizeSingleExecutor,
                                             timeout); //abort everything related to previous query
            }
            catch(IllegalThreadStateException e) { e.printStackTrace(); }

            loadChunkSingleThreadExecutor = ExecutorHelper.newRejectingSingleThreadExecutor();
            scrollSingleExecutor = ExecutorHelper.newRejectingSingleThreadExecutor();
            resizeSingleExecutor = ExecutorHelper.newRejectingSingleThreadExecutor();
            model.clearPictures(); //clear all previous pictures
        }
    }

    private void loadPictures(String query)
    {
        int CHUNK_SIZE = 10;

        try
        {
            model.loadPicturesChunk(query, CHUNK_SIZE);

            final FutureTask<Boolean> getEndOfPicturesTask = new FutureTask<>(() -> model.endOfPictures().getValue());
            Platform.runLater(getEndOfPicturesTask); //get endOfPictures value on UI thread
            if(!getEndOfPicturesTask.get() && !contentFilledViewport())
                loadPictures(query);
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
            e.printStackTrace();
        }
        finally
        {
            searchButton.setDisable(false); //unlock button after loading
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

    private boolean contentFilledViewport() throws ExecutionException, InterruptedException
    {
        final FutureTask<Boolean> getContentFilled = new FutureTask<>(() -> {
            double contentWidth = previewTilePane.getWidth();
            int tilesInRow = (int) (contentWidth / (previewTilePane.getTileWidth() + previewTilePane.getHgap()));
            int rows = (int) Math.ceil((double) previewTilePane.getChildren().size() / tilesInRow);
            double singleRowSize = previewTilePane.getTileHeight() + previewTilePane.getVgap();
            double contentHeight = rows * singleRowSize;

            return contentHeight > scroll.getHeight() + singleRowSize;
        });
        Platform.runLater(getContentFilled);
        return getContentFilled.get();
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
        double scrollThreshold = 0.75;

        if(newValue.doubleValue() >= scrollThreshold * scroll.getVmax()) //when scrolled to end, load new chunk
        {
            scrollSingleExecutor.submit(() -> {
                searchAction(currentQuery);
                try
                {
                    Thread.sleep(500);
                }
                catch(InterruptedException ignored) { }
            });
        }
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
            catch(ExecutionException | InterruptedException e) { e.printStackTrace(); }
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
